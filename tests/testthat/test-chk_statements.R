skip_on_cran()

# Front-end robustness on real-world TAB spellings (6.4 corpus
# re-survey findings B1-B5): every accepted GEMPACK spelling must
# normalise to the same parsed model as the canonical spelling, and
# malformed text must abort with a named message, never a parser crash.

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "statements_test")
if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}
dir.create(write_dir, recursive = TRUE)
ems_option_set(verbose = FALSE, tempdir = write_dir)
withr::defer(ems_option_reset(), teardown_env())

model_files <- ems_example("GTAPv7", write_dir)
model_file <- model_files[["model_file"]]
base_txt <- readChar(model_file, file.info(model_file)$size)

write_tab <- function(text, name = "mut.tab", ...) {
  path <- file.path(write_dir, name)
  writeChar(text, path, eos = NULL, ...)
  path
}

process <- function(path) {
  .process_tablo(tab_file = path, quiet = TRUE, call = NULL)
}

base_model <- process(model_file)
attr(base_model, "tab_file") <- NULL

expect_same_model <- function(path) {
  model <- process(path)
  attr(model, "tab_file") <- NULL
  expect_identical(model, base_model)
}

test_that("keywords glued to their first parenthesis parse (B1)", {
  txt <- gsub("Coefficient (", "Coefficient(", base_txt, fixed = TRUE)
  txt <- gsub("Formula (", "Formula(", txt, fixed = TRUE)
  txt <- gsub("Variable (", "Variable(", txt, fixed = TRUE)
  txt <- gsub("Equation (", "Equation(", txt, fixed = TRUE)
  txt <- gsub("Update (", "Update(", txt, fixed = TRUE)
  expect_true(grepl("Coefficient(all", txt, fixed = TRUE))
  expect_same_model(write_tab(txt))
})

test_that("![[! !]]! block comments with nested ! are stripped (B2)", {
  txt <- paste0(
    "![[! block comment ! with a nested bang ! and\n",
    "Coefficient (all,r,REG) BOGUS(r);\n",
    "across lines !]]!\n",
    base_txt,
    "\n![[! trailing !]]!\n"
  )
  expect_same_model(write_tab(txt))
})

test_that("empty statements are no-ops (B3)", {
  txt <- sub(";", ";;", base_txt, fixed = TRUE)
  txt <- paste0(txt, "\n;\n  ;\n")
  expect_same_model(write_tab(txt))
})

test_that("Latin-1 and CRLF files parse (B4)", {
  txt <- gsub("\n", "\r\n", base_txt, fixed = TRUE)
  path <- file.path(write_dir, "latin1.tab")
  con <- file(path, "wb")
  writeBin(
    c(
      charToRaw("! caf"), as.raw(c(0xE9, 0x20, 0xE0)),
      charToRaw(" la GEMPACK !\r\n"), charToRaw(txt)
    ),
    con
  )
  close(con)
  expect_same_model(path)
})

test_that("a UTF-8 BOM is dropped (B4)", {
  path <- file.path(write_dir, "bom.tab")
  con <- file(path, "wb")
  writeBin(as.raw(c(0xEF, 0xBB, 0xBF)), con)
  writeChar(base_txt, con, eos = NULL)
  close(con)
  expect_same_model(path)
})

test_that("label text does not reach the qualifier scans (B5)", {
  txt <- paste0(
    base_txt,
    "\nCoefficient (all,r,REG) LBLX(r) # (default for zerodivide default) #;\n",
    "Formula (initial) (all,r,REG) LBLX(r) = 1;\n"
  )
  model <- process(write_tab(txt))
  expect_true("LBLX" %in% model$name)
})

test_that("stray label text outside a statement aborts", {
  txt <- sub(
    "Coefficient (all,r,REG)",
    "# stray label # Coefficient (all,r,REG)",
    base_txt,
    fixed = TRUE
  )
  expect_snapshot_error(process(write_tab(txt)))
})

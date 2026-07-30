skip_on_cran()

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "preflight_test")
if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}
dir.create(write_dir, recursive = TRUE)
ems_option_set(verbose = FALSE, tempdir = write_dir)
withr::defer(ems_option_reset(), teardown_env())

model_files <- ems_example("GTAPv7", write_dir)
model_file <- model_files[["model_file"]]
base_txt <- readChar(model_file, file.info(model_file)$size)

mutate_tab <- function(text, name = "mut.tab") {
  path <- file.path(write_dir, name)
  writeChar(paste0(base_txt, "\n", text, "\n"), path, eos = NULL)
  path
}

expect_preflight_error <- function(text) {
  expect_snapshot_error(
    .process_tablo(tab_file = mutate_tab(text), quiet = TRUE, call = NULL)
  )
}

# names (GEMPACK manual 11.2.1; solver names_validate)

test_that("name collisions abort", {
  expect_preflight_error("Coefficient (all,r,REG) PSAVE(r);")
  expect_preflight_error("Coefficient REG;")
  expect_preflight_error("Variable (all,r,REG) reg(r);")
})

test_that("duplicate declarations abort", {
  expect_preflight_error("Coefficient DUPX;\nCoefficient DUPX;")
})

test_that("reserved words abort", {
  expect_preflight_error("Coefficient (all,r,REG) MAX(r);")
})

test_that("c_ prefixed coefficients abort", {
  expect_preflight_error("Coefficient c_foo;")
})

test_that("p_/c_ variable-pair clashes abort", {
  # qgdp is a declared GTAPv7 variable: p_qgdp cannot coexist (the
  # reference token p_qgdp is ambiguous)
  expect_preflight_error("Variable (all,r,REG) p_qgdp(r);")
})

test_that("the hand-linearized pair idiom parses", {
  # VKB is a declared GTAPv7 coefficient: coefficient X + variable
  # p_X is the supported pair (solver section-6 naming resolution)
  model <- .process_tablo(
    tab_file = mutate_tab(
      "Variable (all,r,REG) p_VKB(r) # pair of coefficient VKB #;"
    ),
    quiet = TRUE,
    call = NULL
  )
  expect_true("p_VKB" %in% model$name[model$type == "Variable"])
})

test_that("over-length names abort", {
  expect_preflight_error(
    paste0("Coefficient ", strrep("A", 260), ";")
  )
})

# qualifiers (GEMPACK manual 10.3/10.4; solver tab_qualifiers_parse)

test_that("unknown qualifiers abort", {
  expect_preflight_error("Variable (foo) dummyvar;")
})

test_that("no_split and linear_name qualifiers abort", {
  expect_preflight_error("Variable (no_split) dummyvar;")
  expect_preflight_error("Variable (levels, linear_name=xlin) dummyvar;")
})

test_that("empty qualifiers abort", {
  expect_preflight_error("Variable () dummyvar;")
})

test_that("duplicate bounds abort", {
  expect_preflight_error(
    "Coefficient (ge 0, ge 1) (all,r,REG) BNDP(r);"
  )
})

# Default statements (GEMPACK manual 10.19; solver tab_defaults_validate)

test_that("invalid Default statements abort", {
  expect_preflight_error("Equation (default=levels);")
  expect_preflight_error("Coefficient (default=lower_bound ge 0);")
  expect_preflight_error("Variable (default=foo);")
  expect_preflight_error("Update (default=always);")
})

test_that("solver-valid Default statements abort as unsupported", {
  expect_preflight_error("Variable (default=change);")
})

# sets (GEMPACK manual 10.1.1.1 / 10.1.2.1; solver set readers)

test_that("self-referential set expressions abort", {
  expect_preflight_error("Set SBAD = SBAD + COMM;")
})

test_that("undeclared set references abort", {
  # in a set expression
  expect_preflight_error("Set SND = COMM - MRGX;")
  # as a set-equality right-hand side
  expect_preflight_error("Set SEQ = NOPE;")
  # in a Subset statement
  expect_preflight_error("Subset REG is subset of NOPE2;")
})

test_that("set self-equality aborts", {
  expect_preflight_error("Set SSE = SSE;")
})

test_that("element range abbreviations abort", {
  expect_preflight_error("Set SRG (s1 - s5);")
})

test_that("malformed element lists abort", {
  expect_preflight_error("Set SEL ();")
  expect_preflight_error("Set SEL2 (x1,);")
})

test_that("over-length set headers abort", {
  expect_preflight_error(
    "Set SHD read elements from file GTAPSETS header \"TOOBIG\";"
  )
})

# unsupported statement forms

test_that("math statements without = abort", {
  # a plainly malformed Formula
  expect_preflight_error("Formula NOEQ 1;")
  # an unrecognized keyword is folded into the preceding statement as
  # an implicit continuation; it surfaces as a math statement missing =
  # (used to crash the extract parsers with a raw purrr error)
  expect_preflight_error("Frobnicate all the things;")
})

# Formula & Equation is supported since C0 (split into its 10.9.1
# halves by .check_statements) -- see test-tab_levels.R

# reads (GEMPACK manual 10.6/11.11.8)

test_that("headerless reads abort", {
  expect_preflight_error(
    "Coefficient (all,r,REG) ELX(r);\nRead ELX from file GTAPDATA;"
  )
})

test_that("reads into undeclared names abort", {
  expect_preflight_error(
    'Read NOTDECL from file GTAPDATA header "XXXX";'
  )
})

test_that("read from terminal aborts", {
  expect_snapshot_error(
    .chk_raw_reads("Read ELX from terminal", call = NULL)
  )
})

# PostSim rules (GEMPACK manual 12.2.1-12.2.3)

ps_wrap <- function(...) {
  paste(
    "PostSim (Begin);",
    ...,
    "PostSim (End);",
    sep = "\n"
  )
}

test_that("PostSim reads into PostSim coefficients pass", {
  txt <- ps_wrap(
    "Coefficient PSREADC # ps target #;",
    "File PSDATA;",
    'Read PSREADC from file PSDATA header "PSRD";'
  )
  expect_no_error(
    .process_tablo(tab_file = mutate_tab(txt), quiet = TRUE, call = NULL)
  )
})

test_that("PostSim scope violations abort", {
  txt <- paste0(
    ps_wrap(
      "Coefficient PSCALC # ps #;",
      "Formula PSCALC = sum(r,REG, VKB(r));"
    ),
    "\nCoefficient ORDX # ord #;\nFormula ORDX = PSCALC + 1;"
  )
  expect_preflight_error(txt)
})

test_that("PostSim reads from ordinary files abort", {
  txt <- ps_wrap(
    "Coefficient PSREADC # ps target #;",
    'Read PSREADC from file GTAPDATA header "SAVE";'
  )
  expect_preflight_error(txt)
})

test_that("PostSim reads into ordinary coefficients abort", {
  txt <- ps_wrap(
    "File PSDATA;",
    'Read SAVE from file PSDATA header "PSRD";'
  )
  expect_preflight_error(txt)
})

test_that("PostSim reads into variables abort", {
  txt <- ps_wrap(
    "File PSDATA;",
    'Read psave from file PSDATA header "PSRD";'
  )
  expect_preflight_error(txt)
})

test_that("PostSim formulas assigning variables abort", {
  txt <- ps_wrap(
    "Formula (all,r,REG) psave(r) = 1;"
  )
  expect_preflight_error(txt)
})

test_that("PostSim formulas assigning ordinary coefficients abort", {
  txt <- ps_wrap(
    "Formula (all,r,REG) SAVE(r) = 1;"
  )
  expect_preflight_error(txt)
})

# regression: the shipped models pass the pre-flight unchanged

test_that("internal models pass the pre-flight", {
  tabs <- c(
    system.file("models/GTAPv7/GTAPv7.tab", package = "teems"),
    system.file("models/GTAP-RE/GTAP-RE.tab", package = "teems")
  )
  for (tab in tabs[nzchar(tabs)]) {
    expect_no_error(
      .process_tablo(tab_file = tab, quiet = TRUE, call = NULL)
    )
  }
})

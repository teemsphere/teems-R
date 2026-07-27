skip_on_cran()

# Curated reduced fuzz corpus (dev/validation_table.md Phase 3c(b)):
# every fixture must abort with a named teems message at
# .process_tablo() -- before any solver call -- never with a raw
# parser error (the pre-fix failure mode was a purrr indexed error out
# of the extract parsers). Fixture provenance in fixtures/tab/README.md.

corpus_dir <- test_path("fixtures", "tab")
fixtures <- sort(list.files(corpus_dir, pattern = "\\.tab$"))

corpus_error <- function(fixture) {
  tryCatch(
    {
      .process_tablo(
        tab_file = file.path(corpus_dir, fixture),
        quiet = TRUE,
        call = NULL
      )
      NULL
    },
    error = function(e) e
  )
}

test_that("the corpus is present", {
  expect_gte(length(fixtures), 20L)
})

test_that("every corpus fixture aborts, and never with a parser crash", {
  for (f in fixtures) {
    err <- corpus_error(f)
    expect_false(is.null(err), label = paste0(f, " aborts"))
    expect_false(
      inherits(err, "purrr_error_indexed"),
      label = paste0(f, " aborts with a named message (not a parser crash)")
    )
  }
})

test_that("corpus fixtures abort with their named messages", {
  msgs <- vapply(
    fixtures,
    function(f) {
      err <- corpus_error(f)
      if (is.null(err)) {
        return("NO ERROR")
      }
      msg <- cli::ansi_strip(conditionMessage(err))
      gsub("\\s+", " ", msg)
    },
    character(1)
  )
  expect_snapshot(
    writeLines(paste0(fixtures, ": ", msgs))
  )
})

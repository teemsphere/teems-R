skip_on_cran()

test_that("set difference removes aggregated elements entirely", {
  # two origins map to the aggregated margin commodity; subtracting the
  # margin set must drop the element itself, not just the shared rows
  # (GEMPACK manual 10.1.1.1)
  mappings <- list(
    COMM = data.table::data.table(
      origin = c("o1", "o2", "o3", "o4"),
      mapping = c("crops", "mnfcs", "svces", "svces"),
      key = c("origin", "mapping")
    ),
    MARG = data.table::data.table(
      origin = "o3",
      mapping = "svces",
      key = c("origin", "mapping")
    )
  )
  out <- .eval_set_expr(
    d = "= COMM - MARG",
    mappings = mappings,
    owner = "NMRG",
    call = NULL
  )
  expect_identical(unique(out$mapping), c("crops", "mnfcs"))
})

test_that("set difference rejects elements absent from the minuend", {
  mappings <- list(
    A = data.table::data.table(
      origin = "o1", mapping = "x",
      key = c("origin", "mapping")
    ),
    B = data.table::data.table(
      origin = "o2", mapping = "y",
      key = c("origin", "mapping")
    )
  )
  expect_error(
    .eval_set_expr(d = "= A - B", mappings = mappings, owner = "C", call = NULL),
    "may only remove elements that are present"
  )
})

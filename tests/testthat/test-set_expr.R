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

test_that("set union rejects element-level overlap with disjoint origins", {
  # the same aggregated element carried by different origin rows used
  # to slip past the row-level disjointness test
  mappings <- list(
    A = data.table::data.table(
      origin = "o1", mapping = "x",
      key = c("origin", "mapping")
    ),
    B = data.table::data.table(
      origin = "o2", mapping = "x",
      key = c("origin", "mapping")
    )
  )
  expect_error(
    .eval_set_expr(d = "= A + B", mappings = mappings, owner = "C", call = NULL),
    "requires disjoint sets"
  )
})

test_that("set intersection is element-level with agreeing origins", {
  mappings <- list(
    A = data.table::data.table(
      origin = c("o1", "o2"), mapping = c("x", "y"),
      key = c("origin", "mapping")
    ),
    B = data.table::data.table(
      origin = c("o1", "o3"), mapping = c("x", "z"),
      key = c("origin", "mapping")
    )
  )
  out <- .eval_set_expr(
    d = "= A & B",
    mappings = mappings,
    owner = "C",
    call = NULL
  )
  expect_identical(unique(out$mapping), "x")
  expect_identical(out$origin, "o1")
})

test_that("set intersection aborts on disagreeing origin coverage", {
  # both operands contain element x, but via different origins: the
  # old row-level fintersect silently DROPPED x; ambiguous under
  # aggregation, so it now aborts naming the element
  mappings <- list(
    A = data.table::data.table(
      origin = "o1", mapping = "x",
      key = c("origin", "mapping")
    ),
    B = data.table::data.table(
      origin = "o2", mapping = "x",
      key = c("origin", "mapping")
    )
  )
  expect_snapshot_error(
    .eval_set_expr(d = "= A & B", mappings = mappings, owner = "C", call = NULL)
  )
})

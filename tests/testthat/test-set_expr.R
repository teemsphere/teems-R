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

test_that("set intersection keeps shared elements and stamps disagreeing origins", {
  # both operands contain element x, but via different origins: the
  # old row-level fintersect silently DROPPED x, and an intermediate
  # revision aborted. Element-level semantics (manual 10.1.1) keep x
  # with the accumulator's rows; the disagreement is recorded as the
  # origin_conflict stamp for consumers that read origin rows
  # (.finalize_map_data)
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
  out <- .eval_set_expr(d = "= A & B", mappings = mappings, owner = "C", call = NULL)
  expect_identical(unique(out$mapping), "x")
  expect_identical(out$origin, "o1")
  expect_identical(attr(out, "origin_conflict"), "x")
})

test_that("the origin_conflict stamp propagates through derived sets", {
  mappings <- list(
    A = data.table::data.table(
      origin = "o1", mapping = "x",
      key = c("origin", "mapping")
    ),
    B = data.table::data.table(
      origin = "o2", mapping = "x",
      key = c("origin", "mapping")
    ),
    E = data.table::data.table(
      origin = "o9", mapping = "y",
      key = c("origin", "mapping")
    )
  )
  tainted <- .eval_set_expr(d = "= A & B", mappings = mappings, owner = "C", call = NULL)
  mappings$C <- tainted
  # the conflicted element survives into the union: stamp carries
  derived <- .eval_set_expr(d = "= C + E", mappings = mappings, owner = "D", call = NULL)
  expect_identical(attr(derived, "origin_conflict"), "x")
  # the conflicted element is subtracted away: stamp trimmed off
  cleaned <- .eval_set_expr(d = "= C - B", mappings = mappings, owner = "F", call = NULL)
  expect_null(attr(cleaned, "origin_conflict"))
})

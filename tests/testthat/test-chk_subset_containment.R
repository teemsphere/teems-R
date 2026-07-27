skip_on_cran()

# S11 (dev/validation_table.md): explicit Subset claims are checked
# against the realized (post-aggregation) elements at deploy

test_that("subset containment violations abort", {
  sets <- tibble::tibble(
    name = c("COMM", "MARG"),
    subsets = list("MARG", NA),
    ele = list(c("c1", "c2"), c("c1", "mx"))
  )
  expect_snapshot_error(
    .check_subset_containment(sets = sets, call = NULL)
  )
})

test_that("contained subsets pass", {
  sets <- tibble::tibble(
    name = c("COMM", "MARG"),
    subsets = list("MARG", NA),
    ele = list(c("c1", "c2", "c3"), "c3")
  )
  expect_silent(.check_subset_containment(sets = sets, call = NULL))
})

test_that("intertemporal numeric elements are checked", {
  sets <- tibble::tibble(
    name = c("ALLTIME", "FTRTIME"),
    subsets = list("FTRTIME", NA),
    ele = list(0:4, 1:5)
  )
  expect_snapshot_error(
    .check_subset_containment(sets = sets, call = NULL)
  )
})

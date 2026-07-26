skip_on_cran()

test_that(".nearest_names ranks by edit distance and caps", {
  expect_identical(
    .nearest_names("qgdpp", c("qgdp", "pop", "psave")),
    "qgdp"
  )
  expect_identical(
    .nearest_names("zzzzzzzz", c("qgdp", "pop")),
    character(0)
  )
})

test_that("unknown closure variables suggest candidates", {
  var_extract <- tibble::tibble(
    name = c("qgdp", "pop", "psave"),
    condense = NA_character_
  )
  expect_snapshot_error(
    .check_closure(c("qgdpp", "pop"), var_extract, call = NULL)
  )
})

test_that("unknown closure variables without a near match omit candidates", {
  var_extract <- tibble::tibble(
    name = c("qgdp", "pop", "psave"),
    condense = NA_character_
  )
  expect_snapshot_error(
    .check_closure("zzzzzzzz", var_extract, call = NULL)
  )
})

square_fixture <- function(exo_rows) {
  sets <- list(ele = list(REG = c("a", "b", "c")))
  model <- tibble::tibble(
    type = "Equation",
    name = "E_x",
    tab = "Equation E_x (all,r,REG) x(r) = y(r);",
    condense_eq = NA_character_
  )
  var_extract <- tibble::tibble(
    name = c("x", "y"),
    ls_upper_idx = list(x = "REG", y = "REG")
  )
  ele <- data.table::CJ(REGr = c("a", "b", "c")[seq_len(exo_rows)])
  entry <- structure("y", var_name = "y", ele = ele)
  attr(entry, "var_name") <- "y"
  closure <- list(entry)
  size_metadata <- list(
    n_var_ele = 6,
    n_exo_ele = exo_rows
  )
  list(
    model = model, var_extract = var_extract, sets = sets,
    closure = closure, size_metadata = size_metadata
  )
}

test_that("a squared closure passes the count check", {
  fx <- square_fixture(exo_rows = 3L)
  expect_no_error(
    .check_system_square(
      model = fx$model, var_extract = fx$var_extract, sets = fx$sets,
      closure = fx$closure, size_metadata = fx$size_metadata, call = NULL
    )
  )
})

test_that("unsquared closures abort with arithmetic and candidates", {
  fx <- square_fixture(exo_rows = 2L)
  expect_snapshot_error(
    .check_system_square(
      model = fx$model, var_extract = fx$var_extract, sets = fx$sets,
      closure = fx$closure, size_metadata = fx$size_metadata, call = NULL
    )
  )
})

test_that("over-exogenized closures name endogenizing candidates", {
  fx <- square_fixture(exo_rows = 3L)
  fx$size_metadata$n_exo_ele <- 4
  expect_snapshot_error(
    .check_system_square(
      model = fx$model, var_extract = fx$var_extract, sets = fx$sets,
      closure = fx$closure, size_metadata = fx$size_metadata, call = NULL
    )
  )
})

test_that("unresolvable quantifier sets skip the count check", {
  fx <- square_fixture(exo_rows = 2L)
  fx$model$tab <- "Equation E_x (all,z,MYSTERY) x(z) = 1;"
  expect_no_error(
    .check_system_square(
      model = fx$model, var_extract = fx$var_extract, sets = fx$sets,
      closure = fx$closure, size_metadata = fx$size_metadata, call = NULL
    )
  )
})

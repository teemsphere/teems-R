skip_on_cran()

# S8 (dev/validation_table.md): intertemporal time ranges must be
# non-empty, ascending, and within the model's time steps; the solver
# aborts on these, and R's start:end would otherwise silently count
# DOWN on an inverted range

test_that("intertemporal ranges convert", {
  m <- .convert_int_sets(
    expr = "P[0] - P[NTSP-1]",
    n_timestep = 5,
    n_timestep_coeff = "NTSP",
    set_name = "ALLTIME",
    call = NULL
  )
  expect_equal(m$mapping, 0:4)
  single <- .convert_int_sets(
    expr = "P[NTSP-1]",
    n_timestep = 5,
    n_timestep_coeff = "NTSP",
    set_name = "ENDTIME",
    call = NULL
  )
  expect_equal(single$mapping, 4)
})

test_that("empty or inverted time ranges abort", {
  expect_snapshot_error(
    .convert_int_sets(
      expr = "P[3] - P[1]",
      n_timestep = 5,
      n_timestep_coeff = "NTSP",
      set_name = "BADT",
      call = NULL
    )
  )
})

test_that("out-of-range time indices abort", {
  expect_snapshot_error(
    .convert_int_sets(
      expr = "P[0] - P[NTSP]",
      n_timestep = 5,
      n_timestep_coeff = "NTSP",
      set_name = "BADT",
      call = NULL
    )
  )
})

test_that("malformed intertemporal terms abort", {
  expect_snapshot_error(
    .convert_int_sets(
      expr = "P[frog]",
      n_timestep = 5,
      n_timestep_coeff = "NTSP",
      set_name = "BADT",
      call = NULL
    )
  )
})

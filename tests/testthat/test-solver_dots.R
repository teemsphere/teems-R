skip_on_cran()

# ems_solve()'s dots carry, besides the RK step controls, the MA48
# workspace initial guesses (laA/laD/laDi) and the expert solver
# flags formerly passed as raw append_args strings. Absent la args
# warm-start from the previous run's recorded la_used, else cold
# defaults; the solver grows the workspace from whatever it gets.

mk_cmf <- function(dir) {
  tab <- file.path(dir, "m.tab")
  writeLines("x", tab)
  cmf <- file.path(dir, "m.cmf")
  writeLines(sprintf("tabfile \"%s\";", tab), cmf)
  cmf
}

exec_cmd <- function(dir) {
  readLines(file.path(dir, "model_exec.txt"), warn = FALSE)
}

mk_stats <- function(dir, laA = 410, laDi = 520, laD = 210) {
  stats_dir <- file.path(dir, "out", "variables", "bin")
  dir.create(stats_dir, recursive = TRUE)
  writeLines(
    sprintf(
      '{"version": 2, "la_used": {"laA": %d, "laDi": %d, "laD": %d}}',
      laA, laDi, laD
    ),
    file.path(stats_dir, "sol.stats.json")
  )
}

test_that("cold-start la defaults reach the command line", {
  d <- withr::local_tempdir()
  cmf <- mk_cmf(d)
  suppressMessages(ems_solve(cmf, terminal_run = TRUE))
  cmd <- exec_cmd(d)
  expect_match(cmd, "-laA 300", fixed = TRUE)
  expect_match(cmd, "-laDi 500", fixed = TRUE)
  expect_match(cmd, "-laD 200", fixed = TRUE)
})

test_that("dot-passed la args set the initial guess", {
  d <- withr::local_tempdir()
  cmf <- mk_cmf(d)
  suppressMessages(ems_solve(cmf, terminal_run = TRUE, laA = 350L, laD = 250L))
  cmd <- exec_cmd(d)
  expect_match(cmd, "-laA 350", fixed = TRUE)
  expect_match(cmd, "-laD 250", fixed = TRUE)
  expect_match(cmd, "-laDi 500", fixed = TRUE)
})

test_that("absent la args warm-start from the previous run's la_used", {
  d <- withr::local_tempdir()
  cmf <- mk_cmf(d)
  mk_stats(d)
  suppressMessages(ems_solve(cmf, terminal_run = TRUE))
  cmd <- exec_cmd(d)
  expect_match(cmd, "-laA 410", fixed = TRUE)
  expect_match(cmd, "-laDi 520", fixed = TRUE)
  expect_match(cmd, "-laD 210", fixed = TRUE)
})

test_that("a user-passed la arg beats the warm start", {
  d <- withr::local_tempdir()
  cmf <- mk_cmf(d)
  mk_stats(d)
  suppressMessages(ems_solve(cmf, terminal_run = TRUE, laA = 999L))
  cmd <- exec_cmd(d)
  expect_match(cmd, "-laA 999", fixed = TRUE)
  expect_match(cmd, "-laDi 520", fixed = TRUE)
})

test_that("an unreadable stats file falls back to the cold defaults", {
  d <- withr::local_tempdir()
  cmf <- mk_cmf(d)
  stats_dir <- file.path(d, "out", "variables", "bin")
  dir.create(stats_dir, recursive = TRUE)
  writeLines("not json", file.path(stats_dir, "sol.stats.json"))
  suppressMessages(ems_solve(cmf, terminal_run = TRUE))
  cmd <- exec_cmd(d)
  expect_match(cmd, "-laA 300", fixed = TRUE)
})

test_that("expert flags render as solver CLI flags", {
  d <- withr::local_tempdir()
  cmf <- mk_cmf(d)
  suppressMessages(ems_solve(
    cmf,
    terminal_run = TRUE,
    fastrefac = TRUE,
    gpzerodivide = TRUE,
    cntl_3 = 0.5,
    nsbbdblocks = 4L,
    withmc66 = FALSE,
    nowrites = TRUE,
    condest = TRUE
  ))
  cmd <- exec_cmd(d)
  expect_match(cmd, "-fastrefac 1", fixed = TRUE)
  expect_match(cmd, "-gpzerodivide 1", fixed = TRUE)
  expect_match(cmd, "-cntl_3 0.5", fixed = TRUE)
  expect_match(cmd, "-nsbbdblocks 4", fixed = TRUE)
  expect_match(cmd, "-withmc66 0", fixed = TRUE)
  expect_match(cmd, "-nowrites 1", fixed = TRUE)
  expect_match(cmd, "-condest 1", fixed = TRUE)
  expect_no_match(cmd, "-cntl_6", fixed = TRUE)
  expect_no_match(cmd, "-tempdir", fixed = TRUE)
})

test_that("removed append_args formal is an unknown dots argument", {
  d <- withr::local_tempdir()
  cmf <- mk_cmf(d)
  expect_error(
    ems_solve(cmf, append_args = "-gpzerodivide 1"),
    "append_args",
    class = "rlang_error"
  )
})

test_that("dot-passed extras are validated", {
  d <- withr::local_tempdir()
  cmf <- mk_cmf(d)
  expect_error(
    ems_solve(cmf, laA = -5L),
    "laA",
    class = "rlang_error"
  )
  expect_error(
    ems_solve(cmf, fastrefac = "yes"),
    "fastrefac",
    class = "rlang_error"
  )
  expect_error(
    ems_solve(cmf, condest = "yes"),
    "condest",
    class = "rlang_error"
  )
  expect_error(
    ems_solve(cmf, tempdir = 1),
    "tempdir",
    class = "rlang_error"
  )
  expect_error(
    ems_solve(cmf, cntl_3 = "high"),
    "cntl_3",
    class = "rlang_error"
  )
})

test_that("solve_in_situ solver_args must be a fully named allowlisted list", {
  expect_snapshot_error(solve_in_situ(solver_args = list(300L)))
  expect_snapshot_error(solve_in_situ(solver_args = list(bogus = 1)))
  expect_error(
    solve_in_situ(solver_args = list(adaptive = "yes")),
    "adaptive",
    class = "rlang_error"
  )
})

test_that("ems_probe rejects unknown dot arguments", {
  expect_snapshot_error(ems_probe("no.cmf", bogus_argument = 1))
})

skip_on_cran()

local_solver_log <- function(lines, env = parent.frame()) {
  run_dir <- withr::local_tempdir(.local_envir = env)
  diag_out <- file.path(run_dir, "diag.txt")
  writeLines(lines, diag_out)
  list(diag_out = diag_out, run = run_dir)
}

check_log <- function(paths) {
  .check_solver_log(
    elapsed_time = proc.time(),
    solve_cmd = "cmd",
    paths = paths,
    call = NULL
  )
}

test_that("TAB errors map to the model-specification abort", {
  paths <- local_solver_log(c(
    "solver banner",
    "Error: name psave is declared as both a coefficient and a variable; names are case-insensitive and must be unique (manual 11.2.1)"
  ))
  expect_error(
    check_log(paths),
    "rejected the model specification with 1 error"
  )
  expect_error(check_log(paths), "11.2.1")
})

test_that("multiple TAB errors are previewed with their manual sections", {
  paths <- local_solver_log(c(
    "Error: coefficient dupx is declared more than once (manual 11.2.1)",
    "Error: unknown variable qualifier 'foo'"
  ))
  expect_error(check_log(paths), "2 errors")
  expect_error(check_log(paths), "unknown variable qualifier")
})

test_that("closure errors map to the closure abort", {
  paths <- local_solver_log(c(
    "Error: variable qgdp is not declared",
    "Error: element usa is not in set REG (in qxs)"
  ))
  expect_error(
    check_log(paths),
    "rejected the closure or shock inputs with 2 errors"
  )
})

test_that("shock-file errors map to the closure abort", {
  paths <- local_solver_log(
    "Error: qxs in the shock file is not a declared variable"
  )
  expect_error(check_log(paths), "closure or shock inputs")
})

test_that("data errors map to the data abort", {
  paths <- local_solver_log(
    'Error: header "VKB" not found in the data file'
  )
  expect_error(check_log(paths), "could not read the model data")
})

test_that("runtime errors map to the numeric abort", {
  paths <- local_solver_log(
    "Error: division by zero in a formula; Zerodivide (nonzero_by_zero) is off (GEMPACK default) -- set a default or guard with ID01"
  )
  expect_error(check_log(paths), "runtime error")
  expect_error(check_log(paths), "10.11.1")
})

test_that("TAB class takes priority over data class", {
  paths <- local_solver_log(c(
    "Error: cannot open file baddata.har",
    "Error: set marg is declared as both a coefficient and a set (manual 11.2.1)"
  ))
  expect_error(check_log(paths), "rejected the model specification")
})

test_that("unmapped Error lines fall back to the generic abort", {
  paths <- local_solver_log(
    "Error: some entirely novel condition"
  )
  expect_error(check_log(paths), "Errors detected during solution")
})

test_that("singularity without Error lines routes to the probe hint", {
  paths <- local_solver_log(
    "MA48: the matrix is singular at step 1"
  )
  expect_error(check_log(paths), "Singularity detected")
  expect_error(check_log(paths), "ems_probe")
})

test_that("cli braces in solver output do not break glue rendering", {
  paths <- local_solver_log(
    "Error: malformed formula {unbalanced} in TAB file"
  )
  expect_error(check_log(paths), "rejected the model specification")
})

test_that("a clean log passes and writes the exec record", {
  paths <- local_solver_log(c(
    "solver banner",
    "all steps complete"
  ))
  expect_no_error(suppressMessages(check_log(paths)))
  expect_true(file.exists(file.path(paths$run, "model_exec.txt")))
})

test_that(".map_solver_errors classifies representative catalog lines", {
  mapped <- .map_solver_errors(c(
    "coefficient name max is a reserved word (manual 11.2.1)",
    "duplicate lower bound on a coefficient declaration (one lower GE/GT and one upper LE/LT allowed)",
    "Equation (default=levels) is not supported -- the solver handles linearized equations only",
    "PostSim Formula assigns variable psave; simulation results cannot be changed (manual 12.2.2)",
    "set nmrg references itself in a set expression",
    "the $POS function is not supported yet: $POS(r)",
    "Read without a header is not supported (use 'Read X from file <log> header \"H\"')",
    "variable qq is not declared",
    "zero divided by zero in a formula while Zerodivide (zero_by_zero) is off",
    "assertion failed (Assertions = warn/no in the CMF file suppresses/downgrades this abort)"
  ))
  expect_identical(
    mapped$class,
    c(
      "tab", "tab", "tab", "tab", "tab", "tab", "tab",
      "closure", "numeric", "numeric"
    )
  )
  expect_identical(mapped$manual[1], "11.2.1")
  expect_identical(mapped$manual[9], "10.11.1")
})

skip_on_cran()

# Phase 3c(a) gated e2e negative legs (dev/validation_table.md): the
# solver-only rows -- runtime zerodivide (Z1/Z2), assertions (Z3),
# CMF-fatal range tests (B2) and intrinsic arity (I2/I3) -- pushed
# through real ems_solve runs to exercise the chk_solver_log "Error:"
# mapping end to end. Probe statements mirror the solver-side
# .audit/{zdiv,bounds,intrinsics}-test-kit negative legs. The legs
# assume the local teems image is current with the solver tree
# (features landed 2026-07-26: A1 -gpzerodivide, A4 CMF range tests,
# 3.1 intrinsic arity).

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "solver_err_e2e")
if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}
dir.create(write_dir, recursive = TRUE)
ems_option_set(verbose = FALSE, tempdir = write_dir)
withr::defer(ems_option_reset(), teardown_env())

model_files <- ems_example("GTAPv7", write_dir)
model_file <- model_files[["model_file"]]
closure_file <- model_files[["closure_file"]]

if (nzchar(dat_input)) {
  static_data <- ems_data(
    dat_input = dat_input,
    par_input = par_input,
    set_input = set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  )
}

image_created <- function(image) {
  out <- suppressWarnings(system2(
    "docker",
    c("image", "inspect", "-f", "{{.Created}}", image),
    stdout = TRUE,
    stderr = FALSE
  ))
  if (length(out) == 0L || !nzchar(out[1])) {
    return(NA)
  }
  as.Date(substr(out[1], 1, 10))
}

skip_if_no_e2e <- function() {
  skip_if(!nzchar(dat_input), "GTAP data not available")
  img <- paste0("teems:", .resolve_docker_tag())
  skip_if(!.docker_image_present(img), "teems image not available")
  created <- image_created(img)
  skip_if(
    is.na(created) || created < as.Date("2026-07-26"),
    paste(img, "predates the solver error surfaces under test")
  )
}

# deploys a probe-mutated GTAPv7 and returns the ems_solve abort
# message (whitespace-collapsed, ANSI-stripped); "NO ERROR" if the
# solve unexpectedly succeeds
solve_error_msg <- function(leg, tab_text, cmf_lines = NULL, ...) {
  nest_temp(leg, write_dir)
  tab_file <- write_modified_model(model_file, tab_text)
  model <- ems_model(tab_file, closure_file)
  cmf_path <- ems_deploy(static_data, model)
  if (!is.null(cmf_lines)) {
    cat(cmf_lines, file = cmf_path, sep = "\n", append = TRUE)
  }
  err <- tryCatch(
    {
      suppressMessages(suppressWarnings(ems_solve(cmf_path, ...)))
      NULL
    },
    error = function(e) e
  )
  if (is.null(err)) {
    return("NO ERROR")
  }
  gsub("\\s+", " ", cli::ansi_strip(conditionMessage(err)))
}

test_that("0/0 with zero_by_zero off maps to a named runtime abort (Z1)", {
  skip_if_no_e2e()
  msg <- solve_error_msg(
    "e2e_zbz",
    paste(
      "Zerodivide off;",
      "Coefficient ZDTD # zdiv off probe #;",
      "Formula ZDTD = 0/0;",
      sep = "\n"
    ),
    gpzerodivide = TRUE
  )
  expect_match(msg, "runtime error")
  expect_match(msg, "zero divided by zero in a formula", fixed = TRUE)
  expect_match(msg, "10.11.1", fixed = TRUE)
})

test_that("1/0 with nonzero_by_zero off maps to a named runtime abort (Z2)", {
  skip_if_no_e2e()
  msg <- solve_error_msg(
    "e2e_nbz",
    paste(
      "Zerodivide (nonzero_by_zero) off;",
      "Coefficient ZDTE # zdiv nbz off probe #;",
      "Formula ZDTE = 1/0;",
      sep = "\n"
    ),
    gpzerodivide = TRUE
  )
  expect_match(msg, "runtime error")
  expect_match(msg, "division by zero in a formula", fixed = TRUE)
  expect_match(msg, "10.11.1", fixed = TRUE)
})

test_that("a failing assertion maps to a named runtime abort (Z3)", {
  skip_if_no_e2e()
  msg <- solve_error_msg(
    "e2e_assert",
    paste(
      "Coefficient ZAST # assertion probe #;",
      "Formula ZAST = 1;",
      "Assertion # Probe Assertion Must Fail # ZAST gt 5;",
      sep = "\n"
    )
  )
  expect_match(msg, "runtime error")
  expect_match(msg, "assertion failed", fixed = TRUE)
  expect_match(msg, "25.3", fixed = TRUE)
})

test_that("a fatal range-test bound violation maps to a named runtime abort (B2)", {
  skip_if_no_e2e()
  msg <- solve_error_msg(
    "e2e_range",
    paste(
      "Coefficient (le 10) (all,r,REG) BNDP(r) # bound probe #;",
      "Formula (all,r,REG) BNDP(r) = 20;",
      sep = "\n"
    ),
    # range-test modes are CLI controls via the R API; the solver no
    # longer parses CMF statements (CMF = file manifest only)
    range_test_initial = "fatal"
  )
  expect_match(msg, "runtime error")
  expect_match(msg, "has a value above its declared upper bound", fixed = TRUE)
  expect_match(msg, "25.4.4", fixed = TRUE)
})

test_that("ID0V arity errors map to the model-specification abort (I2)", {
  skip_if_no_e2e()
  msg <- solve_error_msg(
    "e2e_id0v",
    paste(
      "Coefficient IARG # arity probe #;",
      "Formula IARG = ID0V(1,2,3);",
      sep = "\n"
    )
  )
  expect_match(msg, "rejected the model specification")
  expect_match(msg, "ID0V takes exactly 2 arguments", fixed = TRUE)
  expect_match(msg, "11.5", fixed = TRUE)
})

test_that("MAX arity errors map to the model-specification abort (I3)", {
  skip_if_no_e2e()
  msg <- solve_error_msg(
    "e2e_max",
    paste(
      "Coefficient IMX # arity probe #;",
      "Formula IMX = MAX(5);",
      sep = "\n"
    )
  )
  expect_match(msg, "rejected the model specification")
  expect_match(msg, "MAX takes at least 2 arguments", fixed = TRUE)
  expect_match(msg, "11.5.1", fixed = TRUE)
})

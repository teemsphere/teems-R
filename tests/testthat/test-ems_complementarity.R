skip_on_cran()

# ch. 51 complementarity run controls (teems-solver 09bf33b CLI flags
# + c6e7e80 stats.json options record): ems_complementarity() spec
# constructor -> ems_solve(complementarity = ) -> -comp_* solver
# flags; effective values recorded in sol.stats.json and rendered
# into model_diagnostics.txt (the CMF stays a file manifest by
# design). Constructor legs need no data or docker.

test_that("ems_complementarity builds a spec with only set fields", {
  spec <- ems_complementarity(steps_approx_run = 20, state_bound_error = "warn")
  expect_s3_class(spec, "teems_complementarity")
  expect_identical(spec$steps_approx_run, 20L)
  expect_identical(spec$state_bound_error, "warn")
  expect_null(spec$redo_steps)
  expect_null(spec$do_approx_run)
})

test_that("constructor validation aborts", {
  expect_snapshot_error(ems_complementarity(steps_approx_run = 0))
  expect_snapshot_error(ems_complementarity(steps_approx_run = 2.5))
  expect_snapshot_error(ems_complementarity(redo_steps = "yes"))
  expect_snapshot_error(ems_complementarity(redo_step_min_fraction = 1.5))
  expect_snapshot_error(ems_complementarity(state_bound_error = "maybe"))
})

test_that("both runs disabled aborts", {
  expect_snapshot_error(
    ems_complementarity(do_approx_run = FALSE, do_acc_run = FALSE)
  )
})

test_that(".comp_cli_flags renders only the set fields", {
  expect_null(.comp_cli_flags(NULL))
  expect_identical(.comp_cli_flags(ems_complementarity()), "")
  expect_identical(
    .comp_cli_flags(ems_complementarity(steps_approx_run = 20, state_bound_error = "warn")),
    "-comp_steps 20 -comp_sberr_warn 1"
  )
  expect_identical(
    .comp_cli_flags(ems_complementarity(
      redo_steps = FALSE,
      redo_step_min_fraction = 0.01,
      do_approx_run = TRUE,
      do_acc_run = FALSE
    )),
    "-comp_redo 0 -comp_redo_min_frac 0.01 -comp_do_approx 1 -comp_do_acc 0"
  )
})

# --- deploy-dependent legs ------------------------------------------

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")
skip_if(!nzchar(dat_input), "GTAP data not available")

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "comp_spec")
if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}
dir.create(write_dir, recursive = TRUE)
ems_option_set(verbose = FALSE, tempdir = write_dir)
withr::defer(ems_option_reset(), teardown_env())

conv <- GTAP_convert(dat_input, par_input, set_input)
cp_data <- suppressMessages(ems_data(
  dat_input = conv$dat,
  par_input = conv$par,
  set_input = conv$set,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg"
))

model_files <- ems_example("GTAPv7", write_dir)
base_txt <- readChar(model_files[["model_file"]],
  file.info(model_files[["model_file"]])$size
)

active_tab <- file.path(write_dir, "comp_spec.tab")
writeChar(paste0(base_txt, "\n", paste(
  "Variable (change,levels) AIM # import volume #;",
  "Formula (initial) AIM = 8;",
  "Variable (change,levels) ATQ # power of the quota tariff #;",
  "Formula (initial) ATQ = 1;",
  "Variable (change) ASH # exogenous driver #;",
  "Equation E_AIM  p_AIM = p_ASH - p_ATQ;",
  "Complementarity (variable = ATQ, lower_bound = 1) CMPF 10 - AIM;",
  sep = "\n"
), "\n"), active_tab, eos = NULL)

test_that("ems_solve rejects a non-spec complementarity", {
  nest_temp("comp_spec_reject", write_dir)
  model <- ems_model(active_tab, model_files[["closure_file"]])
  cmf_path <- ems_deploy(
    cp_data,
    model,
    shock = ems_uniform_shock(var = "ASH", value = 4.4),
    swap_in = "ASH"
  )
  expect_snapshot_error(
    ems_solve(cmf_path, complementarity = list(steps_approx_run = 5))
  )
})

# --- e2e: controls reach the solver, the run records them (needs a
# --- teems image with the options record, teems-solver c6e7e80+) ----

solver_has_record <- function() {
  img <- paste0("teems:", .resolve_docker_tag())
  if (!.docker_image_present(img)) {
    return(FALSE)
  }
  out <- suppressWarnings(system2(
    "docker",
    c(
      "run", "--rm", img, "/bin/bash", "-c",
      shQuote("grep -c '\"options\": {' /opt/teems-solver/solver/teems-solver")
    ),
    stdout = TRUE,
    stderr = FALSE
  ))
  length(out) > 0L && !is.na(suppressWarnings(as.integer(out[1]))) &&
    as.integer(out[1]) > 0L
}

test_that("controls reach the solver and the run records them (e2e)", {
  nest_temp("comp_spec_e2e", write_dir)
  skip_if(
    !solver_has_record(),
    "teems image absent or predates the effective-options record"
  )
  model <- ems_model(active_tab, model_files[["closure_file"]])
  cmf_path <- ems_deploy(
    cp_data,
    model,
    shock = ems_uniform_shock(var = "ASH", value = 4.4),
    swap_in = "ASH"
  )
  out <- suppressMessages(ems_solve(
    cmf_path,
    complementarity = ems_complementarity(steps_approx_run = 12)
  ))
  expect_s3_class(out, "data.frame")
  expect_equal(out$dat[["ATQ"]]$Value, 2.4, tolerance = 1e-3)
  run_dir <- dirname(cmf_path)
  stats <- jsonlite::read_json(
    file.path(run_dir, "out", "variables", "bin", "sol.stats.json"),
    simplifyVector = TRUE
  )
  expect_identical(stats$options$complementarity$steps_approx_run, 12L)
  expect_identical(stats$options$complementarity$pass, "accurate")
  diag_txt <- readLines(file.path(run_dir, "model_diagnostics.txt"))
  expect_true(any(grepl("-- Solve record", diag_txt, fixed = TRUE)))
  expect_true(any(grepl(
    "Complementarity: 1 active component(s); approximate run 12 Euler steps",
    diag_txt,
    fixed = TRUE
  )))
})

skip_on_cran()

# run-mode switches through the R API (solver c099d5f: -assertions /
# -range_test_initial / -range_test_updated / -postsim flags; the CMF
# statements are gone -- the CMF is a file manifest). Effective values
# are recorded in sol.stats.json and model_diagnostics.txt.

test_that("mode-switch validation aborts", {
  expect_snapshot_error(
    ems_solve("nope.cmf", assertions = "maybe"),
    class = "rlang_error"
  )
  expect_snapshot_error(
    ems_solve("nope.cmf", range_test_initial = 2),
    class = "rlang_error"
  )
  expect_snapshot_error(
    ems_solve("nope.cmf", postsim = "yes"),
    class = "rlang_error"
  )
})

# --- e2e: switches reach the solver and the run records them --------

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")
skip_if(!nzchar(dat_input), "GTAP data not available")

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "switches")
if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}
dir.create(write_dir, recursive = TRUE)
ems_option_set(verbose = FALSE, tempdir = write_dir)
withr::defer(ems_option_reset(), teardown_env())

solver_has_switches <- function() {
  img <- paste0("teems:", .resolve_docker_tag())
  if (!.docker_image_present(img)) {
    return(FALSE)
  }
  out <- suppressWarnings(system2(
    "docker",
    c(
      "run", "--rm", img, "/bin/bash", "-c",
      shQuote("grep -c 'assertions must be 0' /opt/teems-solver/solver/teems-solver")
    ),
    stdout = TRUE,
    stderr = FALSE
  ))
  length(out) > 0L && !is.na(suppressWarnings(as.integer(out[1]))) &&
    as.integer(out[1]) > 0L
}

test_that("switches reach the solver and the run records them (e2e)", {
  nest_temp("switches_e2e", write_dir)
  skip_if(
    !solver_has_switches(),
    "teems image absent or predates the run-mode flags"
  )
  conv <- GTAP_convert(dat_input, par_input, set_input)
  d <- suppressMessages(ems_data(
    dat_input = conv$dat,
    par_input = conv$par,
    set_input = conv$set,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  ))
  model_files <- ems_example("GTAPv7", write_dir)
  tab <- file.path(write_dir, "switches.tab")
  base_txt <- readChar(model_files[["model_file"]],
    file.info(model_files[["model_file"]])$size
  )
  # a failing assertion: fatal under the solver default, downgraded here
  writeChar(paste0(base_txt, "\nAssertion # Never Holds # 1 < 0;\n"),
    tab,
    eos = NULL
  )
  model <- ems_model(tab, model_files[["closure_file"]])
  cmf_path <- ems_deploy(d, model)
  out <- suppressMessages(ems_solve(
    cmf_path,
    assertions = "warn",
    range_test_initial = "off",
    postsim = FALSE
  ))
  expect_s3_class(out, "data.frame")
  run_dir <- dirname(cmf_path)
  stats <- jsonlite::read_json(
    file.path(run_dir, "out", "variables", "bin", "sol.stats.json"),
    simplifyVector = TRUE
  )
  expect_identical(stats$options$assertions, "warn")
  expect_identical(stats$options$range_test_initial, "off")
  expect_identical(stats$options$range_test_updated, "warn")
  expect_false(stats$options$postsim)
  diag_txt <- readLines(file.path(run_dir, "model_diagnostics.txt"))
  expect_true(any(grepl(
    "Modes: assertions warn; range test initial off, updated warn; postsim off",
    diag_txt,
    fixed = TRUE
  )))
})

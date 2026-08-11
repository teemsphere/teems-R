skip_on_cran()

# 6.3(f3) coefficient-storage precision: precision = "double" selects
# the f64 solver binary shipped alongside the default in the same
# image; the effective precision is recorded in sol.stats.json and the
# model_diagnostics.txt solve record.

test_that("precision validation aborts", {
  # a real (empty) cmf so the path check passes and the argument
  # validation is what aborts
  cmf <- withr::local_tempfile(fileext = ".cmf")
  writeLines("", cmf)
  expect_error(
    ems_solve(cmf, precision = "quad"),
    "precision",
    class = "rlang_error"
  )
  expect_error(
    ems_solve(cmf, precision = c("single", "double", "extra")),
    "precision",
    class = "rlang_error"
  )
})

# --- e2e: the f64 binary runs and the record names it ---------------

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")
skip_if(!nzchar(dat_input), "GTAP data not available")

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "precision")
if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}
dir.create(write_dir, recursive = TRUE)
ems_option_set(verbose = FALSE, tempdir = write_dir)
withr::defer(ems_option_reset(), teardown_env())

image_has_f64 <- function() {
  img <- paste0("teems:", .resolve_docker_tag())
  if (!.docker_image_present(img)) {
    return(FALSE)
  }
  rc <- suppressWarnings(system2(
    "docker",
    c(
      "run", "--rm", img, "/bin/bash", "-c",
      shQuote("test -x /opt/teems-solver/solver/teems-solver-f64")
    )
  ))
  identical(rc, 0L)
}

test_that("precision = \"double\" reaches the solver and the record (e2e)", {
  nest_temp("precision_e2e", write_dir)
  skip_if(
    !image_has_f64(),
    "teems image absent or predates the f64 solver binary"
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
  model <- ems_model(model_files[["model_file"]], model_files[["closure_file"]])
  cmf_path <- ems_deploy(d, model)
  out <- suppressMessages(ems_solve(
    cmf_path,
    solution_method = "Gragg",
    precision = "double"
  ))
  expect_s3_class(out, "data.frame")
  run_dir <- dirname(cmf_path)
  stats <- jsonlite::read_json(
    file.path(run_dir, "out", "variables", "bin", "sol.stats.json"),
    simplifyVector = TRUE
  )
  expect_identical(stats$options$store_precision, "double")
  diag_txt <- readLines(file.path(run_dir, "model_diagnostics.txt"))
  expect_true(any(grepl(
    "Coefficient storage: double precision",
    diag_txt,
    fixed = TRUE
  )))
})

test_that("default precision records single (e2e)", {
  nest_temp("precision_single_e2e", write_dir)
  skip_if(
    !image_has_f64(),
    "teems image absent or predates the f64 solver binary"
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
  model <- ems_model(model_files[["model_file"]], model_files[["closure_file"]])
  cmf_path <- ems_deploy(d, model)
  out <- suppressMessages(ems_solve(cmf_path))
  expect_s3_class(out, "data.frame")
  stats <- jsonlite::read_json(
    file.path(dirname(cmf_path), "out", "variables", "bin", "sol.stats.json"),
    simplifyVector = TRUE
  )
  expect_identical(stats$options$store_precision, "single")
})

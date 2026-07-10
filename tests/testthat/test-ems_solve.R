skip_on_cran()

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "solve")

if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}

dir.create(write_dir, recursive = TRUE)
ems_option_set(verbose = FALSE,
               tempdir = write_dir)
withr::defer(ems_option_reset(), teardown_env())

static_data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg"
)

dynamic_data <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  time_steps = c(0, 1, 2)
)

dynamic_model <- "GTAP-RE"
dynamic_model_files <- ems_example(dynamic_model, write_dir)
dynamic_model_file <- dynamic_model_files[["model_file"]]
dynamic_closure_file <- dynamic_model_files[["closure_file"]]
dynamic_model <- ems_model(dynamic_model_file, dynamic_closure_file)

static_model <- "GTAPv7"
static_model_files <- ems_example(static_model, write_dir)
static_model_file <- static_model_files[["model_file"]]
static_closure_file <- static_model_files[["closure_file"]]
static_model <- ems_model(static_model_file, static_closure_file)

variant <- Sys.info()["sysname"]

test_that("ems_solve suppress_outputs returns cmf_path character", {
  nest_temp("solve_suppress", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  result <- ems_solve(cmf_path, suppress_outputs = TRUE)
  expect_type(result, "NULL")
})

test_that("ems_solve errors when cmf_path is missing", {
  expect_snapshot_error(ems_solve())
})

test_that("ems_solve errors when n_tasks is not integerish", {
  nest_temp("solve_err_tasks", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  expect_snapshot_error(ems_solve(cmf_path, n_tasks = 1.5))
})

test_that("ems_solve errors when steps is not length 3", {
  nest_temp("solve_err_steps", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  expect_snapshot_error(ems_solve(cmf_path, steps = c(2L, 4L)))
})

test_that("ems_solve errors when steps are mixed odd/even", {
  nest_temp("solve_err_mixed", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  expect_snapshot_error(ems_solve(cmf_path, steps = c(2L, 3L, 4L)))
})

test_that("ems_solve errors when SBBD used with static model", {
  nest_temp("solve_err_sbbd", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  expect_snapshot_error(ems_solve(cmf_path, matrix_method = "SBBD"))
})

test_that("ems_solve errors when inmemory is not a logical scalar", {
  nest_temp("solve_err_inmemory", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  expect_snapshot_error(ems_solve(cmf_path, inmemory = "yes"))
  expect_snapshot_error(ems_solve(cmf_path, inmemory = c(TRUE, FALSE)))
})

test_that("ems_solve errors when verbosity is invalid", {
  nest_temp("solve_err_verbosity", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  expect_snapshot_error(ems_solve(cmf_path, verbosity = 1.5))
  expect_snapshot_error(ems_solve(cmf_path, verbosity = 3L))
})

test_that("inmemory and verbosity reach the solver command", {
  nest_temp("solve_flags_cmd", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  run_dir <- dirname(cmf_path)
  suppressMessages(
    ems_solve(cmf_path,
      inmemory = FALSE,
      verbosity = 0L,
      terminal_run = TRUE
    )
  )
  cmd <- readLines(file.path(run_dir, "model_exec.txt"), warn = FALSE)
  expect_match(paste(cmd, collapse = " "), "-inmemory 0", fixed = TRUE)
  expect_match(paste(cmd, collapse = " "), "-verbosity 0", fixed = TRUE)

  # defaults: neither flag is passed, the solver decides
  suppressMessages(ems_solve(cmf_path, terminal_run = TRUE))
  cmd <- readLines(file.path(run_dir, "model_exec.txt"), warn = FALSE)
  expect_no_match(paste(cmd, collapse = " "), "-inmemory", fixed = TRUE)
  expect_no_match(paste(cmd, collapse = " "), "-verbosity", fixed = TRUE)
})

test_that("ems_solve errors when solution errors detected", {
  nest_temp("solve_err_error", write_dir)
  shock <- ems_uniform_shock("pop", 1e6)
  cmf_path <- ems_deploy(static_data, static_model, shock)
  expect_snapshot(ems_solve(cmf_path),
    error = TRUE,
    transform = function(lines) {
      gsub("solver_out_\\d{4}\\.txt", "solver_out_HHMM.txt", lines)
    },
    variant = variant
  )
})

test_that("ems_solve errors when solution singularity detected", {
  nest_temp("solve_err_sing", write_dir)
  cmf_path <- ems_deploy(static_data, static_model, swap_out = "pop")
  expect_snapshot(ems_solve(cmf_path),
    error = TRUE,
    transform = function(lines) {
      gsub("solver_out_\\d{4}\\.txt", "solver_out_HHMM.txt", lines)
    },
    variant = variant
  )
})

test_that("ems_solve warns when poor accuracy", {
  nest_temp("solve_wrn_accur", write_dir)
  shock <- ems_uniform_shock("pop", 200)
  cmf_path <- ems_deploy(static_data, static_model, shock)
  expect_snapshot_warning(ems_solve(
    cmf_path,
    solution_method = "mod_midpoint"
  ))
})

test_that("ems_solve returns NULL when suppress_outputs", {
  nest_temp("suppress", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  expect_null(ems_solve(cmf_path, suppress_outputs = TRUE))
})

test_that("ems_solve informs terminal run", {
  nest_temp("solve_info_terminal", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  expect_snapshot(
    ems_solve(cmf_path, terminal_run = TRUE),
    transform = function(lines) {
      gsub("solver_out_\\d{4}\\.txt", "solver_out_HHMM.txt", lines)
    },
    variant = variant
  )
})

test_that("matrix_method auto resolves by model type", {
  nest_temp("solve_auto_static", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  expect_snapshot(
    ems_solve(cmf_path, matrix_method = "auto", terminal_run = TRUE),
    transform = function(lines) {
      gsub("solver_out_\\d{4}\\.txt", "solver_out_HHMM.txt", lines)
    },
    variant = variant
  )
  nest_temp("solve_auto_dynamic", write_dir)
  cmf_path <- ems_deploy(dynamic_data, dynamic_model)
  expect_snapshot(
    ems_solve(cmf_path,
      solution_method = "mod_midpoint",
      matrix_method = "auto",
      terminal_run = TRUE
    ),
    transform = function(lines) {
      gsub("solver_out_\\d{4}\\.txt", "solver_out_HHMM.txt", lines)
    },
    variant = variant
  )
})

test_that("matrix_method auto selects DBBD for large static deployments", {
  nest_temp("solve_auto_dbbd", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  metadata_path <- file.path(dirname(cmf_path), "metadata.rds")
  metadata <- readRDS(metadata_path)
  metadata$system_size <- 2.5e6
  saveRDS(metadata, metadata_path)
  expect_snapshot(
    ems_solve(cmf_path, n_tasks = 2L, terminal_run = TRUE),
    transform = function(lines) {
      gsub("solver_out_\\d{4}\\.txt", "solver_out_HHMM.txt", lines)
    },
    variant = variant
  )
  expect_snapshot(
    ems_solve(cmf_path, terminal_run = TRUE),
    transform = function(lines) {
      gsub("solver_out_\\d{4}\\.txt", "solver_out_HHMM.txt", lines)
    },
    variant = variant
  )
})

test_that("deploy metadata records system size", {
  nest_temp("solve_size_meta", write_dir)
  cmf_path <- ems_deploy(static_data, static_model)
  metadata <- readRDS(file.path(dirname(cmf_path), "metadata.rds"))
  expect_identical(metadata$system_size, 3485)
  expect_identical(metadata$n_reg, 3L)
})

test_that("set expressions solve identically to pairwise forms", {
  nest_temp("solve_set_expr_base", write_dir)
  cmf_base <- ems_deploy(static_data, static_model)
  base <- ems_solve(cmf_base)

  nest_temp("solve_set_expr", write_dir)
  expr_file <- write_modified_model(
    static_model_file,
    NULL,
    .fn = function(m, t) {
      old1 <- "COSTS # industry cost summary # = ENDW + NENDWCOSTS;"
      old2 <- "ENDWM # mobile endowments # = ENDW - ENDWFS;"
      stopifnot(grepl(old1, m, fixed = TRUE), grepl(old2, m, fixed = TRUE))
      m <- sub(old1, "COSTS # industry cost summary # = (ENDW UNION NENDWCOSTS);", m, fixed = TRUE)
      sub(old2, "ENDWM # mobile endowments # = ENDW - ENDWF - ENDWS;", m, fixed = TRUE)
    }
  )
  expr_model <- ems_model(expr_file, static_closure_file)
  cmf_expr <- ems_deploy(static_data, expr_model)
  expr_out <- ems_solve(cmf_expr)
  expect_equal(expr_out, base)
})

test_that("set equality solves identically through an equation quantifier", {
  nest_temp("solve_set_eq_base", write_dir)
  cmf_base <- ems_deploy(static_data, static_model)
  base <- ems_solve(cmf_base)

  nest_temp("solve_set_eq", write_dir)
  eq_file <- write_modified_model(
    static_model_file,
    NULL,
    .fn = function(m, t) {
      old1 <- "ENDWC is subset of ENDWMS;"
      old2 <- paste0(
        "# defines the real (tax-inclusive) return to mobile and sluggish factor e in r #\r\n",
        "(all,e,ENDWMS)(all,r,REG)"
      )
      stopifnot(grepl(old1, m, fixed = TRUE), grepl(old2, m, fixed = TRUE))
      m <- sub(
        old1,
        paste0(old1, "\r\nSet\r\n    ENDWMS2 # identical to ENDWMS # = ENDWMS;"),
        m,
        fixed = TRUE
      )
      sub(old2, sub("ENDWMS)", "ENDWMS2)", old2, fixed = TRUE), m, fixed = TRUE)
    }
  )
  eq_model <- ems_model(eq_file, static_closure_file)
  cmf_eq <- ems_deploy(static_data, eq_model)
  eq_out <- ems_solve(cmf_eq)
  expect_equal(eq_out, base)
})

test_that("IF formulas solve identically to their hand adaptations", {
  nest_temp("solve_if_base", write_dir)
  cmf_base <- ems_deploy(static_data, static_model)
  base <- ems_solve(cmf_base)

  nest_temp("solve_if", write_dir)
  if_file <- write_modified_model(
    static_model_file,
    NULL,
    .fn = function(m, t) {
      old_vcb <- paste0(
        "Formula (all,c,MARG)(all,r,REG)\r\n",
        "    VCB(c,r) = VDB(c,r) + sum{d,REG, VXSB(c,r,d)} + VST(c,r);\r\n",
        "Formula (all,c,NMRG)(all,r,REG)\r\n",
        "    VCB(c,r) = VDB(c,r) + sum{d,REG, VXSB(c,r,d)};"
      )
      new_vcb <- paste0(
        "Formula (all,c,COMM)(all,r,REG)\r\n",
        "    VCB(c,r) = VDB(c,r) + sum{d,REG, VXSB(c,r,d)} + IF[c in MARG, VST(c,r)];"
      )
      old_vxw <- paste0(
        "Formula (all,c,MARG)(all,r,REG)\r\n",
        "    VXW(c,r) = VXDFOB(c,r) + VST(c,r);\r\n",
        "Formula (all,c,NMRG)(all,r,REG)\r\n",
        "    VXW(c,r) = VXDFOB(c,r);"
      )
      new_vxw <- paste0(
        "Formula (all,c,COMM)(all,r,REG)\r\n",
        "    VXW(c,r) = VXDFOB(c,r) + IF[c in MARG, VST(c,r)];"
      )
      stopifnot(
        grepl(old_vcb, m, fixed = TRUE),
        grepl(old_vxw, m, fixed = TRUE)
      )
      m <- sub(old_vcb, new_vcb, m, fixed = TRUE)
      sub(old_vxw, new_vxw, m, fixed = TRUE)
    }
  )
  if_model <- ems_model(if_file, static_closure_file)
  cmf_if <- ems_deploy(static_data, if_model)
  if_out <- ems_solve(cmf_if)
  expect_equal(if_out, base)
})

test_that("ems_solve returns the same output across static matrix methods", {
  nest_temp("solve_static_method", write_dir)
  numeraire <- ems_uniform_shock("pfactwld", 5)
  cmf_path <- ems_deploy(static_data, static_model, numeraire)
  LU <- ems_solve(
    cmf_path,
    solution_method = "mod_midpoint",
    matrix_method = "LU",
    n_subintervals = 2
  )

  DBBD <- ems_solve(
    cmf_path,
    solution_method = "mod_midpoint",
    matrix_method = "DBBD",
    n_subintervals = 2,
    n_tasks = 2
  )

  check <- all.equal(LU, DBBD, tolerance = 1e-4)
  expect_true(check)
})

test_that("ems_solve returns the same output across dynamic matrix methods", {
  nest_temp("solve_dynamic_method", write_dir)
  numeraire <- ems_uniform_shock("pfactwld", 5)
  cmf_path <- ems_deploy(dynamic_data, dynamic_model, numeraire)
  LU <- ems_solve(
    cmf_path,
    solution_method = "mod_midpoint",
    matrix_method = "LU",
    n_subintervals = 2
  )

  SBBD <- ems_solve(
    cmf_path,
    solution_method = "mod_midpoint",
    matrix_method = "SBBD",
    n_subintervals = 2,
    n_tasks = 2
  )

  NDBBD <- ems_solve(
    cmf_path,
    solution_method = "mod_midpoint",
    matrix_method = "NDBBD",
    n_subintervals = 2,
    n_tasks = 2
  )
  
  LU_SBBD_check <- all.equal(LU, SBBD, tolerance = 1e-4)
  LU_NDBBD_check <- all.equal(LU, NDBBD, tolerance = 1e-4)
  check <- c(LU_SBBD_check, LU_NDBBD_check)
  expect_all_true(check)
})

test_that("ems_solve examples work", {
  nest_temp("solve_examples", write_dir)
  cmf_path <- ems_deploy(dynamic_data,
                         dynamic_model)
  # The following examples require the teems solver to be built.
  # See https://teemsphere.github.io/ to get started.

  # Solving a static model with Johansen:
  expect_s3_class(ems_solve(cmf_path), "tbl_df")

  # Solving a dynamic model with the SBBD method:
  expect_s3_class(ems_solve(cmf_path,
            solution_method = "mod_midpoint",
            matrix_method = "SBBD",
            n_tasks = 6), "tbl_df")
})

unlink(tools::R_user_dir("teems", "cache"), recursive = TRUE)
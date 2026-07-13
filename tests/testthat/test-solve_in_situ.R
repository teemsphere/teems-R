skip_on_cran()

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")

year <- 2023

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "in_situ")

if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}
dir.create(write_dir, recursive = TRUE)
ems_option_set(verbose = FALSE,
               tempdir = write_dir)
withr::defer(ems_option_reset(), teardown_env())

model <- "GTAP-RE"

model_files <- ems_example(model, write_dir)
model_file <- model_files[["model_file"]]
closure_file <- model_files[["closure_file"]]

time_steps <- c(0, 1, 2, 3)

dat <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  time_steps = time_steps
)

model <- ems_model(model_file, closure_file)

variant <- Sys.info()["sysname"]

test_that("solve_in_situ solves", {
  REG <- c("chn", "usa", "row")
  ENDW <- c("labor", "capital", "natlres", "land")
  COMM <- c("svces", "food", "crops", "mnfcs", "livestock")
  ACTS <- COMM
  MARG <- "svces"
  ALLTIME <- seq(0, length(time_steps) - 1)


  # 2D
  pop <- expand.grid(
    REGr = REG,
    ALLTIMEt = ALLTIME,
    stringsAsFactors = FALSE
  )

  pop <- pop[do.call(order, pop), ]
  pop$Value <- runif(nrow(pop))

  # 3D
  aoall <- expand.grid(
    ACTSa = ACTS,
    REGr = REG,
    ALLTIMEt = ALLTIME,
    stringsAsFactors = FALSE
  )

  aoall <- aoall[do.call(order, aoall), ]
  aoall$Value <- runif(nrow(aoall))

  # 4D
  afeall <- expand.grid(
    ENDWe = ENDW,
    ACTSa = ACTS,
    REGr = REG,
    ALLTIMEt = ALLTIME,
    stringsAsFactors = FALSE
  )

  afeall <- afeall[do.call(order, afeall), ]
  afeall$Value <- runif(nrow(afeall))

  # 5D
  atall <- expand.grid(
    MARGm = MARG,
    COMMc = COMM,
    REGs = REG,
    REGd = REG,
    ALLTIMEt = ALLTIME,
    stringsAsFactors = FALSE
  )

  atall <- atall[do.call(order, atall), ]
  atall$Value <- runif(nrow(atall))

  pop_shk <- ems_custom_shock( "pop", pop)
  aoall_shk <- ems_custom_shock("aoall", aoall)
  afeall_shk <- ems_custom_shock("afeall", afeall)
  atall_shk <- ems_custom_shock("atall", atall)

  subdir <- "custom_full"
  nest_temp(subdir, write_dir)

  cmf_path <- ems_deploy(dat,
                         model,
                         list(pop_shk, aoall_shk, afeall_shk, atall_shk))

  insitu_dir <- normalizePath(file.path(write_dir, subdir), "/")
  GTAPDATA <- file.path(insitu_dir, "GTAPDATA.txt")
  GTAPINT <- file.path(insitu_dir, "GTAPINT.txt")
  GTAPPARM <- file.path(insitu_dir, "GTAPPARM.txt")
  GTAPSETS <- file.path(insitu_dir, "GTAPSETS.txt")
  shock_file <- list.files(insitu_dir, pattern = "shf", full.names = TRUE)

  outputs <- solve_in_situ(
    GTAPDATA = GTAPDATA,
    GTAPINT = GTAPINT,
    GTAPPARM = GTAPPARM,
    GTAPSETS = GTAPSETS,
    model_file = model_files[["model_file"]],
    model_dir = insitu_dir,
    closure_file = model_files[["closure_file"]],
    shock_file = shock_file,
    solution_method = "Gragg",
    matrix_method = "SBBD",
    n_subintervals = 1,
    n_tasks = 1
  )

  expect_s3_class(outputs, "tbl")

  # checks
  pop_check <- isTRUE(all.equal(pop,
    outputs$dat$pop[, !"Year"],
    check.attributes = FALSE,
    tolerance = 1e-6
  ))

  aoall_check <- isTRUE(all.equal(aoall,
    outputs$dat$aoall[, !"Year"],
    check.attributes = FALSE,
    tolerance = 1e-6
  ))

  afeall_check <- isTRUE(all.equal(afeall,
    outputs$dat$afeall[, !"Year"],
    check.attributes = FALSE,
    tolerance = 1e-6
  ))

  atall_check <- isTRUE(all.equal(atall,
    outputs$dat$atall[, !"Year"],
    check.attributes = FALSE,
    tolerance = 1e-6
  ))

  checks <- c(pop_check, aoall_check, afeall_check, atall_check)
  expect_all_true(checks)
})

test_that("solve_in_situ errors when model directory doesn't exist", {
  pop <- ems_uniform_shock("pop", 1)
  cmf_path <- ems_deploy(dat, model, pop)
  insitu_dir <- normalizePath(file.path(write_dir, "diff_dir"), "/", FALSE)
  GTAPDATA <- file.path(insitu_dir, "GTAPDATA.txt")
  GTAPINT <- file.path(insitu_dir, "GTAPINT.txt")
  GTAPPARM <- file.path(insitu_dir, "GTAPPARM.txt")
  GTAPSETS <- file.path(insitu_dir, "GTAPSETS.txt")
  shock_file <- list.files(insitu_dir, pattern = "shf", full.names = TRUE)

  expect_snapshot_error(solve_in_situ(
    GTAPDATA = GTAPDATA,
    GTAPINT = GTAPINT,
    GTAPSETS = GTAPSETS,
    model_file = model_files[["model_file"]],
    closure_file = model_files[["closure_file"]],
    model_dir = file.path(insitu_dir, "no_dir"),
    shock_file = shock_file,
    solution_method = "Gragg",
    matrix_method = "SBBD",
    n_subintervals = 1,
    n_tasks = 1
  ),
  variant = variant)
})

test_that("solve_in_situ errors when missing input file", {
  pop <- ems_uniform_shock("pop", 1)
  subdir <- "missing_file"
  nest_temp(subdir, write_dir)
  cmf_path <- ems_deploy(dat, model)
  
  insitu_dir <- normalizePath(file.path(write_dir, subdir), "/", FALSE)
  GTAPDATA <- file.path(insitu_dir, "GTAPDATA.txt")
  GTAPINT <- file.path(insitu_dir, "GTAPINT.txt")
  GTAPSETS <- file.path(insitu_dir, "GTAPSETS.txt")
  shock_file <- list.files(insitu_dir, pattern = "shf", full.names = TRUE)

  expect_snapshot_error(solve_in_situ(
    GTAPDATA = GTAPDATA,
    GTAPINT = GTAPINT,
    GTAPSETS = GTAPSETS,
    model_file = model_files[["model_file"]],
    closure_file = model_files[["closure_file"]],
    model_dir = insitu_dir,
    shock_file = shock_file,
    solution_method = "Gragg",
    matrix_method = "SBBD",
    n_subintervals = 1,
    n_tasks = 1
  ))
})

test_that("solve_in_situ errors when input file is without name", {
  pop <- ems_uniform_shock("pop", 1)
  subdir <- "no_name"
  nest_temp(subdir, write_dir)
  cmf_path <- ems_deploy(dat, model)
  
  insitu_dir <- normalizePath(file.path(write_dir, subdir), "/", FALSE)
  GTAPDATA <- file.path(insitu_dir, "GTAPDATA.txt")
  GTAPINT <- file.path(insitu_dir, "GTAPINT.txt")
  GTAPPARM <- file.path(insitu_dir, "GTAPPARM.txt")
  GTAPSETS <- file.path(insitu_dir, "GTAPSETS.txt")
  shock_file <- list.files(insitu_dir, pattern = "shf", full.names = TRUE)
  
  expect_snapshot_error(solve_in_situ(
    GTAPDATA = GTAPDATA,
    GTAPINT = GTAPINT,
    GTAPPARM,
    GTAPSETS = GTAPSETS,
    model_file = model_files[["model_file"]],
    closure_file = model_files[["closure_file"]],
    model_dir = insitu_dir,
    shock_file = shock_file,
    solution_method = "Gragg",
    matrix_method = "SBBD",
    n_subintervals = 1,
    n_tasks = 1
  ))
})

test_that("solve_in_situ example works", {
  pop <- ems_uniform_shock("pop", 1)
  subdir <- "example"
  nest_temp(subdir, write_dir)
  cmf_path <- ems_deploy(dat, model)
  
  insitu_dir <- normalizePath(file.path(write_dir, subdir), "/", FALSE)
  GTAPDATA <- file.path(insitu_dir, "GTAPDATA.txt")
  GTAPINT <- file.path(insitu_dir, "GTAPINT.txt")
  GTAPPARM <- file.path(insitu_dir, "GTAPPARM.txt")
  GTAPSETS <- file.path(insitu_dir, "GTAPSETS.txt")
  shock_file <- list.files(insitu_dir, pattern = "shf", full.names = TRUE)
  
  output <- solve_in_situ(
   GTAPDATA = GTAPDATA,
   GTAPPARM = GTAPPARM,
   GTAPSETS = GTAPSETS,
   GTAPINT = GTAPINT,
   model_dir = insitu_dir,
   model_file = model_files[["model_file"]],
   closure_file = model_files[["closure_file"]],
   shock_file = shock_file
   )
  
  expect_s3_class(output, "tbl_df")
})

unlink(tools::R_user_dir("teems", "cache"), recursive = TRUE)
skip_on_cran()

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "deploy")
temp_dir <- file.path(write_dir, "tmp")

if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}

dir.create(temp_dir, recursive = TRUE)
ems_option_set(
  verbose = FALSE,
  tempdir = write_dir
)
withr::defer(ems_option_reset(), teardown_env())

model <- "GTAP-RE"
model_files <- ems_example(model, write_dir)
model_file <- model_files[["model_file"]]
closure_file <- model_files[["closure_file"]]
dat <- ems_data(
  dat_input,
  par_input,
  set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  time_steps = c(0, 1, 2)
)

model <- ems_model(model_file, closure_file)
auto_model <- ems_model(model_file, closure_file, auto_omit = TRUE)

test_that("ems_deploy errors when .data is missing", {
  expect_snapshot_error(ems_deploy())
})

test_that("ems_deploy errors when model is missing", {
  expect_snapshot_error(ems_deploy(dat))
})

test_that("ems_deploy returns character path to CMF file", {
  nest_temp("deploy_test", write_dir)
  cmf_path <- ems_deploy(dat, model)
  expect_type(cmf_path, "character")
})

test_that("ems_deploy returns path to an existing CMF file", {
  nest_temp("deploy_test2", write_dir)
  cmf_path <- ems_deploy(dat, model)
  expect_true(file.exists(cmf_path))
})

test_that("ems_deploy accepts ems_swap variable swap", {
  shk <- ems_uniform_shock("qfd", 1)
  swap_in <- ems_swap("qfd")
  swap_out <- ems_swap("tfd")
  nest_temp("deploy_swap", write_dir)
  cmf_path <- ems_deploy(dat, model, shk, swap_in, swap_out)
  expect_true(file.exists(cmf_path))
})

test_that("ems_deploy accepts direct input full variable swap", {
  shk <- ems_uniform_shock("qfd", 1)
  nest_temp("deploy_swap2", write_dir)
  cmf_path <- ems_deploy(dat, model, shk, "qfd", "tfd")
  expect_true(file.exists(cmf_path))
})

test_that("ems_deploy accepts mixed direct input ems_swap full variable swap", {
  shk <- ems_uniform_shock("qfd", 1)
  swap_in <- ems_swap("yp")
  swap_out <- ems_swap("dppriv")
  nest_temp("deploy_swap3", write_dir)
  cmf_path <- ems_deploy(dat, model, shk, list(swap_in, "qfd"), list(swap_out, "tfd"))
  expect_true(file.exists(cmf_path))
})

test_that("ems_deploy accepts mixed direct input ems_swap partial variable swap", {
  shk <- ems_uniform_shock("qfd", 1)
  swap_in <- ems_swap("yp", REGr = "row")
  swap_out <- ems_swap("dppriv", REGr = "row")
  nest_temp("deploy_swap4", write_dir)
  cmf_path <- ems_deploy(dat, model, shk, list(swap_in, "qfd"), list(swap_out, "tfd"))
  expect_true(file.exists(cmf_path))
})

test_that("ems_deploy errors when invalid variable provided for swap-in", {
  nest_temp("invalid_swap", write_dir)
  expect_snapshot_error(
    ems_deploy(dat, model, swap_in = "not_a_var", swap_out = "tfd")
  )
})

test_that("ems_deploy errors when invalid variable provided for swap-out", {
  nest_temp("invalid_swap2", write_dir)
  expect_snapshot_error(
    ems_deploy(dat, model, swap_in = "qfd", swap_out = "not_a_var")
  )
})

test_that("ems_deploy errors when shock_file and shock are both provided", {
  shk <- ems_uniform_shock("pop", 1)
  nest_temp("deploy_shk_file", write_dir)
  expect_snapshot_error(
    ems_deploy(dat, model, shk, shock_file = "fake.shf")
  )
})

test_that("ems_deploy errors when read-in headers not present in data", {
  mod_data <- ems_data(
    dat_input,
    par_input,
    set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2)
  )

  mod_data <- mod_data[!names(mod_data) %in% "SAVE"]
  nest_temp("deploy_missing_hdr", write_dir)
  expect_snapshot_error(
    ems_deploy(mod_data, model)
  )
})

test_that("ems_deploy errors when read-in headers are missing mapping", {
  mod_data <- ems_data(
    dat_input,
    par_input,
    set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2)
  )

  mod_data <- mod_data[!names(mod_data) %in% "REG"]
  nest_temp("deploy_missing_map", write_dir)
  expect_snapshot_error(ems_deploy(mod_data, model))
})

test_that("ems_deploy errors when timesteps provided to static model", {
  write_dir <- file.path(write_dir, "gtapv7")
  dir.create(write_dir, recursive = TRUE, showWarnings = FALSE)
  model_files <- ems_example("GTAPv7", write_dir)
  model_file <- model_files[["model_file"]]
  closure_file <- model_files[["closure_file"]]
  model <- ems_model(model_file, closure_file)
  nest_temp("deploy_static_ts", write_dir)
  expect_snapshot_error(ems_deploy(dat, model))
})

test_that("ems_deploy errors when timesteps not provided to a dynamic model", {
  static_data <- ems_data(
    dat_input,
    par_input,
    set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  )
  nest_temp("deploy_dynamic_no_ts", write_dir)
  expect_snapshot_error(ems_deploy(static_data, model))
})

test_that("ems_deploy errors when set-calculated number of entries does not match a finalized data header", {
  mod_data <- ems_data(
    dat_input,
    par_input,
    set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2)
  )

  mod_data$REG[.N, mapping := "row"]
  nest_temp("deploy_set_mismatch", write_dir)
  expect_snapshot_error(ems_deploy(mod_data, model))
})

test_that("ems_deploy errors when aggregated inputs are incomplete", {
  mod_data <- ems_data(
    dat_input,
    par_input,
    set_input,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg",
    time_steps = c(0, 1, 2)
  )

  SAVE <- mod_data$SAVE[!.N, ]
  SAVE$ALLTIMEt <- 0
  colnames(SAVE)[1] <- "REGr"
  model <- ems_model(model_file, closure_file, SAVE = SAVE)
  nest_temp("deploy_incomplete", write_dir)
  expect_snapshot_error(ems_deploy(mod_data, model))
})

test_that("ems_deploy accepts a shock file", {
  shock <- "Shock pfactwld(ALLTIME) = uniform 1;\n"
  temp <- tempfile(tmpdir = temp_dir, fileext = ".shf")
  cat(shock, file = temp)
  nest_temp("shock_file", write_dir)
  cmf_path <- ems_deploy(dat, model, shock_file = temp)
  outputs <- ems_solve(cmf_path)
  expect_all_true(outputs$dat$pfactwld$Value == 1)
})

test_that("ems_deploy examples work", {
  # The following examples require input data. See
  # https://teemsphere.github.io/ to get started.

  # Uniform shock applied to full variable with value 1
  shock <- ems_uniform_shock("qfd", value = 1)

  # Full variable swap, qfd for tfd
  cmf_path <- ems_deploy(
    .data = dat,
    model = model,
    shock = shock,
    swap_in = "qfd",
    swap_out = "tfd"
  )

  expect_true(is.character(cmf_path))
  # Partial variable swap (element "row" of set REG with index r)
  yp_row <- ems_swap("yp", REGr = "row")
  dppriv_row <- ems_swap("dppriv", REGr = "row")

  # Swap part of yp for part of dppriv; qfd for tfd
  subdir <- "examples"
  nest_temp(subdir, write_dir)
  cmf_path <- ems_deploy(
   .data = dat,
   model = model,
   shock = shock,
   swap_in = list(yp_row, "qfd"),
   swap_out = list(dppriv_row, "tfd")
  )
  expect_true(is.character(cmf_path))
})


test_that("auto_omit drops unshocked exogenous variables (roadmap 6.2)", {
  nest_temp("deploy_auto_omit", write_dir)
  expect_snapshot(cmf_path <- ems_deploy(dat, auto_model))

  condense <- readRDS(file.path(dirname(cmf_path), "metadata.rds"))$condense
  expect_true(condense$n_auto_omit > 0L)
  expect_identical(condense$n_omit, condense$n_auto_omit)
  # omission removes exogenous columns only: the solved system is the
  # same size as the plain deployment's
  nest_temp("deploy_auto_omit_plain", write_dir)
  plain_cmf <- ems_deploy(dat, model)
  plain_meta <- readRDS(file.path(dirname(plain_cmf), "metadata.rds"))
  auto_meta <- readRDS(file.path(dirname(cmf_path), "metadata.rds"))
  expect_identical(auto_meta$system_size, plain_meta$system_size)
  expect_true(auto_meta$n_exo_ele < plain_meta$n_exo_ele)

  # omitted declarations are gone from the deployed TAB
  tab <- readLines(list.files(dirname(cmf_path), pattern = "\\.tab$", full.names = TRUE))
  expect_false(any(grepl("^Variable.*\\bpop\\b", tab)))
})

test_that("auto_omit retains shocked variables", {
  nest_temp("deploy_auto_omit_shk", write_dir)
  cmf_path <- ems_deploy(dat, auto_model, ems_uniform_shock("pop", 1))
  tab <- readLines(list.files(dirname(cmf_path), pattern = "\\.tab$", full.names = TRUE))
  expect_true(any(grepl("\\bpop\\b", tab)))
})

test_that("auto_omit is skipped when a shock file is supplied", {
  nest_temp("deploy_auto_omit_file", write_dir)
  shf <- file.path(temp_dir, "usr_shock.shf")
  writeLines("pop = 1 ;", shf)
  expect_snapshot(cmf_path <- ems_deploy(dat, auto_model, shock_file = shf))
  condense <- readRDS(file.path(dirname(cmf_path), "metadata.rds"))$condense
  expect_identical(condense$n_auto_omit, 0L)
})

unlink(tools::R_user_dir("teems", "cache"), recursive = TRUE)

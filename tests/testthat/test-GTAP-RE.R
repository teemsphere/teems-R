skip_on_cran()
ems_option_set(verbose = FALSE)
withr::defer(ems_option_reset(), teardown_env())

data_db <- c("v10", "v11", "v12")
db_inputs <- list(
  v10 = list(dat = Sys.getenv("GTAP10A_dat"),  par = Sys.getenv("GTAP10A_par"),  set = Sys.getenv("GTAP10A_set")),
  v11 = list(dat = Sys.getenv("GTAP11c_dat"),  par = Sys.getenv("GTAP11c_par"),  set = Sys.getenv("GTAP11c_set")),
  v12 = list(dat = Sys.getenv("GTAP12_dat"),  par = Sys.getenv("GTAP12_par"),  set = Sys.getenv("GTAP12_set"))
)

model <- "GTAP-RE"
model_dir <- file.path(tools::R_user_dir("teems", "cache"), model)

if (dir.exists(model_dir)) {
  unlink(model_dir, recursive = TRUE)
}

dir.create(model_dir, recursive = TRUE)

model_files <- ems_example(model, model_dir)
model_file <- model_files[["model_file"]]
closure_file <- model_files[["closure_file"]]

for (db in data_db) {
  dat_input <- db_inputs[[db]]$dat
  par_input <- db_inputs[[db]]$par
  set_input <- db_inputs[[db]]$set

  if (db %in% c("v9", "v10")) {
    v6_data <- GTAP_convert(dat_input, par_input, set_input, "GTAPv7")
    dat_input <- v6_data$dat
    par_input <- v6_data$par
    set_input <- v6_data$set
  }

  year <- switch(db,
    "v10" = 2014,
    "v11" = 2017,
    "v12" = 2023
  )

  write_dir <- file.path(tools::R_user_dir("teems", "cache"), db, model)
  
  if (dir.exists(write_dir)) {
    unlink(write_dir, recursive = TRUE)
  }
  
  dir.create(write_dir, recursive = TRUE)

  test_that(paste(db, paste(model, "null scenario")), {
    run_script(file.path(model, "null.R"), write_dir)
    expect_true(check)
    expect_true(var_check)
    expect_true(coeff_check)
  })

  test_that(paste(db, paste(model, "numeraire")), {
    run_script(file.path(model, "numeraire.R"), write_dir)
    expect_all_true(checks)
  })

  test_that(paste(db, paste(model, "full uniform")), {
    run_script(file.path(model, "full_uniform.R"), write_dir)
    expect_all_true(checks)
  })

  test_that(paste(db, paste(model, "partial uniform")), {
    run_script(file.path(model, "part_uniform.R"), write_dir)
    expect_all_true(checks)
  })

  test_that(paste(db, paste(model, "partial uniform full swap")), {
    run_script(file.path(model, "part_uniform_full_swap.R"), write_dir)
    expect_all_true(checks)
  })

  test_that(paste(db, paste(model, "partial uniform part swap")), {
    run_script(file.path(model, "part_uniform_part_swap.R"), write_dir)
    expect_all_true(checks)
  })

  test_that(paste(db, paste(model, "partial uniform explicit year")), {
    run_script(file.path(model, "part_uniform_year.R"), write_dir)
    expect_all_true(checks)
  })

  test_that(paste(db, paste(model, "partial uniform part swap mixed entry")), {
    run_script(file.path(model, "part_uniform_part_swap_mixed.R"), write_dir)
    expect_all_true(checks)
  })
  
  test_that(paste(db, paste(model, "partial uniform part swap mixed multi subset entry")), {
    run_script(file.path(model, "part_uniform_subset_swap.R"), write_dir)
    expect_all_true(checks)
  })

  test_that(paste(db, paste(model, "custom partial all dim")), {
    run_script(file.path(model, "custom_partial.R"), write_dir)
    expect_all_true(checks)
  })

  test_that(paste(db, paste(model, "custom partial 2d 1fixed")), {
    run_script(file.path(model, "custom_partial_2d_1fixed.R"), write_dir)
    expect_true(isTRUE(check))
  })

  test_that(paste(db, paste(model, "custom partial 3d 1fixed")), {
    run_script(file.path(model, "custom_partial_3d_1fixed.R"), write_dir)
    expect_true(isTRUE(check))
  })

  test_that(paste(db, paste(model, "custom partial 3d 2fixed")), {
    run_script(file.path(model, "custom_partial_3d_2fixed.R"), write_dir)
    expect_true(isTRUE(check))
  })

  test_that(paste(db, paste(model, "custom partial 4d 1fixed")), {
    run_script(file.path(model, "custom_partial_4d_1fixed.R"), write_dir)
    expect_true(isTRUE(check))
  })

  test_that(paste(db, paste(model, "custom partial 4d 2fixed")), {
    run_script(file.path(model, "custom_partial_4d_2fixed.R"), write_dir)
    expect_true(isTRUE(check))
  })

  test_that(paste(db, paste(model, "custom partial 4d 3fixed")), {
    run_script(file.path(model, "custom_partial_4d_3fixed.R"), write_dir)
    expect_true(isTRUE(check))
  })

  test_that(paste(db, paste(model, "custom partial 5d 1fixed")), {
    run_script(file.path(model, "custom_partial_5d_1fixed.R"), write_dir)
    expect_true(isTRUE(check))
  })

  test_that(paste(db, paste(model, "custom partial 5d 2fixed")), {
    run_script(file.path(model, "custom_partial_5d_2fixed.R"), write_dir)
    expect_true(isTRUE(check))
  })

  test_that(paste(db, paste(model, "custom partial 5d 3fixed")), {
    run_script(file.path(model, "custom_partial_5d_3fixed.R"), write_dir)
    expect_true(isTRUE(check))
  })

  test_that(paste(db, paste(model, "custom partial 5d k4")), {
    services <- switch(db,
      "v10" = c(
        "afs", "atp", "cmn", "cns", "crops", "dwe", "edu", "food", "hht", "ins",
        "livestock", "mnfcs", "obs", "ofi", "osg", "otp", "ros", "rsa", "trd",
        "whs", "wtp", "wtr"
      ),
      "v11" = c(
        "afs", "atp", "cmn", "cns", "crops", "dwe", "edu", "food", "hht", "ins",
        "livestock", "mnfcs", "obs", "ofi", "osg", "otp", "ros", "rsa", "trd",
        "whs", "wtp", "wtr"
      ),
      "v12" = c(
        "afs", "atp", "cmn", "cns", "crops", "dwe", "edu", "food", "hht", "ins",
        "livestock", "mnfcs", "obs", "ofi", "osg", "otp", "ros", "rsa", "trd",
        "whs", "wtp", "wtr"
      )
    )
    run_script(file.path(model, "custom_partial_5d_k4.R"), write_dir)
    expect_true(isTRUE(check))
  })

  test_that(paste(db, paste(model, "custom full all dim")), {
    run_script(file.path(model, "custom_full.R"), write_dir)
    expect_all_true(checks)
  })

  test_that(paste(db, paste(model, "custom full csv all dim")), {
    run_script(file.path(model, "custom_full_csv.R"), write_dir)
    expect_all_true(checks)
  })

  test_that(paste(db, paste(model, "custom full all dim year")), {
    run_script(file.path(model, "custom_full_year.R"), write_dir)
    expect_all_true(checks)
  })

  test_that(paste(db, paste(model, "scenario")), {
    run_script(file.path(model, "scenario.R"), write_dir)
    expect_all_true(checks)
  })
}

unlink(tools::R_user_dir("teems", "cache"), recursive = TRUE)
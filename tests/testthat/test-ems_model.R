skip_on_cran()

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "model")
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

model_files <- ems_example("GTAP-RE", write_dir)
model_file <- model_files[["model_file"]]
closure_file <- model_files[["closure_file"]]

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")

dat <- ems_data(
  dat_input,
  par_input,
  set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  time_steps = c(0, 1, 2)
)

test_that("ems_model requires both model_file and closure_file", {
  expect_snapshot_error(ems_model())
})

test_that("ems_model requires closure_file when only model_file provided", {
  expect_snapshot_error(ems_model(model_file))
})

test_that("ems_model requires model_file when only closure_file provided", {
  expect_snapshot_error(ems_model(closure_file = closure_file))
})

test_that("ems_model rejects non-character model_file", {
  expect_snapshot_error(ems_model(1, closure_file))
})

test_that("ems_model rejects non-character closure_file", {
  expect_snapshot_error(ems_model(model_file, TRUE))
  expect_snapshot_error(ems_model(model_file, 1))
})

test_that("ems_model rejects non-existent model_file file", {
  expect_snapshot_error(ems_model("not_a_file", closure_file))
})

test_that("ems_model rejects non-existent closure_file", {
  expect_snapshot_error(ems_model(model_file, "not_a_file"))
})

test_that("ems_model rejects non-character var_omit", {
  expect_snapshot_error(ems_model(model_file, closure_file, 1))
})

test_that("ems_model rejects invalid variable names in var_omit", {
  expect_snapshot_error(ems_model(model_file, closure_file, "not_a_var"))
})

test_that("ems_model rejects invalid coefficient arguments", {
  expect_snapshot_error(ems_model(model_file, closure_file, NOT_A_COEFF = 2))
})

test_that("ems_model returns valid tibble", {
  model <- ems_model(model_file, closure_file)
  expect_s3_class(model, "tbl_df")
  expect_true(all(c("tab", "type", "name", "header", "comp1", "comp2") %in% names(model)))
  expect_true(nrow(model) > 0)
  expect_true(!is.null(attr(model, "tab_file")))
})

test_that("aggregated numeric to a formula", {
  model <- ems_model(
    model_file,
    closure_file,
    KAPPA = 0.54321
  )

  expect_true(model[grepl("KAPPA", model$comp1), ]$comp2 == 0.54321)
  expect_true(grepl("0.54321", model[grepl("KAPPA", model$comp1), ]$tab))
})

test_that("invalid numeric to a formula", {
  expect_snapshot_error(ems_model(
    model_file,
    closure_file,
    KAPPA = c(0, 1)
  ))
})

test_that("aggregated numeric to a read", {
  model <- ems_model(
    model_file,
    closure_file,
    ETRAQ = -4.321
  )

  expect_true(model[grepl("ETRAQ", model$comp1), ]$comp2 == -4.321)
  expect_true(grepl("-4.321", model[grepl("ETRAQ", model$comp1), ]$tab))
})

test_that("aggregated data frame to a read", {
  set.seed(42)
  COMMc <- c("crops", "food", "livestock", "mnfcs", "svces")
  REGr <- c("usa", "chn", "row")
  ALLTIMEt <- seq(0, length(c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16)) - 1)
  SUBPAR <- expand.grid(
    COMMc = COMMc,
    REGr = REGr,
    ALLTIMEt = ALLTIMEt
  )
  SUBPAR$Value <- runif(nrow(SUBPAR))

  model <- ems_model(
    model_file,
    closure_file,
    SUBPAR = SUBPAR
  )

  expect_true(nrow(data.table::fsetdiff(
    data.table::as.data.table(SUBPAR),
    attr(model, "SUBPAR")
  )) == 0)
})

test_that("aggregated csv to a read", {
  set.seed(42)
  COMMc <- c("crops", "food", "livestock", "mnfcs", "svces")
  REGr <- c("usa", "chn", "row")
  ALLTIMEt <- seq(0, length(c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16)) - 1)

  SUBPAR <- expand.grid(
    COMMc = COMMc,
    REGr = REGr,
    ALLTIMEt = ALLTIMEt
  )
  SUBPAR$Value <- runif(nrow(SUBPAR))
  SUBPAR_csv <- file.path(temp_dir, "SUBPAR.csv")
  SUBPAR$Value <- round(SUBPAR$Value, 6)
  write.csv(SUBPAR, SUBPAR_csv, row.names = FALSE)
  model <- ems_model(
    model_file,
    closure_file,
    SUBPAR = SUBPAR_csv
  )

  expect_true(nrow(data.table::fsetdiff(
    data.table::as.data.table(SUBPAR),
    attr(model, "SUBPAR")
  )) == 0)
})

test_that("aggregated data frame to a formula", {
  set.seed(42)
  REGr <- c("usa", "chn", "row")
  ALLTIMEt <- seq(0, length(c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16)) - 1)
  CPHI <- expand.grid(
    REGr = REGr,
    ALLTIMEt = ALLTIMEt
  )

  CPHI$Value <- runif(nrow(CPHI))
  model <- ems_model(
    model_file,
    closure_file,
    CPHI = CPHI
  )

  expect_true(nrow(data.table::fsetdiff(
    data.table::as.data.table(CPHI),
    attr(model, "CPHI")
  )) == 0)
})

test_that("aggregated csv to a formula", {
  set.seed(42)
  REGr <- c("usa", "chn", "row")
  ALLTIMEt <- seq(0, length(c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16)) - 1)
  CPHI <- expand.grid(
    REGr = REGr,
    ALLTIMEt = ALLTIMEt
  )

  CPHI$Value <- runif(nrow(CPHI))
  CPHI_csv <- file.path(temp_dir, "CPHI.csv")
  CPHI$Value <- round(CPHI$Value, 2)
  write.csv(CPHI, CPHI_csv, row.names = FALSE)
  model <- ems_model(
    model_file,
    closure_file,
    CPHI = CPHI_csv
  )

  expect_true(nrow(data.table::fsetdiff(
    data.table::as.data.table(CPHI),
    attr(model, "CPHI")
  )) == 0)
})


test_that("ignored tab statement", {
  wrn_model <- write_modified_model(model_file, "POSTSIM (BEGIN) ;")
  expect_snapshot_warning(ems_model(wrn_model, closure_file))
})

test_that("invalid tab statement", {
  err_model <- write_modified_model(model_file, "OMIT  a1  a1oct  a1mar  a1_s  a2  a2mar  ;")
  expect_snapshot(ems_model(err_model, closure_file),
                  error = TRUE,
                  transform = function(lines) {
                    gsub(utils::packageVersion("teems"), "version_number", lines)
                  },)
})

test_that("invalid intertemporal header", {
  err_model <- write_modified_model(model_file, "AYRS",
    .fn = function(x, y) gsub("YEAR", y, x)
  )
  expect_snapshot_error(ems_model(err_model, closure_file))
})

test_that("valid custom intertemporal header", {
  err_model <- write_modified_model(model_file, "AYRS",
    .fn = function(x, y) gsub("YEAR", y, x)
  )
  ems_option_set(timestep_header = "AYRS")
  withr::defer(ems_option_set(timestep_header = "YEAR"))
  expect_s3_class(
    ems_model(err_model, closure_file),
    "data.frame"
  )
})

test_that("invalid read statement", {
  err_model <- write_modified_model(model_file, "Read but no file;")
  expect_snapshot_error(ems_model(err_model, closure_file))
})

test_that("invalid binary set switch statement", {
  err_model <- write_modified_model(
    model_file,
    "Set ENDWM # mobile endowments # = (all,e,ENDW:ENDOWFLAG(e,\"mobile\") ne 0);"
  )
  expect_snapshot_error(ems_model(err_model, closure_file))
})

test_that("set equality (GEMPACK manual 10.1.2.1)", {
  ok_model <- write_modified_model(model_file, "Set ENDWM2 # mobile endowments 2 # = ENDWM;")
  expect_s3_class(
    ems_model(ok_model, closure_file),
    "data.frame"
  )
})

test_that("intertemporal set equality", {
  err_model <- write_modified_model(model_file, "Set ALLTIME2 # all time copy # = ALLTIME;")
  expect_snapshot_error(ems_model(err_model, closure_file))
})

test_that("unparseable set definition", {
  err_model <- write_modified_model(model_file, "Set BADSET # bad definition # = ENDWM ENDWS;")
  expect_snapshot_error(ems_model(err_model, closure_file))
})

test_that("invalid set qualifier", {
  err_model <- write_modified_model(
    model_file,
    "Set (static) ENDTIME # End time step # size 1 (P[NTSP-1]);"
  )
  expect_snapshot_error(ems_model(err_model, closure_file))
})

test_that("multiple set operators", {
  ok_model <- write_modified_model(
    model_file,
    "Set ENDWCFS # multiple op # = ENDWC + ENDWF + ENDWS;"
  )
  expect_no_error(ems_model(ok_model, closure_file))
})

test_that("set expressions parse (GEMPACK manual 10.1.1.1)", {
  expr_model <- write_modified_model(
    model_file,
    paste(
      "Set TESTA # three-term plus # = ENDWC + ENDWF + ENDWS;",
      "Set TESTB # brackets with intersect # = (ENDW - ENDWC) INTERSECT ENDWMS;",
      'Set TESTC # union with quoted elements # = ENDWF UNION "land" UNION "capital";',
      sep = "\n"
    )
  )
  expect_no_error(ems_model(expr_model, closure_file))
})

test_that("IF in formula RHS (GEMPACK manual 11.4.6)", {
  ok_model <- write_modified_model(
    model_file,
    paste(
      "Coefficient (all,c,COMM)(all,r,REG)(all,t,ALLTIME) IFTESTA(c,r,t) # if in-set #;",
      "Formula (all,c,COMM)(all,r,REG)(all,t,ALLTIME) IFTESTA(c,r,t) = VDB(c,r,t) + IF[c in MARG, VST(c,r,t)];",
      "Coefficient (all,c,COMM)(all,r,REG)(all,t,ALLTIME) IFTESTB(c,r,t) # if comparison #;",
      "Formula (all,c,COMM)(all,r,REG)(all,t,ALLTIME) IFTESTB(c,r,t) = 1 + IF[VDB(c,r,t) gt 0, VDB(c,r,t)];",
      "Coefficient (all,r,REG)(all,t,ALLTIME) IFTESTC(r,t) # if element #;",
      'Formula (all,r,REG)(all,t,ALLTIME) IFTESTC(r,t) = IF[r="chn", VTRPROV(r,t)];',
      sep = "\n"
    )
  )
  model <- ems_model(ok_model, closure_file)
  expect_s3_class(model, "data.frame")
  # in-set and element conditions synthesize intersection sets
  expect_true(all(c("IFS1", "IFS2") %in% model$name))
})

test_that("unsupported IF placement", {
  err_model <- write_modified_model(
    model_file,
    paste(
      "Coefficient (all,r,REG)(all,t,ALLTIME) IFBAD(r,t) # bad if #;",
      "Formula (all,r,REG)(all,t,ALLTIME) IFBAD(r,t) = 2 * IF[r in REG, VTRPROV(r,t)];",
      sep = "\n"
    )
  )
  expect_snapshot_error(ems_model(err_model, closure_file))
})

test_that("unsupported IF condition", {
  err_model <- write_modified_model(
    model_file,
    paste(
      "Coefficient (all,r,REG)(all,t,ALLTIME) IFBAD(r,t) # bad if #;",
      "Formula (all,r,REG)(all,t,ALLTIME) IFBAD(r,t) = IF[VTRPROV(r,t) gt VT(t), VTRPROV(r,t)];",
      sep = "\n"
    )
  )
  expect_snapshot_error(ems_model(err_model, closure_file))
})

test_that("IF in equation", {
  err_model <- write_modified_model(
    model_file,
    paste(
      "Variable (all,r,REG)(all,t,ALLTIME) ifbad(r,t) # bad if #;",
      "Equation E_ifbad # bad if # (all,r,REG)(all,t,ALLTIME) ifbad(r,t) = IF[r in REG, yp(r,t)];",
      sep = "\n"
    )
  )
  expect_snapshot_error(ems_model(err_model, closure_file))
})

test_that("partial read statement", {
  err_model <- write_modified_model(
    model_file,
    'READ (all,i,COM) INTINP(i,"wool") FROM FILE params ; ! a partial read !'
  )
  expect_snapshot_error(ems_model(err_model, closure_file))
})

test_that("data frame input missing a set", {
  set.seed(42)
  REGr <- c("usa", "chn", "row")
  ALLTIMEt <- seq(0, length(c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16)) - 1)
  SUBPAR <- expand.grid(
    REGr = REGr,
    ALLTIMEt = ALLTIMEt
  )
  SUBPAR$Value <- runif(nrow(SUBPAR))

  expect_snapshot_error(ems_model(
    model_file,
    closure_file,
    SUBPAR = SUBPAR
  ))
})

test_that("invalid var in closure", {
  err_closure <- write_modified_closure(
    closure_file,
    "not_a_var"
  )

  expect_snapshot_error(ems_model(model_file, err_closure))
})

test_that("closure missing exo/endo spec", {
  err_closure <- readLines(closure_file)
  err_closure <- tail(err_closure, -1)
  err_file <- file.path(dirname(closure_file), "error.cls")
  writeLines(err_closure, err_file)
  expect_snapshot_error(ems_model(model_file, err_file))
})

test_that("ems_model errors when invalid closure mixed entry present preswap", {
  mod_closure <- readLines(closure_file)
  mod_closure <- mod_closure[-1]
  mod_closure <- mod_closure[mod_closure != "pop"]
  append_cls <- c("exogenous", 'pop("row",ALLTIME)', 'pop("chn",ALLTIME)', 'pop("zzz",ALLTIME)')
  mod_closure <- c(append_cls, mod_closure)
  invalid_cls <- file.path(dirname(closure_file), "invalid.cls")
  writeLines(mod_closure, invalid_cls)
  model <- ems_model(model_file, invalid_cls)
  nest_temp("invalid_closure", write_dir)
  expect_snapshot_error(ems_deploy(dat, model))
})

test_that("ems_model errors when invalid closure subset entry present preswap", {
  mod_closure <- readLines(closure_file)
  mod_closure <- mod_closure[-1]
  mod_closure <- mod_closure[mod_closure != "qe(ENDWC,REG,INITIME)"]
  append_cls <- c("exogenous", "qe(COMM,REG,INITIME)")
  mod_closure <- c(append_cls, mod_closure)
  invalid_cls <- file.path(dirname(closure_file), "invalid.cls")
  writeLines(mod_closure, invalid_cls)

  model <- ems_model(model_file, invalid_cls)
  nest_temp("invalid_closure2", write_dir)
  expect_snapshot_error(ems_deploy(dat, model))
})

test_that("ems_model errors when invalid closure pure element entry present preswap", {
  mod_closure <- readLines(closure_file)
  mod_closure <- mod_closure[-1]
  mod_closure <- mod_closure[mod_closure != "pop"]
  append_cls <- c(
    "exogenous",
    'pop("row","0")', 'pop("row","1")', 'pop("row","2")',
    'pop("chn","0")', 'pop("chn","1")', 'pop("chn","2")',
    'pop("usa","0")', 'pop("usa","1")', 'pop("zzz","2")'
  )
  mod_closure <- c(append_cls, mod_closure)
  invalid_cls <- file.path(dirname(closure_file), "invalid.cls")
  writeLines(mod_closure, invalid_cls)

  model <- ems_model(model_file, invalid_cls)
  nest_temp("invalid_closure3", write_dir)
  expect_snapshot_error(ems_deploy(dat, model))
})

test_that("ems_model errors when duplicate closure entry present preswap", {
  mod_closure <- readLines(closure_file)
  mod_closure <- mod_closure[-1]
  append_cls <- c("exogenous", "pop")
  mod_closure <- c(append_cls, mod_closure)
  invalid_cls <- file.path(dirname(closure_file), "invalid.cls")
  writeLines(mod_closure, invalid_cls)

  model <- ems_model(model_file, invalid_cls)
  nest_temp("invalid_closure4", write_dir)
  expect_snapshot_error(ems_deploy(dat, model))
})

test_that("ems_model errors dots passed without names", {
  expect_snapshot_error(
    ems_model(model_file,
      closure_file,
      var_omit = NULL,
      1
    )
  )
})

test_that("ems_model examples run", {
  # Simple static model retrieval and load
  GTAPv7 <- ems_example("GTAPv7", write_dir)
  model <- ems_model(GTAPv7[["model_file"]], GTAPv7[["closure_file"]])
  
  # Retrieve intertemporal model
  GTAP_RE <- ems_example("GTAP-RE", write_dir)
  expect_s3_class(model, "tbl_df")
  # Construct data frame
  sectors <- c("crops", "food", "livestock", "mnfcs", "svces")
  regions <- c("usa", "chn", "row")
  time_steps <- 0:5

  SUBPAR <- expand.grid(
    COMMc = sectors,
    REGr = regions,
    ALLTIMEt = time_steps
  )
  SUBPAR$Value <- runif(nrow(SUBPAR))
  # Model load with:
  # 1) variable omission
  # 2) uniform numeric value applied to KAPPA coefficient
  # 3) heterogeneous values allocated to SUBPAR via data frame
  model <- ems_model(
    model_file = GTAP_RE[["model_file"]],
    closure_file = GTAP_RE[["closure_file"]],
    var_omit = c("atall", "avaall", "tfe", "tfm", "tgd", "tgm", "tid", "tim"),
    KAPPA = 0.03,
    SUBPAR = SUBPAR
  )
  expect_s3_class(model, "tbl_df")
})

unlink(tools::R_user_dir("teems", "cache"), recursive = TRUE)
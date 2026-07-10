# load GTAP HAR files, apply set mappings, and aggregate data
dat <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg"
)


# parse the model Tablo file and load the closure
model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

# define a uniform shock on a subset of qfd elements
partial <- ems_uniform_shock(
  var = "qfd",
  REGr = "usa",
  ACTSa = "crops",
  value = -0.5
)

# validate inputs, write solver files, with full variable swaps passed as strings
cmf_path <- ems_deploy(
  .data = dat,
  model = model,
  shock = partial,
  swap_in = "qfd",
  swap_out = "tfd"
)

# run the Docker-based solver and parse results
outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "LU",
  solution_method = "Johansen"
)

# checks
exo_shk <- outputs$dat$qfd[REGr == "usa" & ACTSa == "crops"]$Value == -0.5
exo_null <- outputs$dat$qfd[!(REGr == "usa" & ACTSa == "crops")]$Value == 0
endo <- outputs$dat$tfd$Value != 0
qfd_len_check <- (length(exo_shk) + length(exo_null)) == nrow(outputs$dat$qfd)
tfd_len_check <- length(endo) == nrow(outputs$dat$tfd)
checks <- c(exo_shk, exo_null, endo, qfd_len_check, tfd_len_check)

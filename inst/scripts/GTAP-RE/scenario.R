time_steps <- year + c(0, 1, 2, 3)

# load GTAP HAR files, apply set mappings, and aggregate data
dat <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  time_steps = time_steps
)

# parse the model Tablo file and load the closure
model <- ems_model(
  model_file = model_file,
  closure_file = closure_file
)

# load POP at full regional resolution to use as scenario shock baseline
pop <- ems_data(
  dat_input = dat_input,
  par_input = par_input,
  set_input = set_input,
  REG = "full",
  ENDW = "labor_agg"
)$POP

# tag base year rows with reference year, create placeholder rows for future timesteps, and combine
pop$Year <- year
regions <- unique(pop$REG)
pop_traj <- expand.grid(
  REG = regions,
  Value = 0,
  Year = tail(time_steps, -1),
  stringsAsFactors = FALSE
)

pop <- rbind(pop, pop_traj)

# assign a random annual growth rate to each region
growth_rates <- data.frame(
  REG = regions,
  growth_rate = runif(length(regions), min = -0.01, max = 0.05)
)

# join growth rates and base year values, then apply compound growth to fill future timesteps
pop <- merge(pop, growth_rates, by = "REG")
base_values <- pop[pop$Year == year, c("REG", "Value")]
names(base_values)[2] <- "base_value"
pop <- merge(pop, base_values, by = "REG")

pop$Value[pop$Year > year] <-
  pop$base_value[pop$Year > year] *
    (1 + pop$growth_rate[pop$Year > year])^(pop$Year[pop$Year > year] - year)

# drop helper columns, sort by region and year, rename to model set-index convention
pop$growth_rate <- NULL
pop$base_value <- NULL
pop <- pop[order(pop$REG, pop$Year), ]
pop <- pop[, c("REG", "Year", "Value")]
colnames(pop)[1] <- "REGr"

# define an intertemporal scenario shock from absolute values; aggregated and converted to percentage changes internally
pop_trajectory <- ems_scenario_shock(
  var = "pop",
  input = pop
)

# validate inputs, write solver files, and return the CMF path
cmf_path <- ems_deploy(
  .data = dat,
  model = model,
  shock = pop_trajectory
)

# run the Docker-based solver and parse results
outputs <- ems_solve(
  cmf_path = cmf_path,
  matrix_method = "SBBD",
  solution_method = "Gragg"
)

# checks
pop$REGr <- ifelse(pop$REGr == "chn",
  "chn",
  ifelse(pop$REGr == "usa",
    "usa",
    "row"
  )
)

pop <- aggregate(Value ~ REGr + Year, data = pop, FUN = sum)

pop_out <- outputs$dat$pop[, !"ALLTIMEt"]

check_tbl <- merge(pop, pop_out, by = colnames(pop)[-ncol(pop)])

len_check <- nrow(check_tbl) == nrow(pop_out)

check_tbl$check <- ave(check_tbl$Value.x, check_tbl$REGr, FUN = function(x) {
  base <- x[1]
  ((x - base) / base) * 100
})

checks <- c(
  isTRUE(all.equal(check_tbl$Value.y, check_tbl$check, tolerance = 1e-6)),
  len_check
)

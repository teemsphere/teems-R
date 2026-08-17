#' @importFrom data.table fread data.table setnames
#' @importFrom tools file_path_sans_ext
#' 
#' @keywords internal
#' @noRd
.get_timesteps <- function(paths,
                           cmf_path,
                           timestep_header,
                           call) {
  t0 <- readRDS(paths$metadata)$reference_year
  
  model <- .process_tablo(
    tab_file = paths$tab,
    quiet = TRUE,
    call = call
  )
  
  timestep_coeff <- model$name[match(.o_timestep_header(), model$header)]

  # the timestep coefficient's values: from the solver's binary
  # coefficient dump when present, else its Write CSV
  sol_prefix <- file.path(dirname(cmf_path), "out", "variables", "bin", "sol.")
  if (.has_coefficient_dump(sol_prefix)) {
    cof <- .parse_coefficient_bins(
      sol_prefix = sol_prefix,
      coeff_names = tolower(timestep_coeff)
    )
    timesteps <- data.table::data.table(cof$xc$Value)
    data.table::setnames(timesteps, timestep_header)
  } else {
    output_paths <- .get_output_paths(cmf_path = cmf_path)$coeff
    timestep_file <- output_paths[tools::file_path_sans_ext(basename(output_paths)) == timestep_coeff]
    timesteps <- data.table::fread(timestep_file,
      skip = 1,
      col.names = timestep_header
    )
  }
  
  timesteps <- timesteps[!is.na(get(timestep_header))]
  timesteps[, let(CYRS = t0 + unlist(timesteps))]
  timesteps[, let(all_time = seq(0, nrow(timesteps) - 1))]
  
  return(timesteps)
}
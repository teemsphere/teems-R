#' @keywords internal
#' @noRd
.get_output_paths <- function(cmf_path,
                              which,
                              select = NULL,
                              call) {

  model_dir <- dirname(normalizePath(cmf_path))
  if ("tab_path" %in% names(attributes(cmf_path))) {
    tab_path <- attr(cmf_path, "tab_path")
  } else {
    tab_path <- .retrieve_cmf(
      file = "tabfile",
      cmf_path = cmf_path
    )
  }

  if (!dir.exists(paths = model_dir)) {
    .cli_action(compose_err$x_model_dir,
      action = "abort",
      call = call
    )
  }

  metadata_path <- file.path(model_dir, "metadata.rds")
  bin_csv_paths <- list.files(
    path = file.path(
      model_dir,
      "out",
      "variables",
      "bin"
    ),
    pattern = "csvs",
    full.names = TRUE
  )

  if (bin_csv_paths %=% character(0)) {
    .cli_action(compose_err$x_var_out,
      action = "abort",
      call = call
    )
  }

  if (.o_post_set_check()) {
    set_paths <- list.files(
      path = file.path(
        model_dir,
        "out",
        "sets"
      ),
      pattern = "csv",
      full.names = TRUE
    )
  } else {
    set_paths <- NULL
  }

  if (which %=% "coefficient") {
    coeff_paths <- list.files(
      path = file.path(
        model_dir,
        "out",
        "coefficients"
      ),
      pattern = "csv",
      full.names = TRUE
    )

    if (!is.null(select)) {
      return(grep(select, coeff_paths, value = TRUE))
    }
  } else {
    coeff_paths <- NULL
  }
  
  paths <- list(
    tab = tab_path,
    model = model_dir,
    metadata = metadata_path,
    bin_csv = bin_csv_paths,
    coeff = coeff_paths,
    sets = set_paths
  )

  return(paths)
}

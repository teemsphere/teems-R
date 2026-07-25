#' @keywords internal
#' @noRd
.get_output_paths <- function(cmf_path) {

  model_dir <- dirname(normalizePath(cmf_path))
  if ("tab_path" %in% names(attributes(cmf_path))) {
    tab_path <- attr(cmf_path, "tab_path")
  } else {
    tab_path <- .retrieve_cmf(
      file = "tabfile",
      cmf_path = cmf_path
    )
  }

  metadata_path <- file.path(model_dir, "metadata.rds")

  if (!file.exists(metadata_path) || !inherits(readRDS(metadata_path), "teems_metadata")) {
    metadata_path <- NULL
  }

  set_paths <- list.files(
    path = file.path(model_dir, "out", "sets"),
    pattern = "csv",
    full.names = TRUE
  )

  coeff_paths <- list.files(
    path = file.path(model_dir, "out", "coefficients"),
    pattern = "csv",
    full.names = TRUE
  )

  postsim_paths <- list.files(
    path = file.path(model_dir, "out", "postsim"),
    pattern = "csv",
    full.names = TRUE
  )
  if (length(postsim_paths) == 0) {
    postsim_paths <- NULL
  }

  list(
    tab      = tab_path,
    model    = model_dir,
    metadata = metadata_path,
    coeff    = coeff_paths,
    postsim  = postsim_paths,
    sets     = set_paths
  )
}

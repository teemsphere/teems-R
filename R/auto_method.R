#' @keywords internal
#' @noRd
# Calibrated on interleaved solver benchmarks (teems-solver
# docs/solver-reference.md §9, 2026-07 sweeps):
# - intertemporal: SBBD was fastest on every shape tested (T/R up to
#   13.7, 24k-4.36M equations), including single-task runs.
# - static: LU wins small systems; DBBD (4 tasks) overtakes LU at
#   ~2.9M equations with 33 regional blocks and already at ~1.6M with
#   163 blocks. Thresholds sit on the LU side of the measured
#   crossovers; the 1.5M-2M x <100-region band is uncalibrated and
#   deliberately falls to LU.
.auto_dbbd_size <- 2e6
.auto_dbbd_size_many_reg <- 1.5e6
.auto_dbbd_n_reg <- 100

#' @keywords internal
#' @noRd
.resolve_auto_method <- function(enable_time,
                                 n_tasks,
                                 cmf_path) {
  if (enable_time) {
    chosen <- "SBBD"
    model_type <- "intertemporal"
  } else {
    chosen <- "LU"
    model_type <- "static"
    metadata_path <- file.path(dirname(cmf_path), "metadata.rds")
    if (file.exists(metadata_path)) {
      metadata <- readRDS(metadata_path)
      system_size <- metadata$system_size
      n_reg <- metadata$n_reg %|||% 0L
      if (!is.null(system_size)) {
        dbbd_favorable <- system_size >= .auto_dbbd_size ||
          (system_size >= .auto_dbbd_size_many_reg && n_reg >= .auto_dbbd_n_reg)
        if (dbbd_favorable) {
          if (n_tasks >= 2) {
            chosen <- "DBBD"
          } else {
            .cli_action(solve_info$auto_dbbd_hint,
              action = "inform"
            )
          }
        }
      }
    }
  }
  .cli_action(solve_info$auto_method,
    action = "inform"
  )
  return(chosen)
}

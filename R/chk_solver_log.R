#' @keywords internal
#' @noRd
.check_solver_log <- function(elapsed_time,
                              solve_cmd,
                              paths,
                              call) {
  
  paths$diag_out
  model_log <- readLines(paths$diag_out)
  if (any(grepl(pattern = "singular", model_log, ignore.case = TRUE))) {
    .cli_action(solve_err$solution_sing,
                action = "abort",
                call = call)
  }
  if (any(grepl("error", model_log, ignore.case = TRUE))) {
    .cli_action(solve_err$solution_err,
                action = "abort",
                call = call)
  } 

  writeLines(solve_cmd, file.path(paths$run, "model_exec.txt"))
  .inform_diagnostics(
    elapsed_time = elapsed_time,
    model_log = model_log,
    call = call
  )
  
  return(invisible(NULL))
}
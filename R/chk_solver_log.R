#' Classify solver "Error:" lines against solver_error_map
#'
#' Each line (prefix already stripped) is matched case-insensitively
#' against the map patterns in table order; the first hit assigns the
#' class and manual section, no hit leaves NA.
#'
#' @keywords internal
#' @noRd
.map_solver_errors <- function(err_lines) {
  class <- rep(NA_character_, length(err_lines))
  manual <- rep(NA_character_, length(err_lines))
  for (i in seq_along(err_lines)) {
    hit <- which(vapply(
      solver_error_map$pattern,
      grepl,
      logical(1),
      x = err_lines[i],
      ignore.case = TRUE
    ))
    if (length(hit) > 0L) {
      class[i] <- solver_error_map$class[hit[1]]
      manual[i] <- solver_error_map$manual[hit[1]]
    }
  }
  data.frame(class = class, manual = manual, stringsAsFactors = FALSE)
}

#' @keywords internal
#' @noRd
.check_solver_log <- function(elapsed_time,
                              solve_cmd,
                              paths,
                              call) {
  model_log <- readLines(paths$diag_out)
  diag_out <- normalizePath(paths$diag_out, "/")
  paths$diag_out <- diag_out

  err_lines <- grep("Error:", model_log, value = TRUE, fixed = TRUE)
  if (length(err_lines) > 0L) {
    err_lines <- unique(sub(".*Error:\\s*", "", err_lines))
    mapped <- .map_solver_errors(err_lines)
    sel <- intersect(c("tab", "closure", "data", "numeric"), mapped$class)

    n_err <- length(err_lines)
    preview <- utils::head(err_lines, 10L)
    if (n_err > 10L) {
      preview <- c(preview, paste0("... and ", n_err - 10L, " more"))
    }
    # escape cli/glue braces in verbatim solver output
    err_preview <- paste(gsub("([{}])", "\\1\\1", preview), collapse = "\f")

    if (length(sel) > 0L) {
      sel <- sel[1]
      manual_secs <- unique(mapped$manual[mapped$class == sel & !is.na(mapped$manual)])
      msg_name <- switch(sel,
        tab = "solver_tab",
        closure = "solver_closure",
        data = "solver_data",
        numeric = "solver_numeric"
      )
      msg <- solve_err[[msg_name]]
      action <- c("abort", rep("inform", length(msg) - 1L))
      if (sel %in% c("tab", "numeric") && length(manual_secs) == 0L) {
        msg <- msg[-3]
        action <- action[-3]
      }
      .cli_action(msg,
        action = action,
        call = call
      )
    }
    .cli_action(solve_err$solution_err,
      action = "abort",
      call = call
    )
  }

  if (any(grepl(pattern = "singular", model_log, ignore.case = TRUE))) {
    .cli_action(solve_err$solution_sing,
      action = c("abort", "inform", "inform"),
      call = call
    )
  }
  if (any(grepl("error", model_log, ignore.case = TRUE))) {
    .cli_action(solve_err$solution_err,
      action = "abort",
      call = call
    )
  }

  writeLines(solve_cmd, file.path(paths$run, "model_exec.txt"))
  .inform_diagnostics(
    elapsed_time = elapsed_time,
    model_log = model_log,
    run_dir = paths$run,
    call = call
  )
  # posterity record of the run's EFFECTIVE configuration (defaults,
  # validation and forced changes applied), rendered from the
  # solver-written sol.stats.json options object
  .solve_record_append(
    run_dir = paths$run
  )

  return(invisible(NULL))
}

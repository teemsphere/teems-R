#' @importFrom purrr map_chr
#' 
#' @keywords internal
#' @noRd
.inform_diagnostics <- function(elapsed_time,
                                model_log,
                                run_dir,
                                call) {
  diagnostic_file <- file.path(run_dir, "model_diagnostics.txt")

  cat("\n", append = TRUE, file = diagnostic_file)
  cat("-- Solver log --\n\n", append = TRUE, file = diagnostic_file)
  cat(paste(model_log, collapse = "\n"), "\n", append = TRUE, file = diagnostic_file)

  if (any(grepl(pattern = "Accurate", model_log))) {
    accuracy_output <- model_log[grep("Accurate", model_log)]

    all_digits <- as.numeric(trimws(purrr::map_chr(
      strsplit(accuracy_output, "digits|none"),
      function(x) {
        x[length(x)]
      }
    )))

    total_var <- sum(all_digits)
    a4digits <- sum(all_digits[1:3])

    accurate_4 <- a4digits / total_var
    accuracy <- sprintf("%.0f%%", accurate_4 * 100)
    a_threshold <- .o_accuracy_threshold()
    elapsed_time_raw <- elapsed_time[[3]]

    if (elapsed_time_raw < 60) {
      elapsed_time_fmt <- sprintf("%.2fs", elapsed_time_raw)
    } else if (elapsed_time_raw < 3600) {
      elapsed_time_fmt <- sprintf("%dm %02ds", floor(elapsed_time_raw / 60), floor(elapsed_time_raw %% 60))
    } else {
      elapsed_time_fmt <- sprintf("%dh %02dm", floor(elapsed_time_raw / 3600), floor((elapsed_time_raw %% 3600) / 60))
    }

    elapsed_time <- elapsed_time_fmt

    .cli_action(solve_info$elapsed_time,
      action = "inform"
    )

    below_threshold <- round(accurate_4, 2) < a_threshold
    a_threshold_fmt <- sprintf("%.0f%%", a_threshold * 100)

    if (below_threshold) {
      a_threshold <- a_threshold_fmt
      .cli_action(solve_wrn$accuracy,
        action = c("warn", "inform"),
        call = call
      )
    } else if (.o_verbose()) {
      .cli_action(solve_info$accuracy,
        action = "inform"
      )
    }

    cat(
      "\n-- Run summary --\n\n",
      sprintf("Elapsed time:       %s\n", elapsed_time_fmt),
      sprintf("Accuracy (4-digit): %s\n", accuracy),
      sprintf("Accuracy threshold: %s\n", a_threshold_fmt),
      append = TRUE, file = diagnostic_file, sep = ""
    )
  }

  return(invisible(NULL))
}
#' Append the run's effective-configuration record to
#' model_diagnostics.txt (posterity/reproducibility; the CMF stays a
#' file manifest by design). Rendered from the `options` object the
#' solver writes into sol.stats.json -- RESOLVED values after
#' defaults, validation and forced changes, not what the caller
#' passed. Silently skipped against solver images that predate the
#' options record.
#'
#' @importFrom jsonlite read_json
#'
#' @keywords internal
#' @noRd
.solve_record_append <- function(run_dir) {
  diagnostic_file <- file.path(run_dir, "model_diagnostics.txt")
  stats_path <- file.path(run_dir, "out", "variables", "bin", "sol.stats.json")
  if (!file.exists(diagnostic_file) || !file.exists(stats_path)) {
    return(invisible(NULL))
  }
  stats <- tryCatch(
    jsonlite::read_json(stats_path, simplifyVector = TRUE),
    error = function(e) NULL
  )
  opt <- stats$options
  if (is.null(opt)) {
    return(invisible(NULL))
  }
  onoff <- function(x) ifelse(isTRUE(x), "on", "off")
  lines <- c(
    "",
    sprintf("-- Solve record (%s) --", format(Sys.time(), "%Y-%m-%d %H:%M:%S %Z")),
    "",
    sprintf(
      "Solution method: %s%s (subintervals %s)",
      stats$solution_method,
      if (!is.null(opt$steps)) sprintf(" (steps %s)", paste(opt$steps, collapse = ", ")) else "",
      opt$subintervals
    ),
    if (!is.null(opt$adaptive)) {
      sprintf("Adaptive stepping: %s (eps tolerance %s)", opt$adaptive, opt$eps_tolerance)
    },
    sprintf(
      "Matrix method: %s (laA %s, laDi %s, laD %s; fastrefac %s)",
      stats$matrix_method, opt$laA, opt$laDi, opt$laD, onoff(opt$fastrefac)
    ),
    sprintf(
      "Parallelism: %s MPI task(s), %s OpenMP thread(s)",
      stats$mpi_size, opt$max_threads
    ),
    if (!is.null(opt$store_precision)) {
      sprintf("Coefficient storage: %s precision", opt$store_precision)
    },
    sprintf(
      "System: %s equations, %s exogenous elements",
      stats$vecsize, stats$nexo
    ),
    sprintf(
      "Modes: assertions %s; range test initial %s, updated %s; postsim %s; gpzerodivide %s",
      opt$assertions, opt$range_test_initial, opt$range_test_updated,
      onoff(opt$postsim), onoff(opt$gpzerodivide)
    ),
    if (!is.null(opt$complementarity)) {
      cp <- opt$complementarity
      sprintf(
        paste0(
          "Complementarity: %s active component(s); approximate run %s ",
          "Euler steps (%s; redo %s, min fraction %s); accurate run %s; ",
          "state/bound errors %s"
        ),
        cp$active_components, cp$steps_approx_run,
        onoff(cp$do_approx_run), onoff(cp$redo_steps),
        cp$redo_step_min_fraction, onoff(cp$do_acc_run),
        cp$state_bound_error
      )
    }
  )
  cat(paste(unlist(lines), collapse = "\n"), "\n",
    sep = "",
    append = TRUE, file = diagnostic_file
  )
  return(invisible(NULL))
}

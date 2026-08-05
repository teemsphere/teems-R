#' @importFrom cli cli_verbatim cli_ol
#'
#' @keywords internal
#' @noRd
.construct_cmd <- function(paths,
                           terminal_run,
                           timeID,
                           n_tasks,
                           n_subintervals,
                           solmed,
                           laA,
                           laDi,
                           laD,
                           matsol,
                           steps,
                           adaptive = "no",
                           eps_tolerance = 0.1,
                           inmemory = NULL,
                           verbosity = NULL,
                           assertions = NULL,
                           range_test_initial = NULL,
                           range_test_updated = NULL,
                           postsim = NULL,
                           complementarity = NULL,
                           append_args) {
  docker_preamble <- paste(
    "docker run --rm --mount",
    paste("type=bind", paste0("src=", paths$run), "dst=/opt/teems", sep = ","),
    paste0("teems", ":", .resolve_docker_tag()),
    "/bin/bash -c"
  )
  exec_preamble <- paste(
    docker_preamble,
    '"/opt/teems-solver/lib/mpi/bin/mpiexec',
    "-n", n_tasks,
    "/opt/teems-solver/solver/teems-solver",
    "-cmdfile", paths$docker_cmf
  )

  docker_diagnostic_out <- file.path(paths$docker_run, "out", paste0("solver_out", "_", timeID, ".txt"))
  solver_param <- paste(
    "-matsol", matsol,
    if (solmed %in% c("Gragg", "Euler")) {
      paste("-step1", steps[1], "-step2", steps[2], "-step3", steps[3])
    },
    if (solmed %in% c("RK2", "RK4", "BoSha32", "DoPri54")) {
      paste("-step1", steps[1])
    },
    if (solmed %in% c("BoSha32", "DoPri54") && adaptive != "no") {
      paste("-adaptive", adaptive, "-epstol", eps_tolerance)
    },
    "-nsubints", n_subintervals,
    "-solmed", solmed,
    "-laA", laA,
    "-laDi", laDi,
    "-laD", laD,
    if (!is.null(inmemory)) {
      paste("-inmemory", as.integer(inmemory))
    },
    if (!is.null(verbosity)) {
      paste("-verbosity", as.integer(verbosity))
    },
    paste("-maxthreads", 1),
    "-nox"
  )

  # run-mode switches (solver defaults applied when absent; effective
  # values recorded in sol.stats.json and model_diagnostics.txt)
  mode_flag <- function(flag, x) {
    if (is.null(x)) {
      return(NULL)
    }
    paste(flag, c(off = 0L, warn = 1L, fatal = 2L)[[x]])
  }
  solver_param <- paste(c(
    solver_param,
    mode_flag("-assertions", assertions),
    mode_flag("-range_test_initial", range_test_initial),
    mode_flag("-range_test_updated", range_test_updated),
    if (!is.null(postsim)) paste("-postsim", as.integer(postsim))
  ), collapse = " ")

  # ch. 51 complementarity run controls (ems_complementarity();
  # solver -comp_* flags, defaults applied solver-side when absent)
  comp_flags <- .comp_cli_flags(complementarity)
  if (!is.null(comp_flags) && nzchar(comp_flags)) {
    solver_param <- paste(solver_param, comp_flags)
  }

  if (!is.null(append_args)) {
    solver_param <- paste(solver_param, paste(append_args, collapse = " "))
  }

  solver_out <- paste("2>&1 | tee", paste0(docker_diagnostic_out, "\""))
  solver_param <- paste(solver_param, solver_out)
  solve_cmd <- paste(exec_preamble, solver_param)

  if (terminal_run) {
    m_exec <- normalizePath(file.path(paths$run, "model_exec.txt"), "/", FALSE)
    cat(solve_cmd, file = m_exec)
    hsl <- "teems-solver"
    diag_out <- normalizePath(paths$diag_out, "/", FALSE)
    cmf_path <- paste0("\"", normalizePath(paths$cmf, "/"), "\"")
    .cli_action(solve_info$terminal_run,
      action = "inform",
      call = call
    )
    cli::cli_verbatim(solve_cmd, "\n")
    cli::cli_ol(solve_info$terminal_run_steps)
    return(FALSE)
  }

  cmd <- list(
    solve = solve_cmd
  )

  return(cmd)
}
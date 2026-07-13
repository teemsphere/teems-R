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
                           enable_time,
                           inmemory = NULL,
                           verbosity = NULL,
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
    if (solmed %=% "Gragg") {
      paste("-step1", steps[1], "-step2", steps[2], "-step3", steps[3])
    },
    if (any(is.element(el = matsol, set = c(0, 2, 3)))) {
      paste("-regset", "REG")
    },
    if (enable_time) {
      "-enable_time"
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
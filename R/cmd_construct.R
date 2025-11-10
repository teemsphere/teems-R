#' @keywords internal
#' @noRd
.construct_cmd <- function(paths,
                           terminal_run,
                           timeID,
                           n_tasks,
                           n_subintervals,
                           solmed,
                           nesteddbbd,
                           n_timesteps,
                           laA,
                           laDi,
                           laD,
                           matsol,
                           steps,
                           enable_time) {
  docker_preamble <- paste(
    "docker run --rm --mount",
    paste("type=bind", paste0("src=", paths$run), "dst=/opt/teems", sep = ","),
    paste0("teems", ":", .o_docker_tag()),
    "/bin/bash -c"
  )
  exec_preamble <- paste(
    docker_preamble,
    '"/opt/teems-solver/lib/mpi/bin/mpiexec',
    "-n", n_tasks,
    "/opt/teems-solver/solver/hsl",
    "-cmdfile", paths$docker_cmf
  )
  docker_diagnostic_out <- file.path(paths$docker_run, "out", paste0("solver_out", "_", timeID, ".txt"))
  solver_param <- paste(
    "-matsol", matsol,
    if (identical(x = solmed, y = "Mmid")) {
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
    "-nesteddbbd", nesteddbbd,
    if (identical(x = matsol, y = 3)) {
      paste("-ndbbd_bl_rank", n_timesteps)
    },
    "-presol", 1,
    "-laA", laA,
    "-laDi", laDi,
    "-laD", laD,
    paste("-maxthreads", 1),
    "-nox",
    "2>&1 | tee",
    paste0(docker_diagnostic_out, "\"")
  )

  solve_cmd <- paste(exec_preamble, solver_param)
  sol_parse_cmd <- paste(docker_preamble, '"make -C /opt/teems-parser"')

  if (terminal_run) {
    cat(solve_cmd, file = file.path(paths$run, "model_exec.txt"))
    hsl <- "hsl"
    diag_out <- paths$diag_out
    cmf_path <- paths$cmf
    .cli_action(solve_info$terminal_run,
      action = "inform",
      append = solve_info$terminal_run
    )
    eval(solve_info$t_run_append)
    return(FALSE)
  }

  cmd <- list(
    solve = solve_cmd,
    sol_parse = sol_parse_cmd
  )

  return(cmd)
}
#' @keywords internal
#' @noRd
.implement_solve <- function(args_list,
                             call) {

  .check_docker(
    image_name = "teems",
    call = call
  )
  timeID <- format(x = Sys.time(), "%H%M")
  paths <- .get_solver_paths(
    cmf_path = args_list$cmf_path,
    timeID = timeID,
    call = call
  )

  v <- .validate_solver_args(
    a = args_list,
    paths = paths,
    call = call,
    timeID = timeID
  )

  cmds <- .construct_cmd(
    paths = paths,
    terminal_run = v$terminal_run,
    timeID = timeID,
    n_tasks = v$n_tasks,
    steps = v$steps,
    matsol = v$matsol,
    solmed = v$solmed,
    adaptive = v$adaptive,
    eps_tolerance = v$eps_tolerance,
    max_retries = v$max_retries,
    retry_adjust = v$retry_adjust,
    n_threads = v$n_threads,
    precision = v$precision,
    n_subintervals = v$n_subintervals,
    inmemory = v$inmemory,
    verbosity = v$verbosity,
    assertions = v$assertions,
    range_test_initial = v$range_test_initial,
    range_test_updated = v$range_test_updated,
    postsim = v$postsim,
    complementarity = v$complementarity,
    laA = v$laA,
    laD = v$laD,
    laDi = v$laDi,
    extra_flags = .extra_cli_flags(v)
  )

  # need a process running in parallel, grepping output for error and then kill appropriate PID
  if (isFALSE(cmds)) {
    return(invisible(NULL))
  }

  if (isTRUE(v$pre_probe)) {
    .probe_preflight(
      cmf_path = args_list$cmf_path,
      timeID = timeID,
      call = call,
      probe = v$probe
    )
  }


  status <- 0L
  if (Sys.info()[["sysname"]] == "Windows") {
    captured <- character(0)
    elapsed_time <- system.time(captured <- system(cmds$solve, intern = TRUE))
    if (.o_verbose()) cat(captured, sep = "\n")
    status <- attr(captured, "status") %|||% 0L
  } else if (.o_verbose()) {
    elapsed_time <- system.time(status <- system(cmds$solve))
  } else {
    elapsed_time <- system.time(status <- system(cmds$solve,
      ignore.stdout = TRUE,
      ignore.stderr = TRUE
    ))
  }
  
  .check_solver_log(
    elapsed_time = elapsed_time,
    solve_cmd = cmds$solve,
    paths = paths,
    call = call,
    status = status,
    auto_decision = v$auto_decision
  )
  if (!v$suppress_outputs) {
    output <- ems_compose(cmf_path = v$cmf_path)
    return(output)
  }
  return(invisible(NULL))
}
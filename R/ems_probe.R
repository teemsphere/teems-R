#' @title Structural probe of a deployed model
#' @export
#' @description Runs the solver's structural probe (`-solmed probe`) on
#'   a deployed model: the full pre-solve pipeline (data, formulas,
#'   closure, ordering) followed by an HSL_MC79 maximum-matching /
#'   Dulmage-Mendelsohn diagnosis of the condensed Jacobian — without
#'   solving. The probe validates the closure structurally, catches the
#'   zero-flow singularity class (structurally present but zero-valued
#'   at base data), names any defective variable and equation elements,
#'   and returns the statement-level equation-system structure.
#' @param cmf_path Character length 1, path to the CMF file generated
#'   by [`ems_deploy()`].
#' @param fine Logical length 1 (default `TRUE`). Also run the fine
#'   Dulmage-Mendelsohn decomposition: the strongly-connected-component
#'   view of the system — its irreducible simultaneous cores versus the
#'   recursively solvable remainder — including the composition of the
#'   largest cores by equation and variable.
#' @param append_args Character vector (default `NULL`). Additional
#'   arguments appended to the solver command.
#' @details The probe runs on a single MPI rank; its cost is the
#'   pre-solve pipeline plus the matching (milliseconds at 10^4
#'   equations, ~a minute at 10^6). A structurally singular result does
#'   not error here — the object reports it (see
#'   [`plot.teems_probe()`] and the `defects` tibble). Use
#'   `ems_solve(pre_probe = TRUE)` to abort a solve on structural
#'   singularity instead.
#' @seealso [`ems_deploy()`] for generating `"cmf_path"`;
#'   [`plot.teems_probe()`] for the incidence, Dulmage-Mendelsohn and
#'   core visualizations; [`ems_solve()`] and its `pre_probe` argument.
#' @return A `teems_probe` object: validity verdict and rank per
#'   pattern, named defect tibble, statement-level incidence tibbles
#'   (`statements`, `incidence`), core structure (`cores`), ordering
#'   evidence (`structure`), and report paths.
#' @examples
#' \dontrun{
#' # The following examples require the teems solver to be built.
#' # See https://teemsphere.github.io/ to get started.
#'
#' probe <- ems_probe(cmf_path)
#' probe
#' plot(probe, type = "incidence")
#' plot(probe, type = "cores")
#' }
ems_probe <- function(cmf_path,
                      fine = TRUE,
                      append_args = NULL) {
  if (missing(cmf_path)) {
    .cli_missing(cmf_path)
  }
  call <- match.call()
  if (!rlang::is_logical(fine, n = 1) || is.na(fine)) {
    arg <- "fine"
    .cli_action(probe_err$x_logical,
      action = "abort",
      call = call
    )
  }
  .check_docker(
    image_name = "teems",
    call = call
  )
  timeID <- format(x = Sys.time(), "%H%M")
  paths <- .get_solver_paths(
    cmf_path = cmf_path,
    timeID = paste0(timeID, "_probe"),
    call = call
  )
  probe_cmd <- .construct_probe_cmd(
    paths = paths,
    timeID = paste0(timeID, "_probe"),
    fine = fine,
    append_args = append_args
  )
  .run_solver_cmd(probe_cmd)
  probe <- .collect_probe(
    paths = paths,
    call = call
  )
  if (!probe$valid && .o_verbose()) {
    .cli_action(probe_info$probe_defective,
      action = "inform",
      call = call
    )
  }
  return(probe)
}

#' @description The probe run is always sequential (`-n 1`): the
#'   MC79 diagnosis is serial solver-side and skips itself on more
#'   ranks. The matrix method is irrelevant to the diagnosis.
#' @keywords internal
#' @noRd
.construct_probe_cmd <- function(paths,
                                 timeID,
                                 fine,
                                 append_args = NULL) {
  docker_preamble <- paste(
    "docker run --rm --mount",
    paste("type=bind", paste0("src=", paths$run), "dst=/opt/teems", sep = ","),
    paste0("teems", ":", .resolve_docker_tag()),
    "/bin/bash -c"
  )
  exec_preamble <- paste(
    docker_preamble,
    '"/opt/teems-solver/lib/mpi/bin/mpiexec',
    "-n", 1L,
    "/opt/teems-solver/solver/teems-solver",
    "-cmdfile", paths$docker_cmf
  )
  docker_diagnostic_out <- file.path(
    paths$docker_run, "out",
    paste0("solver_out", "_", timeID, ".txt")
  )
  solver_param <- paste(
    "-matsol", 0L,
    "-nsubints", 1L,
    "-solmed", "probe",
    "-probefine", as.integer(fine),
    "-maxthreads", 1L,
    "-nox"
  )
  if (!is.null(append_args)) {
    solver_param <- paste(solver_param, paste(append_args, collapse = " "))
  }
  solver_out <- paste("2>&1 | tee", paste0(docker_diagnostic_out, "\""))
  paste(exec_preamble, solver_param, solver_out)
}

#' @keywords internal
#' @noRd
.run_solver_cmd <- function(cmd) {
  if (Sys.info()[["sysname"]] %=% "Windows") {
    captured <- character(0)
    captured <- system(cmd, intern = TRUE)
    if (.o_verbose()) cat(captured, sep = "\n")
  } else if (.o_verbose()) {
    system(cmd)
  } else {
    system(cmd,
      ignore.stdout = TRUE,
      ignore.stderr = TRUE
    )
  }
  return(invisible(NULL))
}

#' @keywords internal
#' @noRd
.collect_probe <- function(paths,
                           call) {
  # same fixed convention as ems_compose (val_compose_args)
  sol_prefix <- file.path(dirname(paths$cmf), "out", "variables", "bin", "sol")
  diag_out <- normalizePath(paths$diag_out, "/", mustWork = FALSE)
  .probe_object(
    probe_path = paste0(sol_prefix, ".probe.json"),
    stats_path = paste0(sol_prefix, ".stats.json"),
    diag_out = diag_out,
    cmf_path = paths$cmf,
    call = call
  )
}

#' @description `ems_solve(pre_probe = TRUE)` pre-flight: run the
#'   structural probe (without the fine decomposition) and abort with
#'   the named defect sets when the system is structurally singular.
#' @keywords internal
#' @noRd
.probe_preflight <- function(cmf_path,
                             timeID,
                             call) {
  paths <- .get_solver_paths(
    cmf_path = cmf_path,
    timeID = paste0(timeID, "_probe"),
    call = call
  )
  probe_cmd <- .construct_probe_cmd(
    paths = paths,
    timeID = paste0(timeID, "_probe"),
    fine = FALSE
  )
  .run_solver_cmd(probe_cmd)
  probe <- .collect_probe(
    paths = paths,
    call = call
  )
  .probe_verdict(
    probe = probe,
    cmf_path = cmf_path,
    call = call
  )
}

#' @description Abort with the named defect sets when the probe found
#'   the system structurally singular; inform otherwise.
#' @keywords internal
#' @noRd
.probe_verdict <- function(probe,
                           cmf_path,
                           call) {
  if (probe$valid) {
    n <- probe$vecsize
    .cli_action(probe_info$preflight_ok,
      action = "inform",
      call = call
    )
    return(invisible(NULL))
  }
  p <- probe$structural
  pattern <- "structural"
  if (is.null(p) || !p$defective) {
    p <- probe$realized
    pattern <- "realized"
  }
  rank <- p$rank
  n <- p$n
  n_under <- p$unmatched_cols
  n_over <- p$unmatched_rows
  under_preview <- utils::head(p$under_determined$element, 5L)
  over_preview <- utils::head(p$over_constrained$element, 5L)
  probe_path <- probe$paths$report
  .cli_action(probe_err$structurally_singular,
    action = "abort",
    call = call
  )
}

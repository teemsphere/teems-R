#' @title Configure and solve model in-situ
#' @export
#' @description Calls the
#'   \href{https://github.com/teemsphere/teems-solver}{teems-solver}
#'   Docker image directly with user-supplied input files,
#'   bypassing the [`ems_data()`] / [`ems_model()`] /
#'   [`ems_deploy()`] pipeline. All input files must be provided
#'   in their final form.
#' @return A tibble containing model output variables and
#'   coefficients. Alternatively, if `"suppress_outputs"` is
#'   `TRUE`, file path to a CMF file that may be used with
#'   [`ems_compose()`].
#' @inheritParams ems_model
#' @inheritParams ems_solve
#' @inheritParams ems_deploy
#' @param ... Named arguments corresponding to input files
#'   necessary for an in-situ model run. Names must correspond to
#'   "File" statements within the model Tablo file. Values
#'   correspond to file paths where these files are found. No
#'   checks or modifications are conducted on input files used in
#'   this manner. All model declared input files as well as
#'   `"model_file"`, `"closure_file"`, and `"shock_file"` are
#'   required for in-situ model runs.
#' @param model_dir Character of length 1, base directory where
#'   input files will be copied (if not all already present) and
#'   model outputs will be written.
#' @param shock_file A character vector length 1, path to a file
#'   with .shf extension representing a fully prepared shock
#'   file. No checks or modifications are carried out on this
#'   file.
#' @param adaptive Character length 1, adaptive step-size control
#'   for the embedded Runge-Kutta methods (`"BoSha32"`,
#'   `"DoPri54"`); default is `"no"`. See [`ems_RK()`] for the
#'   choices.
#' @param eps_tolerance Numeric length 1 (default is `0.1`), the
#'   per-step error-metric bound targeted by `adaptive` control.
#'   Ignored when `adaptive = "no"`. See [`ems_RK()`].
#' @param max_retries Integer length 1 (default `NULL`, solver
#'   default `3L`), `adaptive = "yes"` only: retries of a step
#'   failing the -100% crossing check. See [`ems_RK()`].
#' @param retry_adjust Numeric length 1 in (0, 1) (default `NULL`,
#'   solver default `0.5`), adaptive control only: the step-length
#'   multiplier applied on each retry. See [`ems_RK()`].
#' @param solver_args Named list (default `NULL`). The additional
#'   named solver arguments [`ems_solve()`] accepts through `...` —
#'   the MA48 workspace initial guesses (`laA`, `laD`, `laDi`) and
#'   the expert solver flags (`fastrefac`, `gpzerodivide`, `cntl_3`,
#'   `cntl_6`, `nsbbdblocks`, `withmc66`, `smllthreads`, `tempdir`,
#'   `nowrites`) — passed as a list because the in-situ `...` carries
#'   the input files. The Runge-Kutta step controls are formal
#'   arguments here. See the [`ems_solve()`] `...` documentation for
#'   each argument's meaning.
#' @seealso [`ems_solve()`] for the standard package-supported
#'   solver.
#' @examples
#' \dontrun{
#' # The following examples require the teems solver to be built.
#' # See https://teemsphere.github.io/ to get started.
#' 
#' solve_in_situ(
#'  GTAPDATA = "path/to/dat_file.txt",
#'  GTAPPARM = "path/to/par_file.txt",
#'  GTAPSETS = "path/to/sets_file.txt",
#'  model_dir = "existing/dir/for/model_run",
#'  model_file = "path/to/model_file.tab",
#'  closure_file = "path/to/closure_file.cls",
#'  shock_file = "path/to/shock_file.shf"
#'  )
#' }
solve_in_situ <- function(...,
                          model_dir,
                          model_file,
                          closure_file,
                          shock_file,
                          solution_method = c("Johansen", "Gragg", "Euler", "RK2", "RK4", "BoSha32", "DoPri54"),
                          matrix_method = c("auto", "LU", "DBBD", "SBBD", "NDBBD"),
                          n_subintervals = 1L,
                          steps = c(2L, 4L, 8L),
                          adaptive = c("no", "yes", "accuracy-only"),
                          eps_tolerance = 0.1,
                          max_retries = NULL,
                          retry_adjust = NULL,
                          n_tasks = 1L,
                          n_threads = 1L,
                          precision = c("single", "double"),
                          inmemory = NULL,
                          verbosity = NULL,
                          suppress_outputs = FALSE,
                          terminal_run = FALSE,
                          assertions = NULL,
                          range_test_initial = NULL,
                          range_test_updated = NULL,
                          postsim = NULL,
                          complementarity = NULL,
                          solver_args = NULL
) {
call <- match.call()
# the in-situ dots carry the input files, so the named solver extras
# ems_solve() takes through its dots arrive as a list here instead
if (!is.null(solver_args)) {
  if (!is.list(solver_args) || length(solver_args) == 0L ||
    is.null(names(solver_args)) || !all(nzchar(names(solver_args)))) {
    .cli_action(solve_err$solver_args_list,
      action = c("abort", "inform"),
      call = call
    )
  }
  unknown_args <- setdiff(names(solver_args), names(.solver_extra_args()))
  if (length(unknown_args)) {
    .cli_action(solve_err$solver_args_unknown,
      action = c("abort", "inform"),
      call = call
    )
  }
}
if (missing(model_dir)) {
  .cli_missing(model_dir)
}
if (missing(model_file)) {
  .cli_missing(model_file)
}
if (missing(closure_file)) {
  .cli_missing(closure_file)
}
if (missing(shock_file)) {
  .cli_missing(shock_file)
}
if (missing(...)) {
  .cli_action(solve_err$no_insitu_inputs,
    action = "abort",
    call = call
  )
}
input_files <- list(...)
return(.implement_solve_in_situ(
  model_dir = model_dir,
  model_file = model_file,
  closure_file = closure_file,
  input_files = input_files,
  shock_file = shock_file,
  solution_method = solution_method,
  matrix_method = matrix_method,
  n_subintervals = n_subintervals,
  steps = steps,
  adaptive = adaptive,
  eps_tolerance = eps_tolerance,
  max_retries = max_retries,
  retry_adjust = retry_adjust,
  n_tasks = n_tasks,
  n_threads = n_threads,
  precision = precision,
  inmemory = inmemory,
  verbosity = verbosity,
  suppress_outputs = suppress_outputs,
  terminal_run = terminal_run,
  assertions = assertions,
  range_test_initial = range_test_initial,
  range_test_updated = range_test_updated,
  postsim = postsim,
  complementarity = complementarity,
  solver_args = solver_args,
  call = call
))
}
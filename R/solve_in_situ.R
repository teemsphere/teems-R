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
                          n_tasks = 1L,
                          laA = 300L,
                          laD = 200L,
                          laDi = 500L,
                          inmemory = NULL,
                          verbosity = NULL,
                          suppress_outputs = FALSE,
                          terminal_run = FALSE,
                          complementarity = NULL,
                          append_args = NULL
) {
call <- match.call()
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
  n_tasks = n_tasks,
  laA = laA,
  laD = laD,
  laDi = laDi,
  inmemory = inmemory,
  verbosity = verbosity,
  suppress_outputs = suppress_outputs,
  terminal_run = terminal_run,
  complementarity = complementarity,
  append_args = append_args,
  call = call
))
}
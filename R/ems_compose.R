#' @title Compose model results into structured data objects
#' @export
#' @description `ems_compose()` retrieves and processes results
#'   from a solved model run. Data validation and consistency
#'   checks are performed during composition.
#' @return A tibble with columns "name", "label", "type", and
#'   "dat" (a list-column of data.tables) containing model
#'   results. For runs solved with an embedded Runge-Kutta method
#'   (`"BoSha32"`, `"DoPri54"`; see [`ems_solve()`]) each
#'   variable's data.table carries an additional `error_metric`
#'   column: the solver's component-by-component estimate of the
#'   cumulative solution error, `|delta| / max(1, |Value|)`.
#' @inheritParams ems_solve
#' @param which Character vector of variable length (default
#'   `"all"`). When `"all"`, all model variables are returned
#'   plus all coefficients if any coefficient output files are
#'   detected. Otherwise, a character vector of variable and/or
#'   coefficient names to retrieve — an error is raised for any
#'   name not found. Note that some coefficient names differ from
#'   their associated headers (e.g., VTWR/VTMFSD).
#' @seealso [`ems_solve()`] for solving the CGE model.
#' @examples
#' \dontrun{
#' # The following examples require that a model run has taken
#' # place. See https://teemsphere.github.io/ to get started.
#'
#' # Return all variables and coefficients
#' outputs <- ems_compose(cmf_path)
#'
#' # Return specific variables and/or coefficients by name
#' outputs <- ems_compose(cmf_path, c("qfd", "EVFP"))
#' }
ems_compose <- function(cmf_path,
                        which = "all"
) {
if (missing(cmf_path)) {
  .cli_missing(cmf_path)
}
args_list <- mget(names(formals()))
call <- match.call()
output <- .implement_compose(
  args_list = args_list,
  call = call
)
output
}

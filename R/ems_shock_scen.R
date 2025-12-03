#' Specify scenario shock
#'
#' @importFrom rlang trace_back check_dots_empty
#' @description `ems_shock.scenario()` loads scenario shocks for
#'   processing as well as conducts a series of compatibility
#'   checks. A scenario shock is one which allows for granular
#'   control over heterogenous shocks that are allocated on a
#'   tuple-specific basis, inputted as a CSV file or dataframe
#'   (or dataframe equivalent) through `"input"` in the form of
#'   absolute values. This is in contrast with `"custom"` shocks
#'   where the inputs are denominated in percentage change
#'   values.  The accepted set values within `"input"` depend on
#'   the `"var"` specified and set mappings associated with this
#'   variable but must encompass all tuples prior to any
#'   aggregation (specific to the database being used). Values
#'   will be aggregated according to the selected set
#'   aggregations and converted to percentage change form.
#' @inheritParams ems_shock
#' @param input character length 1, path to a csv file; a data
#'   frame or data frame extension (e.g., tibble, data table).
#'   Must contain a column "Year" corresponding to selected time
#'   steps and "Value" as last column representing an actual
#'   value that will be converted into a percentage-change shock
#'   based on set and time step selection.
#' @param ... Future extension.
#'
#' @method ems_shock scenario
#' @seealso [teems
#'   manual](https://teemsphere.github.io/scenario_shocks.html)
#'   for an example of the construction and use of a scenario
#'   shock.
#' @export
ems_shock.scenario <- function(var,
                               type = "scenario",
                               input,
                               ...)
{
  call <- rlang::trace_back()$call[[1]]
  rlang::check_dots_empty()
  shock <- mget(names(formals()))
  config <- .validate_shock(shock = shock,
                            call = call)
  config
}
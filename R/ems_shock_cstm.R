#' Specify custom shock
#'
#' @importFrom rlang trace_back check_dots_empty
#' @description `ems_shock.custom()` loads custom shocks for
#'   processing as well as conducts a series of compatibility
#'   checks. A custom shock is one which allows for granular
#'   control over heterogenous shocks that are allocated on a
#'   tuple-specific basis, inputted as a CSV file or dataframe
#'   (or dataframe equivalent) through `"input"`.  The accepted
#'   set values within `"input"` depend on the `"var"` specified
#'   and set mappings associated with this variable. If a custom
#'   shock is to be carried out, the output of this function is a
#'   required input to the `"shock"` argument within the
#'   [`ems_deploy()`] function.
#' 
#' @inheritParams ems_shock
#' @param input character length 1, path to a csv file; a data
#'   frame or data frame extension (e.g., tibble, data table).
#'   Must contain "Value" as last column representing
#'   percentage-change shocks to each respective tuple.
#' @param ... Future extension.
#'
#' @method ems_shock custom
#' @examples
#' REGr <- c("asia", "eit", "lam", "maf", "oecd")
#' ACTSa <- c("svces", "food", "crops", "mnfcs", "livestock")
#' ALLTIMEt <- c(0, 1, 2, 3, 4, 5)
#' 
#' aoall <- expand.grid(ACTSa = ACTSa,
#'                      REGr = REGr,
#'                      ALLTIMEt = ALLTIMEt,
#'                      stringsAsFactors = FALSE)
#' 
#' aoall <- aoall[do.call(order, aoall), ]
#' aoall$Value <- runif(nrow(aoall))
#' aoall_shk <- ems_shock(
#'  var = "aoall",
#'  type = "custom",
#'  input = aoall)
#' 
#' @export
ems_shock.custom <- function(var,
                             type = "custom",
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
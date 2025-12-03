#' Specify uniform shock
#'
#' @importFrom rlang trace_back
#' @description `ems_shock.uniform()` loads uniform shocks for
#'   processing as well as conducts a series of compatibility
#'   checks. A uniform shock is one which applies a homogenous
#'   value across all or part of a variable (using `...`).  The
#'   accepted values for `...` depend on the `"var"` specified
#'   and set mappings associated with this variable. If a uniform
#'   shock is to be carried out, the output of this function is a
#'   required input to the `"shock"` argument within the
#'   [`ems_deploy()`] function.
#'
#' @inheritParams ems_shock
#' @param value Numeric length 1, value of uniform shock.
#' @param ... One or more variable-specific key-value pairs
#'   separated by commas corresponding to the parts of a variable
#'   that will receive a uniform shock.
#'   
#' @method ems_shock uniform
#' 
#' @examples
#' # fully uniform: all variable elements receive the same shock value
#' afeall_full <- ems_shock(var = "afeall",
#'                          type = "uniform",
#'                          value = 2)
#'
#' # partially uniform: applied only to the "chn" element in set REGr (REG)
#' # Note that set designations must consiste of the concatenation of the
#' # standard set (e.g., REG) and variable-specific index (e.g., r).
#' afeall_chn <- ems_shock(var = "afeall",
#'                         type = "uniform",
#'                         REGr = "chn",
#'                         value = 2)
#'
#' @export
ems_shock.uniform <- function(var,
                              type = "uniform",
                              value,
                              ...)
{
  call <- rlang::trace_back()$call[[1]]
  if (!missing(...)) {
    subset <- list(...)
    if (length(subset) > 0 && (is.null(names(subset)) || any(names(subset) == ""))) {
      .cli_action(shk_err$uni_named_lst,
        shk_infm$uni_named_lst,
        action = "abort",
        url = shk_url$type,
        hyperlink = NULL
      )
    }
  } else {
    subset <- NULL
  }
  shock <- list(
    var = var,
    input = value,
    subset = subset
  )
  class(shock) <- c(type, class(shock))
  config <- .validate_shock(shock = shock, call = call)
  config
}
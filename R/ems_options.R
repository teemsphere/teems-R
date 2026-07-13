#' @title Set advanced package options
#' @export
#' @description `ems_option_set()` allows the user to customize
#'   advanced options.
#' @return `invisible(NULL)`, called for its side effects.
#' @param verbose Logical of length 1 (default is `TRUE`). If
#'   `FALSE`, function-specific diagnostics are silenced.
#' @param tempdir A character vector length 1. Default is what is
#'   returned by `tempdir()`.
#' @param ndigits Integer (default is `6`). Exact number of
#'   digits to the right of the decimal point to be written to
#'   file for numeric type double. This value is passed to the
#'   `format()` nsmall argument and `round()` digits argument.
#' @param accuracy_threshold Numeric length 1 (default `0.8`),
#'   converted to a percentage. 4-digit precision is compared
#'   against this threshold; a warning is generated if it is not
#'   met.
#' @param check_shock_status Logical of length 1 (default is
#'   `TRUE`). If `FALSE`, no check on shock element
#'   endogenous/exogenous status is conducted.
#' @param timestep_header A character vector length 1 (default is
#'   `"YEAR"`). Coefficient containing a numeric vector of
#'   timestep intervals. For novel intertemporal models — modify
#'   with caution.
#' @param n_timestep_header A character vector length 1 (default
#'   is `"NTSP"`). Coefficient containing a numeric vector length
#'   one with sum of timestep intervals. For novel intertemporal
#'   models — modify with caution.
#' @param full_exclude A character vector of variable length
#'   (default is `c("DREL", "DVER", "XXCR", "XXCD", "XXCP",
#'   "SLUG", "EFLG")`). Headers to fully exclude from all aspects
#'   of the model run. Failure to designate these headers
#'   properly will result in errors. Modify with caution.
#' @param docker_tag Character length 1. Docker tag specifying
#'   which `teems` image to use. When unset, the tag is selected
#'   automatically: the highest CPU capability level the host
#'   supports (e.g. `"x86-64-v3"`, probed via `ld.so`) with a
#'   matching local image `teems:<level>` (or its short form,
#'   e.g. `teems:v3`) is preferred, falling back to `"latest"`.
#' @seealso [`ems_option_get()`] for retrieving package options.
#'   [`ems_option_reset()`] for resetting package options.
#' @examples
#' # Set multiple options
#' ems_option_set(verbose = FALSE, ndigits = 8)
#' 
#' # Retrieve value of `verbose`
#' ems_option_get("verbose")
#' 
#' # Reset options to default values
#' ems_option_reset()
ems_option_set <- function(verbose = NULL,
                           tempdir = NULL,
                           ndigits = NULL,
                           accuracy_threshold = NULL,
                           check_shock_status = NULL,
                           timestep_header = NULL,
                           n_timestep_header = NULL,
                           full_exclude = NULL,
                           docker_tag = NULL) {
  call <- match.call()
  if (!is.null(verbose)) ems_options$set_verbose(verbose, call = call)
  if (!is.null(tempdir)) ems_options$set_tempdir(tempdir, call = call)
  if (!is.null(ndigits)) ems_options$set_ndigits(ndigits, call = call)
  if (!is.null(accuracy_threshold)) ems_options$set_accuracy_threshold(accuracy_threshold, call = call)
  if (!is.null(check_shock_status)) ems_options$set_check_shock_status(check_shock_status, call = call)
  if (!is.null(timestep_header)) ems_options$set_timestep_header(timestep_header, call = call)
  if (!is.null(n_timestep_header)) ems_options$set_n_timestep_header(n_timestep_header, call = call)
  if (!is.null(full_exclude)) ems_options$set_full_exclude(full_exclude, call = call)
  if (!is.null(docker_tag)) ems_options$set_docker_tag(docker_tag, call = call)
  invisible(NULL)
}

#' @title Get default package options
#' @export
#' @return If `name` is `NULL`, a named list of all current
#'   option values. Otherwise, the value of the requested option.
#' @description `ems_option_get()` returns default package
#'   options. If `name` is `NULL` (the default), all option
#'   values are returned as a list.
#' @param name Name of the option for which to retrieve a
#'   value. One of:
#'   * `NULL` Returns all option values.
#'   * `"verbose"` Logical. If `FALSE`, function-specific
#'     diagnostics are silenced.
#'   * `"tempdir"` Character. Directory used for temporary
#'     file storage during a model run.
#'   * `"ndigits"` Integer. Exact number of digits to the
#'     right of the decimal point written to file for numeric
#'     type double.
#'   * `"accuracy_threshold"` Numeric. Threshold
#'     (converted to a percentage) against which 4-digit
#'     precision is compared.
#'   * `"check_shock_status"` Logical. If `FALSE`, no
#'     check on shock element endogenous/exogenous status is
#'     conducted.
#'   * `"timestep_header"` Character. Coefficient
#'     containing a numeric vector of timestep intervals.
#'   * `"n_timestep_header"` Character. Coefficient
#'     containing a numeric vector length one with sum of
#'     timestep intervals.
#'   * `"full_exclude"` Character vector. Headers to
#'     fully exclude from all aspects of the model run.
#'   * `"docker_tag"` Character. Docker tag specifying
#'     which Docker image to use. `"latest"` when unset; note
#'     the solve command auto-selects a host-matched variant
#'     tag when the option is unset (see [`ems_option_set()`]).
#' @seealso [`ems_option_set()`] for setting package options.
#'   [`ems_option_reset()`] for resetting package options.
#' @examples
#' # Retrieve all options values
#' ems_option_get()
#' 
#' # Retrieve option value for `ndigits`
#' ems_option_get("ndigits")
#' @importFrom cli cli_abort
ems_option_get <- function(name = NULL) {
  if (is.null(name)) {
    return(ems_options$export())
  }
  
  valid <- names(formals(ems_option_set))
  if (!name %in% valid) {
    cli::cli_abort("{.arg name} must be one of {.val {valid}}, not {.val {name}}.")
  }

  switch(name,
         verbose            = ems_options$get_verbose(),
         tempdir            = ems_options$get_tempdir(),
         ndigits            = ems_options$get_ndigits(),
         accuracy_threshold = ems_options$get_accuracy_threshold(),
         check_shock_status = ems_options$get_check_shock_status(),
         timestep_header    = ems_options$get_timestep_header(),
         n_timestep_header  = ems_options$get_n_timestep_header(),
         full_exclude       = ems_options$get_full_exclude(),
         docker_tag         = ems_options$get_docker_tag()
  )
}

#' @title Reset to default package options
#' @export
#' @description `ems_option_reset()` resets all package options
#'   to default values.
#' @return `invisible(NULL)`, called for its side effects.
#' @seealso [`ems_option_set()`] for setting package options.
#'   [`ems_option_get()`] for retrieving package options.
#' @examples
#' # Set multiple options
#' ems_option_set(verbose = FALSE, ndigits = 8)
#' 
#' # Retrieve modified option value for `verbose`
#' ems_option_get("verbose")
#' 
#' # Reset options to default values
#' ems_option_reset()
#' 
#' # Retrieve default option value for `verbose`
#' ems_option_get("verbose")
ems_option_reset <- function() {
  ems_options$reset()
  invisible(NULL)
}

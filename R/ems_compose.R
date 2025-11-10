#' Compose model results
#'
#' @description `ems_compose()` retrieves and processes results
#'   from a solved model run. Results are parsed according to the
#'   specified type (variables, coefficients, or base data). Data
#'   validation and consistency checks are performed during the
#'   parsing process.
#'
#' @inheritParams ems_solve
#' 
#' @param type Character length 1, type of data to parse (default
#'   includes all). Choices:
#'   * `"variable"`: Percentage change values for model variables
#'   * `"coefficient"`: Absolute values for model coefficients
#'   * `"inputdata"`: Merged pre- and post-model basedata values
#' @param name Character vector, a subset of the selected type
#'   filtered by name.
#'
#' @importFrom rlang arg_match
#'
#' @seealso [`ems_solve()`] for running the model simulation.
#'
#' @examples
#' \dontrun{
#' inputdata <- ems_compose(cmf_path = cmf_path, type = "inputdata")
#' variables <- ems_compose(cmf_path = cmf_path, type = "variable")
#' coefficients <- ems_compose(cmf_path = cmf_path, type = "coefficient")
#' sets <- ems_compose(cmf_path = cmf_path, type = "set")
#' 
#' qfd <- ems_compose(cmf_path = cmf_path, type = "variable", name = "qfd")
#' }
#'
#' @return A list containing the parsed model results according
#'   to the specified type.
#' @export
ems_compose <- function(cmf_path,
                        which = c("all", "variable", "coefficient"),
                        name = NULL) {

  call <- match.call()
  type <- rlang::arg_match(arg = which)
  paths <- .get_output_paths(
    cmf_path = cmf_path,
    which = which,
    call = call
  )
  sets <- .check_sets(
    bin_csv_paths = paths$bin_csv,
    model_dir = paths$model,
    set_path = paths$sets,
    call = call
  )

  comp_extract <- .retrieve_tab_comp(
    tab_path = paths[["tab"]],
    which = which,
    call = call
  )

  if (any(purrr::map_lgl(sets, function(s){
    isTRUE(attr(s, "intertemporal"))}))) {
    timesteps <- .get_timesteps(paths,
      cmf_path,
      timestep_header = .o_timestep_header(),
      call = call
    )
  } else {
    timesteps <- NULL
  }

  output <- .retrieve_output(
    type = type,
    comp_extract = comp_extract,
    name = name,
    paths = paths,
    sets = sets,
    time_steps = timesteps,
    call = call
  )
  output
}
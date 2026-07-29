#' @importFrom purrr compact map_lgl
#'
#' @keywords internal
#' @noRd
.process_data <- function(i_data,
                          set_mappings,
                          call) {

  metadata <- attr(i_data, "metadata")
  # character headers in input-file order, pre-aggregation: mapping
  # (by_elements) data and the positional pairing with its domain
  # set's source elements are recovered from these at deploy
  # (.finalize_map_data)
  set_raw <- lapply(
    i_data[purrr::map_lgl(i_data, is.character)],
    function(h) tolower(trimws(unclass(h)))
  )
  i_data <- .array2DT(i_data = i_data)
  i_data <- .weight_param(
    i_data = i_data,
    data_format = metadata$data_format
  )
  
  i_data <- lapply(i_data,
    .aggregate_data,
    sets = set_mappings,
    ndigits = .o_ndigits()
  )
  i_data <- purrr::compact(i_data)
  attr(i_data, "metadata") <- metadata
  attr(i_data, "call") <- call
  attr(i_data, "set_raw") <- set_raw
  if ("time_steps" %in% names(attributes(set_mappings))) {
    attr(i_data, "time_steps") <- attr(set_mappings, "time_steps")
  }
  return(i_data)
}
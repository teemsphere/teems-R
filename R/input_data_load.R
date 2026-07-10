#' @importFrom purrr map_lgl
#'
#' @noRd
#' @keywords internal
.load_input_data <- function(dat_input,
                             par_input,
                             set_input,
                             data_call,
                             convert = FALSE) {

  if (!is.list(dat_input)) {
    dat_input <- .read_input(
      input = dat_input,
      data_type = "dat",
      attach_metadata = TRUE
    )
  }
  
  metadata <- attr(dat_input, "metadata")

  if (!is.list(par_input)) {
    par_input <- .read_input(
      input = par_input,
      data_type = "par",
      metadata = metadata
    )
  }

  if (!is.list(set_input)) {
    set_input <- .read_input(
      input = set_input,
      data_type = "set",
      metadata = metadata
    )
  }

  i_data <- c(set_input, par_input, dat_input)
  i_data <- i_data[!purrr::map_lgl(i_data, is.null)]
  
  if (!convert) {
    i_data <- i_data[!names(i_data) %in% .o_full_exclude()]
    if (any(duplicated(names(i_data)))) {
      i_data <- i_data[!duplicated(names(i_data), fromLast = TRUE)]
    }
  }
  
  .check_database_version(
    vetted = vetted_db_versions,
    provided = metadata$full_database_version,
    call = data_call
  )

  if (.o_verbose() && !convert) {
    .inform_metadata(metadata = metadata)
  }

  attr(i_data, "metadata") <- metadata
  class(i_data) <- c(metadata$data_format, class(i_data))
  return(i_data)
}
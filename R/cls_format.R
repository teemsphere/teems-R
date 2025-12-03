#' @importFrom purrr map_chr
#' @importFrom utils capture.output
#' 
#' @keywords internal
#' @noRd
.format_closure <- function(closure,
                            tab_readline = 15000) {
  closure <- purrr::map_chr(closure, 1)
  final_closure <- utils::capture.output(cat(
    "exogenous",
    closure,
    ";",
    "rest endogenous",
    ";",
    sep = "\n"
  ))

  # the TABREADLINE definition (ha_cge_global.h) limits lines to 20000 characters
  # inject new lines if closure passes this limit with 500 buffer
  if (sum(nchar(x = final_closure)) > tab_readline) {
    modified_vector <- character(0)
    current_length <- 0

    for (i in seq_along(final_closure)) {
      current_length <- current_length + nchar(x = final_closure[i])

      if (current_length >= tab_readline) {
        modified_vector <- c(modified_vector, ";", "exogenous")
        current_length <- 0
      }

      modified_vector <- c(modified_vector, final_closure[i])
    }
    final_closure <- modified_vector
  }

  class(final_closure) <- "closure"
  return(final_closure)
}
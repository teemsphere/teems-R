#' @importFrom purrr map2
#'
#' @keywords internal
#' @noRd
.advance_remainder <- function(remainder,
                               pattern,
                               fixed = TRUE) {
  remainder <- unlist(purrr::map2(
    pattern,
    remainder,
    function(p, rem) {
      # an empty pattern (e.g. the empty label "# #") advances nothing
      if (!is.na(p) && nzchar(p)) {
        trimws(sub(p, "", rem, fixed = fixed))
      } else {
        return(rem)
      }
    }
  ))

  return(remainder)
}
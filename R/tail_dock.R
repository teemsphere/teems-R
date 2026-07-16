#' @keywords internal
#' @noRd
.dock_tail <- function(string) {
  # index tails are lowercase, possibly digit-suffixed when condensation
  # freshens a sum index (e.g. MARGm1 -> MARG)
  docked_str <- sub("[a-z][a-z0-9]*$", "", string)

  return(docked_str)
}
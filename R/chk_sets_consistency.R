#' @importFrom purrr map2_lgl
#' @keywords internal
#' @noRd
.check_set_consistency <- function(bin_sets,
                                   tab_sets,
                                   call) {

  tab_set_names <- tolower(names(tab_sets))
  if (!all(tab_set_names %in% bin_sets$setname)) {
    x_sets <- toupper(setdiff(tab_set_names, bin_sets$setname))
    .cli_action(compose_err$missing_sets,
      action = "abort",
      call = call
    )
  }
  bin_sets <- subset(
    bin_sets,
    setname %in% tab_set_names
  )
  r_idx <- match(bin_sets$setname, tab_set_names)
  if (!all(purrr::map2_lgl(
    tab_sets[r_idx],
    bin_sets$ele,
    all.equal
  ))) {
    .cli_action(compose_err$set_mismatch,
      action = "abort",
      call = call
    )
  }

  return(invisible(NULL))
}
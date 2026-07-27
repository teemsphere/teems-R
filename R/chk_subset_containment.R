#' Subset containment against realized elements (S11)
#'
#' Explicit Subset statements assert containment the TAB cannot prove:
#' after aggregation both sides hold realized elements, and a wrong
#' mapping (or a wrong Subset claim) leaves subset elements missing
#' from the superset. Implied subsets from set expressions hold by
#' construction and pass through. The solver enforces the same
#' invariant (superset search in the set builders); aborting here
#' names the offending pair before the deploy round-trip.
#'
#' @keywords internal
#' @noRd
.check_subset_containment <- function(sets,
                                      call) {
  for (i in seq_len(nrow(sets))) {
    subs <- sets$subsets[[i]]
    subs <- subs[!is.na(subs)]
    if (length(subs) == 0L) {
      next
    }
    super_ele <- sets$ele[[i]]
    for (nm in subs) {
      j <- match(nm, sets$name)
      if (is.na(j)) {
        next
      }
      missing_ele <- setdiff(sets$ele[[j]], super_ele)
      if (length(missing_ele) > 0L) {
        bad_sub <- sets$name[j]
        bad_super <- sets$name[i]
        .cli_action(model_err$subset_not_contained,
          action = c("abort", "inform"),
          call = call
        )
      }
    }
  }
  return(invisible(NULL))
}

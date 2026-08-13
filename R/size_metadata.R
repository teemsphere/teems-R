#' @importFrom purrr map_dbl
#'
#' @keywords internal
#' @noRd
.count_var_elements <- function(var_extract,
                                sets) {
  sum(purrr::map_dbl(
    var_extract$ls_upper_idx,
    function(var_sets) {
      if (var_sets %=% NA) {
        return(1)
      }
      prod(lengths(with(sets$ele, mget(var_sets, ifnotfound = ""))))
    }
  ))
}

#' @keywords internal
#' @noRd
.compute_size_metadata <- function(var_extract,
                                   sets,
                                   closure) {
  n_var_ele <- .count_var_elements(
    var_extract = var_extract,
    sets = sets
  )

  n_exo_ele <- sum(purrr::map_dbl(
    closure,
    function(entry) {
      ele <- attr(entry, "ele")
      if (ele %=% NA) {
        return(1)
      }
      nrow(ele)
    }
  ))

  list(
    system_size = n_var_ele - n_exo_ele,
    n_var_ele = n_var_ele,
    n_exo_ele = n_exo_ele,
    n_reg = length(sets$ele$REG)
  )
}

# Condensation record for the deployed model. The measured cost of
# backsolving scales with the share of the uncondensed system that was
# substituted out (teems-solver ROADMAP 6.2), so the solve-time and
# probe-time advisories need the share, not just the nomination counts.
# Omission is recorded too but carries no densification cost.
#' @keywords internal
#' @noRd
.compute_condense_metadata <- function(model,
                                       sets,
                                       system_size) {
  vars <- model[model$type == "Variable" & !is.na(model$condense), ]
  omitted <- vars[vars$condense %in% "omit", ]
  backsolved <- vars[vars$condense %in% "backsolve", ]

  n_backsolve_ele <- .count_var_elements(
    var_extract = backsolved,
    sets = sets
  )
  uncondensed <- system_size + n_backsolve_ele

  list(
    n_omit = nrow(omitted),
    n_backsolve = nrow(backsolved),
    n_omit_ele = .count_var_elements(
      var_extract = omitted,
      sets = sets
    ),
    n_backsolve_ele = n_backsolve_ele,
    elimination_share = if (uncondensed > 0) n_backsolve_ele / uncondensed else 0
  )
}

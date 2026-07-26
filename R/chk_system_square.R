#' Closure count-squaring check (validation table row C4)
#'
#' The exogenous element count fixed by the closure (after swaps) must
#' leave exactly as many endogenous variable elements as the equation
#' system determines. The solver has no named check for this: a
#' non-square system surfaces downstream as an unnamed MA48 failure.
#' Equation element counts come from the (all,...) quantifier sets of
#' each retained equation; equations nominated as condensation
#' defining equations are out of the solve system. When a quantifier
#' set cannot be resolved against the loaded set elements the check is
#' skipped rather than risk a false abort.
#'
#' @keywords internal
#' @noRd
.check_system_square <- function(model,
                                 var_extract,
                                 sets,
                                 closure,
                                 size_metadata,
                                 call) {
  eqs <- model[model$type == "Equation", ]
  defining <- unique(stats::na.omit(model$condense_eq))
  if (length(defining) > 0L) {
    eqs <- eqs[!tolower(eqs$name) %in% tolower(defining), ]
  }
  if (nrow(eqs) == 0L) {
    return(invisible(NULL))
  }

  eq_sets <- regmatches(
    eqs$tab,
    gregexpr("\\(\\s*all\\s*,[^,()]+,\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*\\)", eqs$tab, ignore.case = TRUE)
  )
  eq_sets <- lapply(eq_sets, function(m) {
    toupper(sub(".*,\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*\\)$", "\\1", m))
  })
  set_sizes <- lengths(sets$ele)
  names(set_sizes) <- toupper(names(sets$ele))
  unresolved <- setdiff(unique(unlist(eq_sets)), names(set_sizes))
  if (length(unresolved) > 0L) {
    return(invisible(NULL))
  }
  n_eq_ele <- sum(vapply(
    eq_sets,
    function(s) prod(set_sizes[s]),
    numeric(1)
  ))

  n_var_ele <- size_metadata$n_var_ele
  n_exo_ele <- size_metadata$n_exo_ele
  n_endo <- n_var_ele - n_exo_ele
  if (n_endo == n_eq_ele) {
    return(invisible(NULL))
  }

  gap <- n_endo - n_eq_ele
  gap_abs <- abs(gap)
  gap_dir <- if (gap > 0) {
    "must still be exogenized"
  } else {
    "too many are exogenous (endogenize via swaps)"
  }
  candidate_txt <- .square_candidates(
    gap = gap,
    var_extract = var_extract,
    sets = sets,
    closure = closure
  )
  .cli_action(cls_err$not_square,
    action = c("abort", "inform", "inform", "inform"),
    call = call
  )
}

#' Candidate variables whose element counts could close the squaring
#' gap: exact matches first, nearest counts otherwise
#'
#' @keywords internal
#' @noRd
.square_candidates <- function(gap,
                               var_extract,
                               sets,
                               closure) {
  nelem <- vapply(
    var_extract$ls_upper_idx,
    function(var_sets) {
      if (var_sets %=% NA) {
        return(1)
      }
      prod(lengths(with(sets$ele, mget(var_sets, ifnotfound = ""))))
    },
    numeric(1)
  )
  names(nelem) <- var_extract$name

  exo_count <- rep(0, length(nelem))
  names(exo_count) <- tolower(var_extract$name)
  for (entry in closure) {
    vn <- tolower(attr(entry, "var_name"))
    ele <- attr(entry, "ele")
    n <- if (ele %=% NA) 1 else nrow(ele)
    if (vn %in% names(exo_count)) {
      exo_count[vn] <- exo_count[vn] + n
    }
  }

  if (gap > 0) {
    # need more exogenous elements: how many endogenous elements each
    # variable still has
    avail <- nelem - exo_count[tolower(names(nelem))]
    verb <- "exogenizing"
  } else {
    # too many exogenous: what each variable currently contributes
    avail <- exo_count[tolower(names(nelem))]
    names(avail) <- names(nelem)
    verb <- "endogenizing"
  }
  avail <- avail[avail > 0]
  if (length(avail) == 0L) {
    return("No single-variable candidate closes the gap.")
  }
  exact <- avail[avail == abs(gap)]
  if (length(exact) > 0L) {
    picks <- utils::head(names(exact), 5L)
    return(paste0(
      "Candidates: ", verb, " ", abs(gap),
      " element", if (abs(gap) != 1) "s", " of one of ",
      paste(picks, collapse = ", "), " closes the gap exactly."
    ))
  }
  near <- utils::head(names(avail)[order(abs(avail - abs(gap)))], 5L)
  paste0(
    "No single variable matches the gap exactly; nearest by element ",
    "count: ",
    paste(paste0(near, " (", avail[near], ")"), collapse = ", "), "."
  )
}

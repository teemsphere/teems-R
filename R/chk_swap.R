#' @importFrom purrr pluck map2
#' @importFrom rlang cnd_signal
#' 
#' @noRd
#' @keywords internal
.check_swap <- function(swap,
                        var_extract,
                        ...) {
  UseMethod(".check_swap")
}

#' @export
#' @method .check_swap list
.check_swap.list <- function(swap,
                             var_extract,
                             ...) {
  swap <- swap[[1]]
  UseMethod(".check_swap", swap)
}

#' @export
#' @method .check_swap ems
.check_swap.ems <- function(swap,
                            var_extract,
                            ...) {
  NextMethod()
}

#' @export
#' @method .check_swap default
.check_swap.default <- function(swap,
                                var_extract,
                                ...) {
  swap <- ems_swap(swap)[[1]]
  UseMethod(".check_swap", swap)
}

#' @export
#' @method .check_swap full
.check_swap.full <- function(swap,
                             var_extract,
                             sets,
                             call) {
  if (is.null(attr(swap[[1]], "call"))) {
    swap <- ems_swap(swap)
    attr(swap[[1]], "call") <- NULL
  }

  swap <- swap[[1]]
  
  if (!swap$var %in% var_extract$name) {
    var_name <- swap$var
    .cli_action(swap_err$no_var,
      action = "abort",
      call = call
    )
  }
  return(swap$var)
}

#' @export
#' @method .check_swap partial
.check_swap.partial <- function(swap,
                                var_extract,
                                sets,
                                call) {
  if (is.null(attr(swap, "call"))) {
    swap <- swap[[1]]
  }
  call <- attr(swap, "call")
  
  if (!swap$var %in% var_extract$name) {
    var_name <- swap$var
    .cli_action(swap_err$no_var,
      action = "abort",
      call = call
    )
  }

  ls_mixed <- purrr::pluck(var_extract, "ls_mixed_idx", swap$var)

  if (!all(names(swap$subset) %in% ls_mixed)) {
    non_exist_set <- setdiff(names(swap$subset), ls_mixed)
    var_name <- swap$var
    .cli_action(
      swap_err$invalid_set,
      action = c("abort", "inform", "inform"),
      call = call
    )
  }

  swap$subset <- withCallingHandlers(
    purrr::map2(
      swap$subset,
      names(swap$subset),
      function(comp, nm) {
        valid_ele <- with(sets$ele, get(.dock_tail(nm)))
        valid_subsets <- with(sets$subsets, get(.dock_tail(nm)))
        if (all(is.na(valid_subsets))) {
          vs_check <- character(0)
          valid_subsets <- "*none*"
        } else {
          vs_check <- valid_subsets
        }
        if (!comp %in% c(valid_ele, vs_check)) {
          invalid_comp <- setdiff(comp, c(valid_ele, vs_check))
          .cli_action(swap_err$invalid_comp,
            action = c("abort", "inform", "inform"),
            call = call
          )
        }
        if (comp %in% valid_ele) {
          attr(comp, "type") <- "ele"
        } else if (comp %in% valid_subsets) {
          attr(comp, "type") <- "subset"
        }
        return(comp)
      }
    ),
    purrr_error_indexed = function(err) {
      rlang::cnd_signal(err$parent)
    }
  )

  ls_upper <- purrr::pluck(var_extract, "ls_upper_idx", swap$var)
  m_mixed <- setdiff(ls_mixed, names(swap$subset))
  ls_missing_sets <- as.list(m_mixed)
  names(ls_missing_sets) <- m_mixed
  swap$subset <- c(swap$subset, ls_missing_sets)
  r_idx <- match(ls_mixed, names(swap$subset))
  swap$subset <- swap$subset[r_idx]
  swap$subset <- purrr::pmap_chr(
    list(
      names(swap$subset),
      swap$subset,
      ls_upper
    ),
    function(nm, c, u) {
      if (nm %=% c) {
        u
      } else if (attr(c, "type") %=% "ele") {
        paste0("\"", c, "\"")
      } else if (attr(c, "type") %=% "subset") {
        c
      }
    }
  )

  swap <- paste0(
    swap$var,
    "(",
    paste0(swap$subset, collapse = ","),
    ")"
  )

  return(swap)
}
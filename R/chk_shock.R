#' @keywords internal
#' @noRd
.check_shock <- function(shock,
                         var_extract,
                         sets) {
  if (is.null(names(shock))) {
    shock <- shock[[1]]
  }

  if (!shock$var %in% var_extract$name) {
    var_name <- shock$var
    call <- attr(shock, "call")
    .cli_action(
      shk_err$not_a_var,
      action = c("abort", "inform"),
      call = call
    )
  }

  .abort_condensed(
    var_name = shock$var,
    var_extract = var_extract,
    err = shk_err$condensed_var,
    call = attr(shock, "call")
  )

  UseMethod(".check_shock", shock)
}

#' @importFrom purrr pluck
#'
#' @keywords internal
#' @noRd
#' @method .check_shock uniform
#' @export
.check_shock.uniform <- function(shock,
                                 var_extract,
                                 sets) {

  if (is.null(names(shock))) {
    shock <- shock[[1]]
  }

  call <- attr(shock, "call")
  ls_mixed <- purrr::pluck(var_extract, "ls_mixed_idx", shock$var)

  if (attr(sets, "intertemporal")) {
    int_sets <- sets[which(sets$qualifier_list == "(intertemporal)"), "name"][[1]]
  } else {
    int_sets <- NULL
  }

  shock$ls_upper <- purrr::pluck(var_extract, "ls_upper_idx", shock$var)
  shock$ls_mixed <- ls_mixed

  if (!is.null(int_sets)) {
    shock <- .year2time_set(
      shk = shock,
      sets = sets,
      int_set_names = int_sets,
      call = call
    )
  }
  
  if (!is.null(shock$subset)) {
    shock_subsets <- names(shock$subset)
    if (!all(shock_subsets %in% ls_mixed)) {
      errant_set <- setdiff(shock_subsets, ls_mixed)
      l_errant_set <- length(errant_set)
      var_name <- shock$var
      .cli_action(
        shk_err$invalid_set,
        action = c("abort", rep("inform", 4)),
        call = call
      )
    }
    
    for (i in seq_along(shock$subset)) {
      ele <- shock$subset[[i]]
      ele_set <- .dock_tail(shock_subsets[[i]])
      recognized_ele <- purrr::pluck(sets, "ele", ele_set)
      recognized_ss <- purrr::pluck(sets, "subsets", ele_set)
      
      if (!ele %in% c(recognized_ele, recognized_ss)) {
        .cli_action(
          shk_err$uni_invalid_RHS,
          action = c("abort", rep("inform", 3)),
          call = call
        )
      }
      
      if (ele %in% recognized_ele) {
        attr(shock$subset[[i]], "subset") <- FALSE
      } else {
        attr(shock$subset[[i]], "subset") <- TRUE
      }
      
      
    }
    
    attr(shock, "full_var") <- FALSE
  } else {
    attr(shock, "full_var") <- TRUE
  }

  return(shock)
}

#' @importFrom data.table CJ setnames fsetdiff fsetequal
#' @importFrom utils capture.output
#'
#' @keywords internal
#' @noRd
#' @method .check_shock custom
#' @export
.check_shock.custom <- function(shock,
                                var_extract,
                                sets) {
  
  if (is.null(names(shock))) {
    shock <- shock[[1]]
  }

  call <- attr(shock, "call")

  if (attr(sets, "intertemporal")) {
    int_sets <- sets[which(sets$qualifier_list == "(intertemporal)"), "name"][[1]]
  } else {
    int_sets <- NULL
  }

  shock <- .check_cst_scen(
    shock = shock,
    var_extract = var_extract,
    int_sets = int_sets,
    call = call
  )

  shock <- .year2time_set(
    shk = shock,
    sets = sets,
    int_set_names = int_sets,
    call = call
  )

  set_ele <- with(sets$ele, mget(shock$ls_upper))
  template <- do.call(data.table::CJ, c(set_ele, sorted = FALSE))
  data.table::setnames(template, new = shock$ls_mixed)

  is_full <- data.table::fsetequal(template, shock$input[, !"Value"])

  if (!is_full) {
    if (nrow(data.table::fsetdiff(shock$input[, !"Value"], template)) %!=% 0L) {
      errant_tuples <- data.table::fsetdiff(shock$input[, !"Value"], template)
      errant_tuples <- utils::capture.output(print(errant_tuples))
      errant_tuples <- errant_tuples[-c(1, 2)]
      .cli_action(shk_err$cust_invalid_tup,
        action = c("abort", "inform"),
        call = call
      )
    }
  }

  attr(shock, "is_full") <- is_full
  return(shock)
}

#' @importFrom data.table CJ setnames fsetdiff fsetequal
#' @importFrom utils capture.output
#'
#' @keywords internal
#' @noRd
#' @method .check_shock scenario
#' @export
.check_shock.scenario <- function(shock,
                                  var_extract,
                                  sets) {
  if (is.null(names(shock))) {
    shock <- shock[[1]]
  }

  call <- attr(shock, "call")

  if (attr(sets, "intertemporal")) {
    int_sets <- sets[which(sets$qualifier_list == "(intertemporal)"), "name"][[1]]
  } else {
    int_sets <- NULL
  }

  if (is.null(int_sets)) {
    .cli_action(shk_err$scen_dynamic,
      action = c("abort", "inform"),
      call = call
    )
  }

  shock <- .check_cst_scen(
    shock = shock,
    var_extract = var_extract,
    int_sets = int_sets,
    call = call
  )

  set_ele <- with(sets$mapping, mget(shock$ls_upper))
  set_ele <- lapply(set_ele, \(s) {
    s[["origin"]]
  })

  template_shk <- do.call(data.table::CJ, c(set_ele, sorted = FALSE))
  int_col <- colnames(template_shk)[which(colnames(template_shk) %in% int_sets)]
  year_tbl <- attr(sets, "CYRS")
  r_idx <- match(template_shk[[int_col]], year_tbl$all_time)
  template_shk[[int_col]] <- year_tbl$Value[r_idx]
  data.table::setnames(template_shk, old = shock$ls_upper, new = shock$set)

  if (.o_check_shock_status()) {
    if (!data.table::fsetequal(template_shk, shock$input[, !"Value"])) {
      missing_tuples <- data.table::fsetdiff(template_shk, shock$input[, !"Value"])
      n_missing_tuples <- nrow(missing_tuples)
      missing_tuples <- utils::capture.output(print(missing_tuples))[-c(1:2)]
      .cli_action(
        shk_err$scen_missing_tup,
        action = c("abort", "inform", "inform"),
        call = call
      )
    }
  }

  shock <- .year2time_set(
    shk = shock,
    sets = sets,
    int_set_names = int_sets,
    call = call
  )

  attr(shock, "is_full") <- TRUE
  return(shock)
}
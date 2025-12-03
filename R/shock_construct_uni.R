#' @importFrom utils capture.output
#' 
#' @noRd
#' @keywords internal
#' @method .construct_shock uniform
#' @export
.construct_shock.uniform <- function(raw_shock,
                                     closure,
                                     sets,
                                     var_extract,
                                     ...) {

  if (attr(raw_shock, "full_var")) {
    if (.o_check_shock_status()) {
      if (!raw_shock$var %in% closure$full_var) {
        # if parts of a var are provided that comprise the full, not sure if caught
        # provide some endo components as examples here
        .cli_action(
          shk_err$x_full_exo,
          action = "abort",
          call = attr(raw_shock, "call")
        )
      }
    }

    if (!raw_shock$ls_upper %=% "null_set") {
      shock_LHS <- paste0(raw_shock$var, "(", paste0(raw_shock$ls_upper, collapse = ","), ")")
    } else {
      shock_LHS <- raw_shock$var
    }
  } else {
    if ("Year" %in% names(raw_shock$subset)) {
      Year <- purrr::pluck(raw_shock, "subset", "Year")
      time_set_upper <- intersect(raw_shock$ls_upper, subset(sets, qualifier_list == "(intertemporal)", name, 1))
      time_set <- raw_shock$ls_mixed[match(time_set_upper, raw_shock$ls_upper)]
      CYRS <- attr(sets, "CYRS")
      if (!Year %in% CYRS$Value) {
        CYRS <- CYRS$Value
        .cli_action(
          shk_err$uni_invalid_year,
          action = "abort",
          call = attr(raw_shock, "call")
        )
      }
      v_idx <- match(Year, CYRS$Value)
      purrr::pluck(raw_shock, "subset", "Year") <- purrr::pluck(sets, "ele", time_set_upper, v_idx)
      names(raw_shock$subset) <- sub("Year", time_set, names(raw_shock$subset))
    }

    mixed_ss <- names(raw_shock$subset)

    withCallingHandlers(
      purrr::map2(
        raw_shock$subset,
        mixed_ss,
        function(ele, ele_set) {
          ele_set <- .dock_tail(ele_set)
          recognized_ele <- purrr::pluck(sets, "ele", ele_set)
          if (!ele %in% recognized_ele) {
            .cli_action(
              shk_err$uni_invalid_ele,
              action = c("abort", "inform"),
              call = attr(raw_shock, "call")
            )
          }
        }
      ),
      purrr_error_indexed = function(err) {
        rlang::cnd_signal(err$parent)
      }
    )

    r_idx <- match(mixed_ss, raw_shock$ls_mixed)
    if (!raw_shock$ls_upper %=% NA) {
      shock_LHS <- raw_shock$ls_upper
      shock_LHS[r_idx] <- paste0('"', raw_shock$subset, '"')
      shock_LHS <- paste0(raw_shock$var, "(", paste0(shock_LHS, collapse = ","), ")")
    } else {
      shock_LHS <- raw_shock$var
    }

    if (.o_check_shock_status()) {
      classified_shk <- .classify_cls(
        closure = shock_LHS,
        sets = sets
      )[[1]]

      expanded_shk <- .exp_cls_entry(
        cls_entry = classified_shk,
        var_extract = var_extract,
        sets = sets$ele
      )

      check <- closure[purrr::map_lgl(closure, function(c) {
        attr(c, "var_name") %=% raw_shock$var
      })]
      
      if (length(check) %=% 0L) {
        .cli_action(shk_err$x_full_exo_part,
                    action = "abort",
                    call = attr(raw_shock, "call"))
      }

      if (!attr(check[[1]], "ele") %=% NA) {
        check <- data.table::rbindlist(purrr::map(check, attr, "ele"))

        if (!nrow(data.table::fsetdiff(attr(expanded_shk, "ele"), check)) %=% 0L) {
          errant_tup <- data.table::fsetdiff(attr(expanded_shk, "ele"), check)
          errant_tup <- utils::capture.output(print(errant_tup))[-c(1, 2)]
          .cli_action(
            shk_err$x_part_exo,
            action = "abort",
            call = attr(raw_shock, "call")
          )
        }
      }
    }
  }

  shock_RHS <- paste("=", "uniform", paste0(raw_shock$input, ";", "\n"))
  shock <- list(shock = paste("Shock", shock_LHS, shock_RHS))
  shock <- structure(shock,
    class = class(raw_shock),
    full_var = attr(raw_shock, "full_var")
  )

  shock <- list(shock)
  return(shock)
}
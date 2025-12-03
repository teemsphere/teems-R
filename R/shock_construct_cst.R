#' @importFrom data.table fread as.data.table setnames CJ fsetequal fsetdiff rbindlist
#' @importFrom purrr pluck list_flatten
#' @importFrom tibble tibble
#' @importFrom utils capture.output
#' 
#' @noRd
#' @keywords internal
#' 
#' @method .construct_shock custom
#' @export
.construct_shock.custom <- function(raw_shock,
                                    closure,
                                    sets,
                                    ...) {

  ndigits <- .o_ndigits()

  value <- raw_shock$input

  if ("Year" %in% raw_shock$set) {
    updated <- .year2time_set(raw_shock = raw_shock,
                              sets = sets,
                              value = value,
                              call = call)
    value <- updated$value
    raw_shock <- updated$raw_shock
  }

  set_ele <- with(sets$ele, mget(raw_shock$ls_upper))
  template_shk <- do.call(data.table::CJ, c(set_ele, sorted = FALSE))
  data.table::setnames(template_shk, new = raw_shock$ls_mixed)

  if (data.table::fsetequal(template_shk, value[, !"Value"])) {
    if (.o_check_shock_status()) {
      # if closure entries have been split and the shock is over the full set then this will fail
      if (!inherits(closure[closure == raw_shock$var][[1]], "full")) {
        .cli_action(
          shk_err$x_full_exo,
          action = "abort",
          call = attr(raw_shock, "call")
        )
      }
    }

    value[, Value := format(
      round(Value, ndigits),
      trim = TRUE,
      nsmall = ndigits,
      scientific = FALSE
    )]

    data.table::setorder(value)
    new_classes <- append(class(raw_shock), "full_set", after = 1)
    lead <- paste(
      "Shock",
      paste0(raw_shock$var, "(", paste0(raw_shock$ls_upper, collapse = ","), ")"),
      "="
    )
    shock <- structure(list(dt = value),
      lead = lead,
      dim_sizes = with(sets$ele, lengths(mget(raw_shock$ls_upper))),
      class = new_classes
    )
    shock <- list(shock)
  } else {
    if (!nrow(data.table::fsetdiff(value[, !"Value"], template_shk)) %=% 0L) {
      errant_tuples <- data.table::fsetdiff(value[, !"Value"], template_shk)
      errant_tuples <- utils::capture.output(print(errant_tuples))
      errant_tuples <- errant_tuples[-c(1, 2)]
      .cli_action(shk_err$cust_invalid_tup,
        action = "abort",
        call = attr(raw_shock, "call")
      )
    }

    # Year to time conversion here
    if (.o_check_shock_status()) {
      cls_entries <- closure[purrr::map_lgl(closure, function(c) {
        attr(c, "var_name") == raw_shock$var
      })]
      # again here some complication can occur from split var entries
      if (!inherits(cls_entries[[1]], "full")) {
        all_exo_parts <- data.table::rbindlist(
          purrr::map(cls_entries, attr, "ele")
        )

        key_names <- names(value[, !"Value"])
        data.table::setnames(all_exo_parts, new = key_names)
        if (!nrow(data.table::fsetdiff(value[, !"Value"], all_exo_parts)) %=% 0L) {
          x_exo_parts <- data.table::fsetdiff(
            value[, ..key_names],
            all_exo_parts
          )
          x_exo_parts <- trimws(utils::capture.output(print(x_exo_parts)))
          x_exo_parts <- x_exo_parts[-c(1, 2)]

          .cli_action(shk_err$cust_endo_tup,
            action = "abort",
            call = attr(raw_shock, "call")
          )
        }
      }
    }

    set_combn <- list()
    key_cols <- names(set_ele)
    for (size in 1:length(key_cols)) {
      col_combinations <- combn(key_cols,
        m = size,
        simplify = FALSE
      )
      set_combn <- c(set_combn, col_combinations)
    }

    set_combn <- set_combn[-length(set_combn)]

    shock <- list()
    shk_idx <- 1

    for (sel_comb in unique(set_combn)) {
      cycle_ele <- with(
        data = set_ele,
        expr = mget(sel_comb)
      )

      ele_combn <- do.call(data.table::CJ, cycle_ele)
      for (sel_ele in 1:nrow(ele_combn)) {
        ele <- unlist(ele_combn[sel_ele, ])
        col_idx <- match(sel_comb, names(set_ele))
        col <- colnames(value)[col_idx]
        # get template subset
        dynamic_filter <- purrr::map2_chr(col, ele, function(c, e) {
          paste(c, "==", paste0("\"", e, "\""))
        })

        if (length(dynamic_filter) > 1) {
          dynamic_filter <- paste(dynamic_filter, collapse = " & ")
        }

        ss_tmpl <- template_shk[eval(parse(text = dynamic_filter))]
        ss_value <- value[eval(parse(text = dynamic_filter))]

        if (data.table::fsetequal(ss_tmpl, ss_value[, !"Value"])) {
          shk <- raw_shock
          shk$value <- ss_value
          value <- data.table::fsetdiff(value, ss_value)
          ss <- raw_shock$ls_upper
          ss[match(names(ele), raw_shock$ls_upper)] <- paste0("\"", ele, "\"")
          shk$value <- shk$value[, -c(col), with = FALSE]
          data.table::setorder(shk$value)
          shk$value[, Value := format(
            round(Value, ndigits),
            trim = TRUE,
            nsmall = ndigits,
            scientific = FALSE
          )]

          new_classes <- append(class(raw_shock), "full_set", after = 1)
          lead <- paste(
            "Shock",
            paste0(raw_shock$var, "(", paste0(ss, collapse = ","), ")"),
            "="
          )

          dim_sizes <- with(
            sets$ele,
            lengths(mget(.dock_tail(colnames(shk$value)[!colnames(shk$value) %in% "Value"])))
          )
          shock[[shk_idx]] <- structure(list(dt = shk$value),
            lead = lead,
            dim_sizes = dim_sizes,
            class = new_classes
          )

          shk_idx <- shk_idx + 1
          if (nrow(value) %=% 0L) {
            break
          }
        }
      }
    }

    if (!nrow(value) %=% 0L) {
      value[, Value := format(
        round(Value, ndigits),
        trim = TRUE,
        nsmall = ndigits,
        scientific = FALSE
      )]

      value <- paste(
        "Shock",
        paste0(raw_shock$var,
        "(",
        paste(apply(value[, !"Value"], 2, function(e) {
          paste0("\"", e, "\"")
        }), collapse = ","),
        ")"),
        "=",
        paste0(value$Value,
        ";")
      )
      new_classes <- append(class(raw_shock), "ele", after = 1)
      ele_shocks <- structure(list(ele = value),
        class = new_classes
      )

      shock <- c(shock, list(ele_shocks))
    }
  }

  return(shock)
}
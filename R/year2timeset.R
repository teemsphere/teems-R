#' @importFrom purrr pluck
#' @importFrom data.table setnames
#' @importFrom utils capture.output
#' 
#' @noRd
#' 
#' @keywords internal
.year2time_set <- function(raw_shock,
                           sets,
                           value,
                           call) {
  time_set_upper <- intersect(
    raw_shock$ls_upper,
    subset(sets, qualifier_list == "(intertemporal)", name, 1)
  )
  time_set <- raw_shock$ls_mixed[match(time_set_upper, raw_shock$ls_upper)]
  CYRS <- attr(sets, "CYRS")
  if (!all(value$Year %in% CYRS$Value)) {
    errant_year_tuples <- value[!value$Year %in% CYRS$Value]
    n_errant_year_tuples <- nrow(errant_year_tuples)
    errant_year_tuples <- utils::capture.output(print(errant_year_tuples))[-c(1, 2)]
    .cli_action(
      shk_err$cust_invalid_year,
      action = "abort",
      call = attr(raw_shock, "call")
    )
  }
  time_set_ele <- purrr::pluck(sets, "ele", time_set_upper)
  r_idx <- match(value$Year, CYRS$Value)
  value$Year <- CYRS$all_time[r_idx]
  data.table::setnames(value, "Year", time_set)
  raw_shock$set[match("Year", raw_shock$set)] <- time_set
  updated <- list(raw_shock = raw_shock,
                  value = value)
  return(updated)
}
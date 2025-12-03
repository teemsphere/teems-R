#' @importFrom data.table fread CJ setnames fsetdiff
#' @importFrom purrr pluck map
#' @importFrom tibble tibble
#' @importFrom rlang abort
#' @importFrom utils capture.output
#' 
#' @noRd
#' @keywords internal
#' @method .construct_shock scenario
#' @export
.construct_shock.scenario <- function(raw_shock,
                                      closure,
                                      sets,
                                      ...) {
  value <- raw_shock$input
  value <- subset(value, Year %in% attr(sets, "CYRS")$Value)
  list2env(.year2time_set(raw_shock = raw_shock,
                          sets = sets,
                          value = value,
                          call = call),
           envir = environment())


  set_ele <- purrr::map(with(sets$mapping, mget(raw_shock$ls_upper)), 1)
  template_shk <- do.call(data.table::CJ, c(set_ele, sorted = FALSE))
  data.table::setnames(template_shk, new = raw_shock$ls_mixed)
  
  if (.o_check_shock_status()) {
    if (!data.table::fsetequal(template_shk, value[, !"Value"])) {
      missing_tuples <- data.table::fsetdiff(template_shk, value[, !"Value"])
      n_missing_tuples <- nrow(missing_tuples)
      missing_tuples <- utils::capture.output(print(missing_tuples))[-c(1:3)]
      .cli_action(
        shk_err$scen_missing_tup,
        action = c("abort", "inform"),
        call = attr(raw_shock, "call")
      )
    }
  }

  class(value) <- c("dat", class(value))
  data.table::setnames(value, raw_shock$ls_mixed, raw_shock$ls_upper)
  value <- .aggregate_data(dt = value,
                           sets = sets$mapping,
                           ndigits = .o_ndigits())

  int_sets <- subset(sets,
                     qualifier_list == "(intertemporal)",
                     name,
                     1)
  
  # some grep should be which
  int_col <- which(colnames(value) %in% int_sets)
  data.table::setnames(value, raw_shock$ls_upper, raw_shock$ls_mixed)
  non_int_col <- colnames(value)[-c(int_col, ncol(value))]
  int_col <- colnames(value)[int_col]
  
  value[, Value := {
    baseline <- Value[get(int_col) == 0]
    (Value - baseline) / baseline * 100
  }, by = non_int_col]


  raw_shock$input <- value
  class(raw_shock) <- "custom"

  .construct_shock(
    raw_shock = raw_shock,
    closure = closure,
    sets = sets
  )
}

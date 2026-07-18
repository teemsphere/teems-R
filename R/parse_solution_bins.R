#' @importFrom tibble as_tibble
#' @importFrom data.table data.table
#'
#' @keywords internal
#' @noRd
.parse_solution_bins <- function(sol_prefix, var_names = character(0)) {
  raw <- parse_solution_bins(sol_prefix, var_names)

  var_union <- tibble::as_tibble(data.frame(
    r_idx       = seq_along(raw$var$cofname) - 1L,
    cofname     = raw$var$cofname,
    begadd      = raw$var$begadd,
    size        = raw$var$size,
    setid       = raw$var$setid,
    antidims    = raw$var$antidims,
    matsize     = raw$var$matsize,
    level_par   = raw$var$level_par,
    change_real = raw$var$change_real,
    suplval     = raw$var$suplval,
    gltype      = raw$var$gltype,
    glval       = raw$var$glval,
    stringsAsFactors = FALSE
  ))

  xc <- data.table::data.table(
    r_idx = seq_along(raw$bin) - 1L,
    Value = raw$bin
  )
  # embedded-RK runs write per-element cumulative error metrics (.acc)
  if (!is.null(raw$acc)) {
    xc$error_metric <- raw$acc
  }

  list(
    var_union = var_union,
    xc        = xc
  )
}

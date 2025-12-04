#' @importFrom data.table setkey setkeyv setnames .SD
#' @importFrom purrr pluck
#' @importFrom rlang is_integerish
#' 
#' @keywords internal
#' @noRd
.aggregate_data <- function(dt,
                            sets,
                            ...) {
  UseMethod(".aggregate_data")
}

#' @keywords internal
#' @noRd
#' @method .aggregate_data dat
#' @export
.aggregate_data.dat <- function(dt,
                                sets,
                                ndigits,
                                ...) {
  # NSE
  Value <- NULL
  
  xval_col <- colnames(dt)[!colnames(dt) %in% "Value"]
  dt <- .map_data(dt = dt, sets = sets, col = xval_col)
  if (any(duplicated(xval_col))) {
    xval_col[duplicated(xval_col)] <- paste0(xval_col[duplicated(xval_col)], ".1")
    colnames(dt)[seq_along(xval_col)] <- xval_col
  }
  data.table::setkeyv(dt, xval_col)
  dt <- dt[, sum(Value), by = xval_col]
  data.table::setnames(dt, "V1", "Value")
  if (!rlang::is_integerish(dt$Value)) {
    dt[, let(Value = round(Value, ndigits))]
  } 
  return(dt)
}

#' @keywords internal
#' @noRd
#' @method .aggregate_data par
#' @export
.aggregate_data.par <- function(dt,
                                sets,
                                ndigits,
                                ...) {
  # NSE
  Value <- NULL
  
  xval_col <- colnames(dt)[!colnames(dt) %in% c("Value", "omega", "sigma")]
  dt <- .map_data(dt = dt, sets = sets, col = xval_col)
  if (any(duplicated(xval_col))) {
    xval_col[duplicated(xval_col)] <- paste0(xval_col[duplicated(xval_col)], ".1")
    colnames(dt)[seq_along(xval_col)] <- xval_col
  }

  if (all(c("sigma", "omega") %in% colnames(dt))) {
    dt <- dt[, lapply(.SD, FUN = sum), .SDcols = c("Value", "omega", "sigma"), by = xval_col]
    dt$Value <- dt$sigma / dt$omega
    dt[is.nan(Value), let(Value = 1)] # for CGDS, return here - make this a header-specific method
    dt[, let(sigma = NULL, omega = NULL)]
  } else {
    sets <- setdiff(colnames(dt), "Value")
    if (!sets %=% character(0)) {
      dt <- dt[, list(Value = mean(Value)), by = sets]
    }
  }
  if (!rlang::is_integerish(dt$Value)) {
    dt[, let(Value = round(Value, ndigits))]
  } 
  if (!xval_col %=% character(0)) {
    data.table::setkeyv(dt, xval_col)
  }
  return(dt)
}

#' @keywords internal
#' @noRd
#' @method .aggregate_data set
#' @export
.aggregate_data.set <- function(dt,
                                sets,
                                ...) {

  # NSE
  mapping <- origin <- NULL
  
  if (inherits(dt, names(sets))) {
    r_idx <- match(dt$Value, purrr::pluck(sets, class(dt)[2], 1))
    dt[[2]] <- purrr::pluck(sets, class(dt)[2], 2)[r_idx]
    data.table::setnames(dt, new = c("origin", "mapping"))
    data.table::setkey(dt, mapping, origin)
    return(dt)
  }
}

# introduce header-specific methods



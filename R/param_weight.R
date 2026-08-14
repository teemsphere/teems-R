#' @importFrom data.table copy rbindlist let setnames
#'
#' @keywords internal
#' @noRd
.weight_param <- function(i_data,
                          weights,
                          data_format) {

  # NSE
  omega <- Value <- NULL

  weight_map <- switch(data_format,
                       "GTAPv6" = param_weights$GTAPv6,
                       "GTAPv7" = param_weights$GTAPv7)

  if (data_format %=% "GTAPv7" && !is.null(weights$ISEP)) {
    w <- weights$ISEP
    if (is.data.frame(w)) {
      w <- data.table::copy(w)
      data.table::setnames(w, old = "COMM", new = "ACTS")
    } else {
      nd <- names(dimnames(w))
      nd[nd %in% "COMM"] <- "ACTS"
      names(dimnames(w)) <- nd
    }
    weights$ISEP <- w
  }

  flip_headers <- sub("-", "", grep("-", unlist(weight_map), value = TRUE))
  weight_map <- lapply(weight_map, gsub, pattern = "-", replacement = "")

  # several parameter headers share weight headers and set signatures;
  # reduce each (weight, kept-sets) combination only once
  reduce_cache <- new.env(parent = emptyenv())

  i_data <- lapply(i_data, function(h) {
    if (inherits(h, names(weight_map))) {
      w_headers <- weight_map[[class(h)[1]]]
      ls_w <- weights[w_headers]
      sets <- colnames(h)[!colnames(h) %in% "Value"]
      w <- data.table::rbindlist(lapply(ls_w, function(weight) {
        flip <- inherits(weight, flip_headers)
        if (!is.data.frame(weight)) {
          cache_key <- paste(c(class(weight)[1], sort(sets)), collapse = "|")
          reduced <- reduce_cache[[cache_key]]
          if (is.null(reduced)) {
            reduced <- .reduce_array(arr = weight, keep = sets)
            reduce_cache[[cache_key]] <- reduced
          }
          weight <- reduced
        }
        weight <- weight[, list(Value = sum(Value)), by = sets]
        if (flip) {
          weight[, let(Value = Value * -1)]
        }
        return(weight)
      }))[, list(omega = sum(Value)), by = sets]
      h <- merge(h, w, sets)
      h[, let(sigma = Value * omega)]
    }
    return(h)
  })

  return(i_data)
}

#' @importFrom data.table setDT setkeyv
#' @importFrom rlang is_integerish
#'
#' @keywords internal
#' @noRd
.arr_codes <- function(dn, sets) {
  ulevs <- vector("list", length(dn))
  codes <- vector("list", length(dn))
  for (k in seq_along(dn)) {
    lev <- dn[[k]]
    nm <- names(dn)[k]
    if (!is.null(sets[[nm]])) {
      tab <- sets[[nm]]
      lev <- tab[, 2][[1]][match(lev, tolower(tab[, 1][[1]]))]
    }
    ulevs[[k]] <- unique(lev)
    codes[[k]] <- match(lev, ulevs[[k]]) - 1L
  }
  list(ulevs = ulevs, codes = codes)
}

# expand the dense kernel result into columns for the kept dims;
# strides follow the full out_sizes vector (collapsed dims are size 1)
#' @keywords internal
#' @noRd
.arr_expand_cols <- function(ulevs, out_sizes, keep, n_out) {
  cols <- vector("list", sum(keep))
  ci <- 0L
  for (k in seq_along(ulevs)) {
    if (!keep[k]) next
    ci <- ci + 1L
    each <- prod(out_sizes[seq_len(k - 1L)])
    idx <- rep(
      rep.int(seq_along(ulevs[[k]]), rep.int(as.integer(each), out_sizes[k])),
      length.out = n_out
    )
    cols[[ci]] <- ulevs[[k]][idx]
  }
  cols
}

# aggregate a classed numeric array directly to the keyed data.table that
# .aggregate_data.dat would produce from its array2DF expansion; the set
# mapping is applied to dimension levels, never to the expanded cells
#' @keywords internal
#' @noRd
.aggregate_array <- function(arr,
                             sets,
                             ndigits) {
  dn <- lapply(dimnames(arr), tolower)
  nms <- names(dn)
  nms[duplicated(nms)] <- paste0(nms[duplicated(nms)], ".1")

  cd <- .arr_codes(dn = dn, sets = sets)
  out_sizes <- vapply(cd$ulevs, length, integer(1))
  val <- agg_array_sum(arr, cd$codes, out_sizes)

  cols <- .arr_expand_cols(
    ulevs = cd$ulevs,
    out_sizes = out_sizes,
    keep = rep(TRUE, length(cd$ulevs)),
    n_out = length(val)
  )
  names(cols) <- nms

  if (is.integer(arr)) {
    val <- as.integer(val)
  }
  dt <- data.table::setDT(c(cols, list(Value = val)))
  if (!rlang::is_integerish(dt$Value)) {
    dt[, let(Value = round(Value, ndigits))]
  }
  data.table::setkeyv(dt, nms)
  class(dt) <- c(class(arr)[1:2], class(dt))
  return(dt)
}

# marginal sum of a numeric array over the dims named in `keep`,
# returned as a data.table with lowercased level columns; used to build
# parameter weights without expanding the source array
#' @keywords internal
#' @noRd
.reduce_array <- function(arr,
                          keep) {
  dn <- lapply(dimnames(arr), tolower)
  nms <- names(dn)
  keep_dim <- nms %in% keep

  ulevs <- vector("list", length(dn))
  codes <- vector("list", length(dn))
  for (k in seq_along(dn)) {
    if (keep_dim[k]) {
      ulevs[[k]] <- unique(dn[[k]])
      codes[[k]] <- match(dn[[k]], ulevs[[k]]) - 1L
    } else {
      ulevs[[k]] <- NA_character_
      codes[[k]] <- integer(length(dn[[k]]))
    }
  }
  out_sizes <- vapply(ulevs, length, integer(1))
  val <- agg_array_sum(arr, codes, out_sizes)

  cols <- .arr_expand_cols(
    ulevs = ulevs,
    out_sizes = out_sizes,
    keep = keep_dim,
    n_out = length(val)
  )
  names(cols) <- nms[keep_dim]
  dt <- data.table::setDT(c(cols, list(Value = val)))
  return(dt)
}

#' @importFrom purrr map map2 map2_lgl pmap
#' @importFrom data.table data.table CJ setnames setkeyv set
#' @importFrom tibble tibble
#'
#' @keywords internal
#' @noRd
.compose_coeff_bin <- function(data_dt,
                               coeff_extract,
                               cofs,
                               sets,
                               time_steps,
                               call) {

  # ordinary coefficients first, PostSim after, each alphabetical (the
  # order the CSV path's list.files() produced)
  cofs <- cofs[order(cofs$postsim, cofs$cofname), ]

  cofs$setid <- strsplit(cofs$setid, split = ",")
  cofs$column_id <- purrr::map2(
    cofs$setid,
    cofs$size,
    function(x, y) {
      if (y != 0) x[1:y] else NA
    }
  )

  set_tbl <- tibble::tibble(
    id = seq(0, length(sets) - 1),
    sets = sets
  )

  cofs$set <- purrr::map(cofs$column_id, function(c_id) {
    set_tbl$sets[match(c_id, set_tbl$id)]
  })

  cofs$dt <- lapply(cofs$set, function(ele) {
    if (is.null(unlist(ele))) {
      data.table::data.table(null_set = NA)
    } else {
      do.call(data.table::CJ, c(ele, sorted = FALSE))
    }
  })

  if (!all(lapply(cofs$dt, nrow) == cofs$matsize) ||
    sum(unlist(lapply(cofs$dt, nrow))) %!=% nrow(data_dt)) {
    .cli_action(compose_err$idx_mismatch,
      action = "abort",
      .internal = TRUE,
      call = call
    )
  }

  ce_idx <- match(cofs$cofname, tolower(coeff_extract$name))
  if (anyNA(ce_idx)) {
    .cli_action(compose_err$coeff_check,
      action = "abort",
      .internal = TRUE,
      call = call
    )
  }
  coeff_extract <- coeff_extract[ce_idx, ]

  offsets <- purrr::map2(cofs$pack_begadd, cofs$matsize, function(b, m) {
    seq.int(b + 1L, length.out = m)
  })

  # declared index sets must be the sets the solver dumped
  set_names <- names(sets)
  strict_check <- all(purrr::map2_lgl(
    coeff_extract$ls_mixed_idx,
    cofs$column_id,
    function(mixed, c_id) {
      if (mixed %=% NA_character_ || anyNA(c_id)) {
        return(mixed %=% NA_character_ && anyNA(c_id))
      }
      declared <- tolower(.dock_tail(mixed))
      dumped <- tolower(set_names[as.integer(c_id) + 1L])
      length(declared) == length(dumped) && all(declared == dumped)
    }
  ))
  if (!strict_check) {
    .cli_action(compose_err$strict_check,
      action = "abort",
      .internal = TRUE,
      call = call
    )
  }

  dats <- purrr::pmap(
    list(cofs$dt, offsets, coeff_extract$ls_mixed_idx),
    function(dt, idx, mixed) {
      if (mixed %=% NA_character_) {
        return(data.table::data.table(Value = data_dt$Value[idx]))
      }
      data.table::set(dt, j = "Value", value = data_dt$Value[idx])
      data.table::setnames(dt, new = c(mixed, "Value"))
      data.table::setkeyv(dt, cols = mixed)
      dt
    }
  )
  names(dats) <- coeff_extract$name

  if (!is.null(time_steps)) {
    dats <- lapply(dats,
      FUN = .match_year,
      sets = sets,
      time_steps = time_steps
    )
  }

  tibble::tibble(
    name = coeff_extract$name,
    label = coeff_extract$label,
    type = ifelse(cofs$postsim, "postsim", "coefficient"),
    dat = dats
  )
}

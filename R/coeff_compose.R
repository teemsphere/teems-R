#' @importFrom purrr pluck pmap transpose simplify
#' @importFrom tibble tibble add_column
#' @keywords internal
#' @noRd
.compose_coeff <- function(paths,
                           coeff_extract,
                           sets,
                           time_steps,
                           call,
                           type_label = "coefficient") {

  ls_data <- lapply(paths, readLines)

  list_coeff <- lapply(
    ls_data,
    FUN = function(dat) {
      lead <- dat[1]
      dat  <- dat[-length(dat)]
      dat  <- dat[-1]
      name  <- purrr::pluck(strsplit(lead, split = '"'), 1, 2)
      label <- purrr::pluck(strsplit(lead, split = '"'), 1, 4)
      dim   <- as.integer(strsplit(strsplit(lead, split = "Real|Integer")[[1]][1], " ")[[1]])
      list(name, label, dim, dat)
    }
  )

  transposed <- purrr::transpose(list_coeff)

  coeff_tib <- tibble::tibble(
    name    = purrr::simplify(transposed[[1]], .type = "character"),
    label   = purrr::simplify(transposed[[2]], .type = "character"),
    dim     = purrr::simplify(transposed[[3]], .type = "character"),
    ls_data = transposed[[4]]
  )

  coeff_names <- gsub(".csv", "", basename(paths), fixed = TRUE)
  coeff_extract <- coeff_extract[coeff_extract$name %in% coeff_names, ]

  if (!all(coeff_extract$name %in% coeff_tib$name)) {
    .cli_action(
      compose_err$coeff_check,
      action = "abort",
      call = call,
      .internal = TRUE
    )
  }

  r_idx <- match(coeff_tib$name, coeff_extract$name)
  coeff_tib$set_nmes <- coeff_extract$ls_mixed_idx[r_idx]

  coeff_tib$dat <- purrr::pmap(
    .l = list(
      dimen    = coeff_tib$dim,
      col_nmes = coeff_tib$set_nmes,
      num_ls   = coeff_tib$ls_data
    ),
    .f = function(dimen, col_nmes, num_ls) {
      dt <- .parse_coeff_block(dimen, col_nmes, num_ls, sets, call)
      return(dt)
    }
  )

  names(coeff_tib$dat) <- coeff_tib$name

  if (!is.null(time_steps)) {
    coeff_tib$dat <- lapply(coeff_tib$dat,
      FUN = .match_year,
      sets = sets,
      time_steps = time_steps
    )
  }

  coeff_tib <- coeff_tib[, c("name", "label", "dat")]
  coeff_tib <- tibble::add_column(coeff_tib, type = type_label, .after = "label")
  return(coeff_tib)
}

#' @importFrom purrr map_lgl pluck pmap transpose simplify
#' @importFrom tibble tibble
#' @importFrom data.table setDT setkeyv
#' 
#' @keywords internal
#' @noRd
.parse_coeff <- function(paths,
                         coeff_extract,
                         sets,
                         time_steps,
                         call) {
  ls_data <- lapply(paths, readLines)

  list_coeff <- lapply(
    ls_data,
    FUN = function(dat) {
      lead <- dat[1]
      dat <- dat[-length(dat)]
      dat <- dat[-1]
      name <- purrr::pluck(strsplit(lead, split = '"'), 1, 2)
      label <- purrr::pluck(strsplit(lead, split = '"'), 1, 4)
      dim <- as.integer(strsplit(strsplit(lead, split = "Real|Integer")[[1]][1], " ")[[1]])
      coeff <- list(name, label, dim, dat)
      return(coeff)
    }
  )

  transposed <- purrr::transpose(list_coeff)

  coeff_tib <- tibble::tibble(
    name = purrr::simplify(transposed[[1]], .type = "character"),
    label = purrr::simplify(transposed[[2]], .type = "character"),
    dim = purrr::simplify(transposed[[3]], .type = "character"),
    ls_data = transposed[[4]]
  )

  coeff_extract <- subset(
    coeff_extract,
    name %in% gsub(
      ".csv",
      "",
      basename(paths)
    )
  )

  if (!all(coeff_extract$name %in% coeff_tib$name)) {
    .cli_action(
      compose_err$coeff_check,
      action = "abort",
      call = call,
      .internal = TRUE
    )
  }

  r_idx <- match(
    coeff_tib$name,
    coeff_extract$name
  )

  coeff_tib$set_nmes <- coeff_extract$ls_mixed_idx[r_idx]

  coeff_tib$dat <- purrr::pmap(
    .l = list(
      dimen = coeff_tib$dim,
      col = coeff_tib$set_nmes,
      num_ls = coeff_tib$ls_data
    ),
    .f = function(dimen, col_nmes, num_ls) {
      dim_length <- length(dimen)

      if (!col_nmes %=% NA_character_) {
        plain_col <- .dock_tail(string = col_nmes)
        r_idx <- match(plain_col, names(sets))
        setele <- sets[r_idx]
        if (is.null(setele)) {
          .cli_action(compose_err$invalid_coeff_set,
            action = "abort",
            call = call
          )
        }
      }

      if (dim_length %=% 1L) {
        if (col_nmes %=% NA_character_) {
          df <- data.frame(Value = as.numeric(num_ls))
        } else {
          df <- data.frame(setele, Value = as.numeric(num_ls))
          colnames(df)[1] <- col_nmes
        }
      } else if (dim_length %=% 2L) {
        flat_vec <- as.numeric(unlist(strsplit(num_ls, split = ",")))
        arr <- t(array(
          flat_vec,
          rev(dimen)
        ))
        dimnames(arr) <- setele
        names(dimnames(arr)) <- col_nmes
        df <- array2DF(arr)
      } else {
        num_ls <- lapply(num_ls, function(r) {
          as.numeric(unlist(strsplit(r, split = ",")))
        })
        null_markers <- purrr::map_lgl(
          num_ls,
          function(l) {
            l %=% numeric(0)
          }
        )
        # not a pretty algo
        group_ids <- cumsum(null_markers)
        split_ls <- split(num_ls[!null_markers], f = group_ids[!null_markers])
        ls_mat <- lapply(
          split_ls,
          FUN = function(l) {
            t(array(unlist(l), rev(dimen[c(1, 2)])))
          }
        )
        arr <- array(unlist(ls_mat), dimen)
        dimnames(arr) <- setele
        names(dimnames(arr)) <- col_nmes
        df <- array2DF(arr)
      }
      if (!col_nmes %=% NA_character_) {
        if (any(purrr::map_lgl(setele, inherits, "numeric"))) {
          int_col <- colnames(df)[purrr::map_lgl(setele, inherits, "numeric")]
          df[int_col] <- lapply(df[int_col], as.numeric)
        }
        data.table::setDT(df, key = col_nmes)
      } else {
        data.table::setDT(df)
      }
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

  coeff_tib <- subset(coeff_tib, select = c(name, label, dat))
  return(coeff_tib)
}

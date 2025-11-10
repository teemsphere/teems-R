#' @importFrom purrr map map2
#' @importFrom data.table data.table CJ setnames fread setkey
#' @importFrom tibble tibble
#' 
#' @keywords internal
#' @noRd
.parse_var <- function(paths,
                       var_extract,
                       vars,
                       sets,
                       time_steps,
                       call) {

  vars$setid <- strsplit(vars$setid, split = ",")

  vars$column_id <- purrr::map2(
    vars$setid,
    vars$size,
    function(x, y) {
      if (y != 0) {
        dim <- x[1:y]
      } else {
        dim <- NA
      }
      return(dim)
    }
  )

  sets <- tibble::tibble(
    id = seq(0, length(sets) - 1),
    sets = sets
  )

  vars$set <- purrr::map(vars$column_id, function(c_id) {
    c_idx <- match(c_id, sets$id)
    set_col <- sets$sets[c_idx]
    return(set_col)
  })

  vars$dt <- lapply(vars$set, function(ele) {
    if (is.null(unlist(ele))) {
      data.table::data.table(null_set = NA)
    } else {
      do.call(data.table::CJ, c(ele, sorted = FALSE))
    }
  })

  names(vars$dt) <- vars$cofname

  data_file <- list.files(grep("bin_csvs", paths, value = TRUE), full.names = TRUE)

  data_dt <- data.table::fread(data_file,
    header = FALSE,
    skip = 1
  )

  if (!all(lapply(vars$dt, nrow) == vars$matsize) ||
    !sum(unlist(lapply(vars$dt, nrow))) %=% nrow(data_dt)) {
    .cli_action(compose_err$idx_mismatch,
      action = "abort",
      .internal = TRUE,
      call = call
    )
  }

  data.table::setnames(data_dt, new = c("r_idx", "Value"))

  # bring in variable names by matrix size
  data_dt$var <- rep(vars$cofname, vars$matsize)

  data_dt <- purrr::map(vars$cofname, function(nm) {
    sets <- vars$dt[[nm]]
    dt_data <- data_dt[which(var == nm)]
    dt <- cbind(sets, dt_data[, -c("r_idx", "var")])
    return(dt)
  })

  names(data_dt) <- vars$cofname

  var_extract$ls_upper_idx <- ifelse(is.na(var_extract$ls_upper_idx),
    "null_set",
    var_extract$ls_upper_idx
  )

  lax_check <- all(unlist(purrr::map2(
    var_extract$ls_upper_idx,
    purrr::map(data_dt, colnames),
    function(check, parsed) {
      all(is.element(check, parsed[parsed != "Value"]))
    }
  )))
  if (!lax_check) {
    .cli_action(compose_err$lax_check,
      action = "abort",
      .internal = TRUE,
      call = call
    )
  }

  strict_check <- all(unlist(purrr::map2(
    var_extract$ls_upper_idx,
    purrr::map(data_dt, colnames),
    function(check, parsed) {
      all(check == parsed[parsed != "Value"])
    }
  )))
  if (!strict_check) {
    .cli_action(compose_err$strict_check,
      action = "abort",
      .internal = TRUE,
      call = call
    )
  }

  if (!all(names(data_dt) == tolower(var_extract$name))) {
    .cli_action(compose_err$var_check,
      action = "abort",
      call = call
    )
  }

  purrr::map2(
    data_dt,
    var_extract$ls_mixed_idx,
    function(dt, mixed_col) {
      if (!mixed_col %=% NA_character_) {
        data.table::setnames(dt, new = c(mixed_col, "Value"))
        data.table::setkeyv(dt, cols = mixed_col)
      } else {
        dt[, null_set := NULL]
      }
    }
  )

  r_idx <- match(names(data_dt), tolower(var_extract$name))

  f_var <- tibble::tibble(
    name = var_extract$name[r_idx],
    label = var_extract$label[r_idx],
    dat = data_dt
  )
  
  names(f_var$dat) <- f_var$name

  if (!is.null(time_steps)) {
    f_var$dat <- lapply(f_var$dat,
                        FUN = .match_year,
                        sets = sets$sets,
                        time_steps = time_steps)
  }

  return(f_var)
}

#' @importFrom purrr map map2
#' @importFrom data.table data.table CJ setnames setkeyv
#' @importFrom tibble tibble add_column add_row
#' 
#' @keywords internal
#' @noRd
.compose_var <- function(data_dt,
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

  if (!all(lapply(vars$dt, nrow) == vars$matsize) ||
    sum(unlist(lapply(vars$dt, nrow))) %!=% nrow(data_dt)) {
    .cli_action(compose_err$idx_mismatch,
      action = "abort",
      .internal = TRUE,
      call = call
    )
  }

  has_acc <- "error_metric" %in% colnames(data_dt)
  if (has_acc) {
    data.table::setnames(data_dt, new = c("r_idx", "Value", "error_metric"))
  } else {
    data.table::setnames(data_dt, new = c("r_idx", "Value"))
  }

  # bring in variable names by matrix size
  data_dt$var <- rep(vars$cofname, vars$matsize)

  # solver-managed derived complementarity variables (teems-solver
  # C1/C2, design doc sections 7-8) ride the solution binaries but are
  # not model variables ('@' cannot occur in user names). The
  # machinery internals -- the dummy comp@d and the Newton-correction
  # del_comp@ -- are dropped; the VALUE-carrying derived variables
  # (comp@e = the complementarity expression, comp@l/@u = variable
  # minus bound) are exposed with var_extract rows synthesized below.
  derived <- grepl("@", vars$cofname, fixed = TRUE)
  drop <- grepl("@d$", vars$cofname) | vars$cofname == "del_comp@"
  if (any(drop)) {
    vars <- vars[!drop, ]
    derived <- derived[!drop]
  }
  if (any(derived)) {
    for (nm in vars$cofname[derived]) {
      dt_cols <- colnames(vars$dt[[nm]])
      scalar <- identical(dt_cols, "null_set")
      var_extract <- tibble::add_row(
        var_extract,
        name = nm,
        label = paste0(
          "derived complementarity ",
          switch(substring(nm, nchar(nm), nchar(nm)),
            e = "expression",
            l = "variable minus lower bound",
            u = "variable minus upper bound",
            "variable"
          )
        ),
        ls_upper_idx = list(if (scalar) NA_character_ else dt_cols),
        ls_mixed_idx = list(if (scalar) NA_character_ else dt_cols)
      )
    }
    # the solver's variable order (post-drop) drives the alignment; a
    # genuine mismatch falls through to the var_check abort below
    ve_idx <- match(vars$cofname, tolower(var_extract$name))
    if (!anyNA(ve_idx)) {
      var_extract <- var_extract[ve_idx, ]
    }
  }

  data_dt <- purrr::map(vars$cofname, function(nm) {
    sets <- vars$dt[[nm]]
    dt_data <- data_dt[data_dt$var == nm, ]
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
      all(is.element(tolower(check), tolower(parsed[!parsed %in% c("Value", "error_metric")])))
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
      all(tolower(check) == tolower(parsed[!parsed %in% c("Value", "error_metric")]))
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
      if (mixed_col %!=% NA_character_) {
        new_names <- c(mixed_col, "Value")
        if (has_acc) new_names <- c(new_names, "error_metric")
        data.table::setnames(dt, new = new_names)
        data.table::setkeyv(dt, cols = mixed_col)
      } else {
        dt[, let(null_set = NULL)]
      }
    }
  )

  r_idx <- match(names(data_dt), tolower(var_extract$name))

  var_tib <- tibble::tibble(
    name = var_extract$name[r_idx],
    label = var_extract$label[r_idx],
    dat = data_dt
  )

  names(var_tib$dat) <- var_tib$name

  if (!is.null(time_steps)) {
    var_tib$dat <- lapply(var_tib$dat,
      FUN = .match_year,
      sets = sets$sets,
      time_steps = time_steps
    )
  }

  var_tib <- tibble::add_column(var_tib, type = "variable", .after = "label")
  return(var_tib)
}
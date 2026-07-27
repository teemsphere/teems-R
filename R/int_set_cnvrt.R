#' @importFrom data.table data.table
#'
#' @keywords internal
#' @noRd
.convert_int_sets <- function(expr,
                              n_timestep,
                              n_timestep_coeff,
                              set_name,
                              call) {

  bad_set <- set_name
  bad_def <- expr
  terms <- strsplit(expr, "-|\\s*-\\s*")[[1]]
  if (grepl("P\\[.*\\]\\s*-\\s*P\\[", expr)) {
    terms <- strsplit(expr, "\\s*-\\s*(?=P\\[)",
      perl = TRUE
    )[[1]]
    start <- .convert_p_term(
      term = terms[1],
      n_timestep = n_timestep,
      n_timestep_coeff = n_timestep_coeff,
      set_name = set_name,
      call = call
    )
    end <- .convert_p_term(
      term = terms[2],
      n_timestep = n_timestep,
      n_timestep_coeff = n_timestep_coeff,
      set_name = set_name,
      call = call
    )
    # S8: the range must be non-empty and ascending (the solver
    # aborts; R's start:end would silently count DOWN)
    if (end < start) {
      range_defect <- "an empty or inverted"
      resolved_txt <- paste0("p[", start, "] - p[", end, "]")
      .cli_action(model_err$set_int_range,
        action = c("abort", "inform"),
        call = call
      )
    }
    num_vec <- c(start:end)
  } else {
    num_vec <- .convert_p_term(
      term = expr,
      n_timestep = n_timestep,
      n_timestep_coeff = n_timestep_coeff,
      set_name = set_name,
      call = call
    )
  }

  # S8: indices must exist among the model's time steps (CYRS spans
  # p[0] .. p[n_timestep - 1])
  if (any(num_vec < 0 | num_vec > n_timestep - 1)) {
    range_defect <- "an out-of-range"
    resolved_txt <- paste0(
      "p[", num_vec[1], "]",
      if (length(num_vec) > 1L) paste0(" - p[", num_vec[length(num_vec)], "]")
    )
    .cli_action(model_err$set_int_range,
      action = c("abort", "inform"),
      call = call
    )
  }

  mapping <- data.table::data.table(
    origin = num_vec,
    mapping = num_vec,
    key = c("origin", "mapping")
  )
  return(mapping)
}

#' @keywords internal
#' @noRd
.convert_p_term <- function(term,
                            n_timestep,
                            n_timestep_coeff,
                            set_name,
                            call) {

  content <- gsub(
    "P\\[|\\]",
    "",
    term
  )
  content <- gsub(
    n_timestep_coeff,
    as.character(n_timestep),
    content
  )
  out <- tryCatch(
    eval(parse(text = content)),
    error = function(e) NULL
  )
  if (is.null(out) || length(out) != 1L || !is.numeric(out) ||
    !is.finite(out) || out != round(out)) {
    bad_set <- set_name
    bad_def <- term
    .cli_action(model_err$set_int_malformed,
      action = "abort",
      call = call
    )
  }
  out
}

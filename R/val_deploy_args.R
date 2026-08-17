#' @importFrom tools R_user_dir
#' 
#' @keywords internal
#' @noRd
.validate_deploy_args <- function(a,
                                  sets,
                                  call,
                                  data_call) {
  checklist <- list(
    .data = c("ems_data", "list"),
    model = "data.frame",
    shock = c("NULL", "list"),
    swap_in = c("NULL", "character", "list"),
    swap_out = c("NULL", "character", "list"),
    shock_file = c("NULL", "character"),
    write_coefficients = "logical"
  )
  
  .check_arg_class(
    args_list = a,
    checklist = checklist,
    call = call
  )

  if (!is.null(a$shock)) {
    a$shock <- .expand_ele(input = a$shock)
    a$shock <- lapply(
      a$shock,
      .check_shock,
      var_extract = a$model[a$model$type == "Variable", ],
      sets = sets
    )
  }
  
  if (!is.null(a$swap_in)) {
    a$swap_in <- .expand_ele(input = a$swap_in, nested = TRUE)
    a$swap_in <- lapply(a$swap_in,
      .check_swap,
      var_extract = a$model[a$model$type == "Variable", ],
      sets = sets,
      call = call
    )
  }

  if (!is.null(a$swap_out)) {
    a$swap_out <- .expand_ele(input = a$swap_out, nested = TRUE)
    a$swap_out <- lapply(a$swap_out,
      .check_swap,
      var_extract = a$model[a$model$type == "Variable", ],
      sets = sets,
      call = call
    )
  }

  if (!is.null(a$shock_file)) {
    if (!is.null(a$shock)) {
      .cli_action(deploy_err$shk_file_shocks,
        action = "abort",
        call = call
      )
    }

    a$shock <- .check_input(
      file = a$shock_file,
      valid_ext = "shf",
      call = call
    )
  }

  if (length(a$write_coefficients) %!=% 1L || is.na(a$write_coefficients)) {
    .cli_action(deploy_err$write_coefficients,
      action = "abort",
      call = call
    )
  }

  non_int_req <- setdiff(
    a$model[!is.na(a$model$header), ]$header,
    c(.o_n_timestep_header(), .o_timestep_header())
  )

  # (by_elements) mapping headers are character data validated against
  # the raw input headers at the data build (.finalize_map_data), not
  # against the aggregated coefficient tables
  byele <- a$model$type == "Read" &
    !is.na(a$model$qualifier_list) &
    grepl("by_elements", a$model$qualifier_list, ignore.case = TRUE)
  non_int_req <- setdiff(non_int_req, a$model$header[byele])

  model_headers <- attr(a$model, "header")
  if (!is.null(model_headers)) {
    non_int_req <- setdiff(non_int_req, model_headers)
  }

  if (any(!non_int_req %in% names(a$.data))) {
    missing_headers <- setdiff(non_int_req, names(a$.data))
    .cli_action(deploy_err$missing_header,
      action = "abort",
      call = data_call
    )
  }

  return(a)
}
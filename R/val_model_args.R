#' @keywords internal
#' @noRd
.validate_model_args <- function(a,
                                 call) {
  a[["..."]] <- NULL

  a$mod_coeff <- .check_named_dots(a$mod_coeff)
  
  if (isFALSE(a$mod_coeff)) {
    .cli_action(model_err$no_name_coeff,
                action = "abort",
                call = call
    )
  }
  
  checklist <- list(
    model_file = "character",
    closure_file = "character",
    omit = c("NULL", "character"),
    backsolve = c("NULL", "character"),
    ignore_condense = "logical",
    mod_coeff = c("logical", "list")
  )

  .check_arg_class(
    args_list = a,
    checklist = checklist,
    call = call
  )

  if (length(a$ignore_condense) != 1L || is.na(a$ignore_condense)) {
    .cli_action("{.arg ignore_condense} must be {.val TRUE} or {.val FALSE}.",
      action = "abort",
      call = call
    )
  }

  a$model_file <- .check_input(
    file = a$model_file,
    valid_ext = "tab",
    call = call
  )

  if (!is.null(a$closure_file)) {
    a$closure_file <- .check_input(
      file = a$closure_file,
      valid_ext = "cls",
      call = call
    )
  }

  a$closure <- .load_closure(
    closure_file = a$closure_file,
    model_file = a$model_file,
    call = call
  )

  return(a)
}
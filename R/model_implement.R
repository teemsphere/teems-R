#' @importFrom cli cli_verbatim
#' 
#' @keywords internal
#' @noRd
.implement_model <- function(args_list,
                             call) {

  v <- .validate_model_args(
    a = args_list,
    call = call
  )

  model <- .process_tablo(
    tab_file = v$model_file,
    omit = v$omit,
    backsolve = v$backsolve,
    ignore_condense = v$ignore_condense,
    call = call
  )

  if (.o_verbose()) {
    model_summary <- attr(model, "model_summary")
  }

  if (v$mod_coeff %!=% NA) {
    model <- .coeff_mod(
      mod_coeff = v$mod_coeff,
      model = model,
      call = call
    )
  }

  closure <- .check_closure(
    closure = v$closure,
    var_extract = model[model$type == "Variable", ],
    call = call
  )

  if (.o_verbose()) {
    cli::cli_verbatim(model_summary)
  }
  
  model <- structure(model,
    closure = closure,
    closure_file = attr(v$closure, "file"),
    auto_omit = v$auto_omit,
    call = call
  )

  return(model)
}
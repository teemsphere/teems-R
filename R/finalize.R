#' @importFrom purrr map_lgl
#' 
#' @keywords internal
#' @noRd
.finalize <- function(args_list,
                      call) {
  metadata <- attr(args_list$.data, "metadata")
  attr(metadata, "file") <- "metadata.rds"
  data_call <- attr(args_list$.data, "call")
  model_call <- attr(args_list$model, "call")
  # condensed (omitted/backsolved) variables are out of the solve system:
  # closure, shock, and system-size handling see live variables only
  var_extract <- args_list$model[
    args_list$model$type == "Variable" & is.na(args_list$model$condense),
  ]
  sets <- .finalize_sets(
    sets = args_list$.data[purrr::map_lgl(args_list$.data, inherits, "set")],
    set_extract = args_list$model[args_list$model$type == "Set", ],
    coeff_extract = args_list$model[args_list$model$type == "Coefficient", ],
    time_steps = attr(args_list$.data, "time_steps"),
    reference_year = metadata$reference_year,
    call = call,
    data_call = data_call,
    model_call = model_call
  )
  .check_subset_containment(
    sets = sets,
    call = model_call
  )
  v <- .validate_deploy_args(
    a = args_list,
    sets = sets,
    call = call,
    data_call = data_call
  )
  closure <- .finalize_closure(
    closure = attr(v$model, "closure"),
    closure_file = attr(v$model, "closure_file"),
    swap_in = v$swap_in,
    swap_out = v$swap_out,
    sets = sets,
    var_extract = var_extract,
    call = call,
    model_call = model_call
  )
  # opt-in at ems_model(): unshocked wholly-exogenous variables leave the
  # deployed model, which the post-swap closure and finalized shocks are
  # the first point able to establish
  n_auto_omit <- 0L
  if (isTRUE(attr(args_list$model, "auto_omit"))) {
    auto <- .auto_omit(
      model = v$model,
      closure = closure,
      sets = sets,
      shock = v$shock,
      call = call
    )
    if (!is.null(auto)) {
      v$model <- auto$model
      closure <- auto$closure
      n_auto_omit <- length(auto$omitted)
      var_extract <- v$model[
        v$model$type == "Variable" & is.na(v$model$condense),
      ]
    }
  }
  # C2: components whose complementarity variable stays endogenous in
  # the post-swap closure are ACTIVE (solved by the solver's
  # approximate-run state machinery) and each contributes one E_$comp
  # equation element to the count-squaring below; exogenized
  # components are inert and net zero (teems-solver design doc
  # section 8)
  n_comp_active <- .comp_active_count(
    model = v$model,
    closure = closure,
    var_extract = var_extract,
    sets = sets,
    call = call
  )
  size_metadata <- .compute_size_metadata(
    var_extract = var_extract,
    sets = sets,
    closure = closure
  )
  .check_system_square(
    model = v$model,
    var_extract = var_extract,
    sets = sets,
    closure = closure,
    size_metadata = size_metadata,
    n_comp_active = n_comp_active,
    call = call
  )
  metadata$system_size <- size_metadata$system_size
  metadata$n_var_ele <- size_metadata$n_var_ele
  metadata$n_exo_ele <- size_metadata$n_exo_ele
  metadata$n_reg <- size_metadata$n_reg
  # read back at solve/probe time by the condensation advisory
  metadata$condense <- .compute_condense_metadata(
    model = v$model,
    sets = sets,
    system_size = size_metadata$system_size
  )
  metadata$condense$n_auto_omit <- n_auto_omit
  shocks <- .finalize_shocks(
    shock = v$shock,
    closure = closure,
    sets = sets,
    var_extract = var_extract
  )
  .data <- .finalize_data(
    .data = v$.data,
    sets = sets,
    model = v$model,
    call = call,
    model_call = model_call
  )
  .data <- c(.data, .finalize_map_data(
    model = v$model,
    sets = sets,
    set_raw = attr(args_list$.data, "set_raw"),
    call = call,
    data_call = data_call
  ))
  # record only: the bordered methods classify mapping-indexed
  # references as border since the Part A slice, so every matrix
  # method accepts mapped equations
  map_names <- v$model$name[v$model$type == "Mapping"]
  metadata$mapped_equations <- length(map_names) > 0L &&
    any(grepl(
      paste0("\\b(", paste(tolower(map_names), collapse = "|"), ")\\s*\\("),
      tolower(v$model$tab[v$model$type == "Equation"])
    ))
  tab <- .finalize_tab(
    model = v$model,
    write_coefficients = v$write_coefficients
  )
  cmf <- .finalize_cmf(
    model = v$model,
    shock_file = attr(shocks, "file"),
    tab_file = attr(tab, "file"),
    cls_file = attr(closure, "file"),
    write_coefficients = v$write_coefficients
  )
  cmf_path <- .write_input_files(
    tab = tab,
    closure = closure,
    shocks = shocks,
    cmf = cmf,
    .data = .data,
    v_shock = v$shock,
    metadata = metadata,
    sets = sets
  )
  return(cmf_path)
}
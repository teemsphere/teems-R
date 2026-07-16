build_shk_err <- function() {
  list(
    # test-ems_custom_shock.R: "ems_custom_shock errors when input data frame lacks Value column"
    # test-ems_scenario_shock.R: "ems_scenario_shock errors when input data frame lacks Value column"
    cst_scen_val_df = c(
      "{.obj_type_friendly {input}} supplied as a shock must have {.field Value} as the last column.",
      "Call: {.code {deparse(call)}}"
    ),
    # test-ems_scenario_shock.R: "ems_scenario_shock errors when no year df provided"
    scen_year_df = c(
      "{.obj_type_friendly {input}} supplied as a scenario shock must have a {.field Year} column consistent with selected time steps.",
      "Call: {.code {deparse(call)}}"
    ),
    # test-ems_uniform_shock.R: "ems_uniform_shock subsets are provided as named lists"
    uni_named_lst = c(
      "Subset arguments in {.arg ...} must be named pairs: {.code SETi = \"component\"}.",
      "Note that set names consist of the concatenation of the set name and variable-specific lowercase index.",
      "Call: {.code {deparse(call)}}"
    ),
    # test-ems_uniform_shock.R: "ems_uniform_shock errors when variable is not present in the model file"
    not_a_var = c(
      "Shock variable {.val {var_name}} not found in the model.",
      "Call: {.code {deparse(call)}}"
    ),
    # test-ems_model.R: "shocks on condensed variables abort"
    condensed_var = c(
      "Shock variable {.val {var_name}} was condensed out of the model ({condense_action}).",
      "Omitted variables must stay unshocked and backsolved variables are endogenous; drop the condensation action in {.fun teems::ems_model} to shock this variable."
    ),
    # test-ems_uniform_shock.R: "ems_uniform_shock errors when both int set and year are provided"
    # test-ems_custom_shock.R: "ems_custom_shock errors when both year and int set are provided"
    extra_col = c(
      "Either {.field Year} or intertemporal set {.field {supplied_int_set}} should be provided, not both.",
      "Call: {.code {deparse(call)}}"
    ),
    # test-ems_uniform_shock.R: "ems_uniform_shock errors when a set is specified that does not belong to a variable"
    invalid_set = c(
      "Set {.field {errant_set}} is not associated with {.val {var_name}}.",
      "Set designations within {.pkg teems} comprise the variable-specific uppercase set name and lowercase index.",
      "For {.val {var_name}} these include: {.field {ls_mixed}}.",
      "In intertemporal models, {.field Year} may be provided in lieu of an intertemporal set.",
      "Call: {.code {deparse(call)}}"
    ),
    # test-ems_uniform_shock.R: "ems_uniform_shock errors when full var shock is applied to not fully exogenous variable"
    x_full_exo = c(
      "{.field {raw_shock$var}} is not fully exogenous; full shocks require full exogeneity.",
      "If fully exogenous with multiple closure entries, consolidate into a single {.val {raw_shock$var}} entry.",
      "Call: {.code {deparse(call)}}"
    ),
    # test-ems_uniform_shock.R: "ems_uniform_shock errors when part var shock is applied to fully endogenous variable"
    x_full_exo_part = c(
      "No exogenous components in {.field {raw_shock$var}}; partial shocks require at least one.",
      "Call: {.code {deparse(call)}}"
    ),
    # test-ems_uniform_shock.R: "ems_uniform_shock errors when invalid year provide"
    uni_invalid_year = c(
      "{.val {Year}} is not among the valid years for the provided time steps: {.field {CYRS}}.",
      "Call: {.code {deparse(call)}}"
    ),
    # test-ems_uniform_shock.R: "ems_uniform_shock errors when invalid elements are"
    uni_invalid_RHS = c(
      "{.val {ele}} is not an element or subset belonging to set {.field {ele_set}}.",
      "Valid elements: {.val {recognized_ele}}.",
      "Valid subsets: {.val {recognized_ss}}.",
      "Call: {.code {deparse(call)}}"
    ),
    # test-ems_uniform_shock.R: "ems_uniform_shock errors when endogenous components are allocated shock"
    x_part_exo = c(
      "Shock allocated to non-exogenous tuples: {.field {errant_tup}}.",
      "Call: {.code {deparse(call)}}"
    ),
    # test-ems_custom_shock.R: "ems_custom_shock errors when invalid year is provided"
    invalid_year = c(
      "{n_errant_year_tuples} tuple{?s} in the provided {class(shk)[[1]]} shock contain invalid {.field Year} specifications: {.field {errant_year_tuples}}.",
      "Call: {.code {deparse(call)}}"
    ),
    # test-ems_custom_shock.R: "ems_custom_shock errors when input is missing sets"
    missing_set = c(
      "Shock input for {.field {var_name}} is missing sets: {.field {missing_set}}.",
      "Call: {.code {deparse(call)}}"
    ),
    # test-ems_custom_shock.R: "ems_custom_shock errors when invalid tuple is provided"
    cust_invalid_tup = c(
      "Some shock tuples contain elements outside their respective sets: {.field {errant_tuples}}.",
      "Call: {.code {deparse(call)}}"
    ),
    # test-ems_custom_shock.R: "ems_custom_shock errors when some shock tuples are endogenous"
    cust_endo_tup = c(
      "Shock applied to non-exogenous tuples: {.field {x_exo_parts}}.",
      "Call: {.code {deparse(call)}}"
    ),
    # leaving this as a failsafe (not possible to get this far)
    scen_dynamic = c(
      "Scenario shocks can only be employed with a temporally dynamic model.",
      "Call: {.code {deparse(call)}}"
    ),
    # test-ems_scenario_shock.R: "ems_scenario_shock errors when not all preaggregation tuples provided"
    scen_missing_tup = c(
      "{n_missing_tuples} tuple{?s} missing from the scenario shock: {.field {missing_tuples}}.",
      "Scenario shocks must cover all pre-aggregation tuples for associated sets.",
      "Call: {.code {deparse(call)}}"
    )
  )
}

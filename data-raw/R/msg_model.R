build_model_err <- function() {
  list(
    # test-ems_model.R: "ems_model rejects invalid variable names in var_omit"
    invalid_var_omit = "{.val {invalid_var}} designated for omission not found in the model.",
    # test-ems_model.R: "ems_model rejects invalid coefficient arguments"
    invalid_coeff = "{.arg {nme}} is not declared in the model.",
    # test-ems_model.R: "partial read statement"
    invalid_read = "Partial {.field Read} statements are not supported.",
    # leaving this in but there is not possible?
    invalid_mod = "{.arg nme} is neither read in nor appearing on the LHS of a formula.",
    # test-ems_model.R: "invalid numeric to a formula"
    invalid_numeric = c("Directly assigned numeric values must be length 1.",
                        "To assign heterogeneous values, use a {.code data.frame} with the appropriate set columns."),
    # test-ems_model.R: "invalid tab statement"
    invalid_state = c("teems {version} does not support {.field {inv_state}} statements.",
                      "Supported statements include: {.field {supported_state}}."),
    # probably a redundant check but if a weird unrecognized statement is found then there needs to be a way of distinguishing between an implied statement and an unrecognized statement
    unsupported_tab = "Unsupported Tablo declarations detected: {.field {unsupported}}.",
    # test-ems_model.R: "invalid intertemporal header"
    invalid_int_header = c(
      "Intertemporal {header_descr} {.val {timestep_header}} not found in loaded data.",
      "Use {.fun teems::ems_option_set} {.arg {arg_name}} to set a custom {header_descr}."
    ),
    # test-ems_model.R: "invalid read statement"
    missing_file = "Read statements missing \"from file\" detected.",
    # test-ems_model.R: "invalid binary set switch statement"
    binary_switch = c("Unsupported binary switch detected in a {.field Set} definition.",
                      "Declare sets explicitly within the Tablo file or using {.arg ...} within {.fun teems::ems_model}.",
                      "For example, {.field Set ENDWM # mobile endowment # (capital,unsklab,sklab);} {.emph not} {.field Set ENDWM # mobile endowments # = (all,e,ENDW:ENDOWFLAG(e,\"mobile\") ne 0);}."),
    # test-ems_model.R: "intertemporal set equality"
    int_set_eq_fail = c(
      "Set equality involving an intertemporal set detected: {.field {eq_statement}}.",
      "Converting between intertemporal and non-intertemporal sets via set equality is not supported."
    ),
    # test-ems_model.R: "unparseable set definition"
    invalid_set_def = "Unparseable {.field Set} definition detected: {.field {bad_def}}.",
    # test-ems_model.R: "unsupported IF placement"
    invalid_if_placement = c(
      "Unsupported {.field IF} placement detected: {.field {if_statement}}.",
      "{.field IF} terms must enter a {.field Formula} additively at the top level of the right-hand side."
    ),
    # test-ems_model.R: "unsupported IF condition"
    invalid_if_cond = c(
      "Unsupported {.field IF} condition detected: {.field {if_cond}}.",
      "Supported forms: {.field <index> in <set>}, {.field <index> = \"<element>\"}, and {.field <coefficient> <op> <constant>}."
    ),
    # test-ems_model.R: "IF in equation"
    invalid_if_eq = "{.field IF} conditionals in {.field Equation} statements are not supported yet.",
    # test-ems_model.R: "invalid set qualifier"
    invalid_set_qual = "Invalid set qualifier detected: {.field {invalid_qual}}.",
    # not in tests
    set_parse_fail = "Remnant set label detected during Tablo parsing.",
    # test-ems_model.R: "data frame input missing a set"
    injection_missing_col = c(
      "Input for {.field {nme}} is missing required columns.",
      "Required: {.field {req_col}}."
    ),
    # test-ems_model.R: "invalid var in closure"
    no_var = "Closure contains variables not in the model: {.val {var_discrepancy}}.",
    # the following error should never be issued (full will be assigned)
    entry_type = "The following closure entries have not been classified properly: {invalid_entry}.",
    # test-ems_model.R: "closure missing exo/endo spec"
    missing_specification = "The closure must contain both {.val Exogenous} and {.val Rest Endogenous} entries. The inverse approach is not supported.",
    # test-ems_model.R: "ems_model errors when invalid closure mixed entry present preswap"
    mixed_invalid = "{n_invalid_entries} closure entry element{?s} in {.field {cls_entry}} do not belong to the respective variable sets: {invalid_entries}.",
    # test-ems_model.R: "ems_model errors when duplicate closure entry present preswap"
    pre_overlap_ele = "{n_overlap} tuple{?s} for {.val {e}} in the pre-swap closure with multiple entries: {overlap}.",
    # test-ems_model.R: "ems_model errors when invalid closure pure element entry present preswap"
    ele_invalid = "The closure entry tuple {.field {cls_entry}} is invalid under the current set mapping.",
    # test-ems_model.R: "ems_model errors when invalid closure subset entry present preswap"
    subset_invalid = c("Some subsets in {.field {cls_entry}} do not belong to {.field {var_name}}.",
                       "Parent sets include: {.field {var_sets}}."),
    no_name_coeff = "Coefficients to modify must be passed as named pairs: {.code RDLT = 1}."
    )
}

build_model_wrn <- function() {
  list(
    # test-ems_model.R: "ignored tab statement"
    ignored_state = "The following model statements are unsupported and will be ignored: {.field {ign_state}}."
  )
}

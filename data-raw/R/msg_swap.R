build_swap_err <- function() {
  list(
    # test-ems_swap.R: "ems_swap errors when invalid variable provided for swap-in"
    no_var = c(
      "Swap variable {.val {var_name}} not found in the model.",
      "Call: {.code {deparse(call)}}"
    ),
    # test-ems_model.R: "swaps on condensed variables abort"
    condensed_var = c(
      "Swap variable {.val {var_name}} was condensed out of the model ({condense_action}).",
      "Condensed variables cannot enter the closure; drop the condensation action in {.fun teems::ems_model} to swap this variable."
    ),
    # test-ems_swap.R: "ems_swap errors when invalid set provided to swap-in"
    invalid_set = c(
      "Set {.val {non_exist_set}} is not associated with {.val {var_name}}.",
      "Set designations in {.pkg teems} are the uppercase set name and lowercase variable-specific index.",
      "For {.val {var_name}} these include: {.field {ls_mixed}}.",
      "Call: {.code {deparse(call)}}"
    ),
    # test-ems_swap.R: "ems_swap errors when invalid closure element entry swapped in"
    invalid_comp = c(
      "Elements or subsets designated for a swap are not part of the set {.field {nm}}: {invalid_comp}.",
      "Valid elements include {.val {valid_ele}}.",
      "Valid subsets include {.field {valid_subsets}}.",
      "Call: {.code {deparse(call)}}"
    ),
    # test-ems_swap.R: "ems_swap out some ele not exogenous"
    invalid_tup = c(
      "{n_invalid_tuples} tuple{?s} on {.val {var_name}} designated for swap-out are not exogenous: {.field {invalid_tuples}}.",
      "Call: {.code {deparse(call)}}"
    ),
    # test-ems_swap.R: "ems_swap duplicate tup in closure"
    overlap_ele = c(
      "Swap on {.val {var_name}} creates duplicate closure entries: {.field {overlap}}.",
      "Call: {.code {deparse(call)}}"
    ),
    # test-ems_swap.R: "ems_swap out no entry"
    no_var_cls = c(
      "No closure entry for swap-out variable {.val {var_name}}.",
      "Call: {.code {deparse(call)}}"
    ),
    # test-ems_swap.R: "full ems_swap out var not fully exogenous"
    invalid_full = c(
      "{.val {var_name}} cannot be fully swapped out; it is not fully exogenous.",
      "Call: {.code {deparse(call)}}"
    )
  )
}
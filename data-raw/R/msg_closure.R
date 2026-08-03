build_cls_err <- function() {
  list(
    # test-chk_closure.R: "unknown closure variables suggest candidates"
    unknown_var = c(
      "{cli::qty(var_discrepancy)}Closure variable{?s}
      {.val {var_discrepancy}} not found among the model's variables.",
      "Did you mean {.or {.val {candidates}}}?"
    ),
    # test-chk_system_square.R: "unsquared closures abort with arithmetic"
    not_square = c(
      "The closure does not square the system: {n_endo} endogenous
      variable element{?s} against {n_eq_ele} equation element{?s}.",
      "Arithmetic: {n_var_ele} variable elements - {n_exo_ele} exogenous
      elements (closure after swaps) = {n_endo} endogenous; the
      equation system determines exactly {n_eq_ele}, so {gap_abs}
      element{?s} {gap_dir}.",
      "{candidate_txt}",
      "If the counts look right but the partition is structurally
      deficient, run {.fun teems::ems_probe} on the deployed model for
      a named diagnosis."
    )
  )
}

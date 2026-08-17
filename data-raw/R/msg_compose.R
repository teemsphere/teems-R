build_compose_err <- function() {
  list(
    no_sol = "No solution files found at {.path {cmf_path}}; the path is wrong or the model has not been run.",
    set_mismatch = "Tablo-parsed sets/elements do not match binary set outputs.",
    idx_mismatch = "Output variable index mismatch in {.fun teems::ems_compose}.",
    lax_check = "Lax column check failed: one or more parsed variable column names are absent from the tab extract.",
    strict_check = "Strict column check failed: one or more column names are absent or out of order relative to the tab extract.",
    var_check = "Parsed variable names do not match extract names.",
    coeff_check = "One or more Tablo-identified coefficients absent from model output.",
    invalid_coeff_set = "Set not found; a space in a coefficient set declaration may be the cause, e.g. {.code (all, r, REG)} instead of {.code (all,r,REG)}.",
    missing_sets = "Set information missing from binary outputs: {.field {x_sets}}.",
    invalid_name = "{.field {name}} is not present in output variables or coefficients.",
    no_outputs = "No valid variables or model coefficients are availble to output. Ensure that {.fun teems::ems_compose} arguments are compatible.",
    # test-ems_compose.R: "a run without a coefficient dump or CSVs warns and returns variables only"
    no_coefficients = c(
      "No coefficient outputs found for this run; only variables are returned.",
      "The solver image predates the binary coefficient dump ({.file sol.cof}). Update the image, or deploy with {.code write_coefficients = TRUE} for the per-coefficient CSVs."
    )
  )
}

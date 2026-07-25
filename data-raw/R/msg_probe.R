build_probe_err <- function() {
  list(
    # test-ems_probe.R: "ems_probe errors when fine is not a logical scalar"
    x_logical = "{.arg {arg}} must be a logical of length 1.",
    # test-ems_probe.R: "probe report errors when the report is absent"
    no_report = c(
      "No probe report was produced at {.path {probe_path}}.",
      "The structural probe requires a solver image with MC79 support.
      Rebuild the teems image (see the teems-solver README) or inspect
      the solver log at {.path {diag_out}}."
    ),
    # test-ems_probe.R: "pre_probe aborts on a structurally singular system"
    structurally_singular = c(
      "The deployed system is structurally singular on the {pattern}
      pattern: structural rank {rank} of {n}.",
      "{n_under} under-determined variable element{?s}:
      {.val {under_preview}}",
      "{n_over} over-constrained equation element{?s}:
      {.val {over_preview}}",
      "Run {.code ems_probe({.file {cmf_path}})} for the full diagnosis;
      report: {.path {probe_path}}."
    ),
    # test-ems_probe.R: "plot errors on cores without fine data"
    no_fine = c(
      "This probe carries no fine-decomposition (core) data.",
      "Rerun with {.code ems_probe(cmf_path, fine = TRUE)}."
    ),
    # test-ems_probe.R: "plot errors on an unknown type"
    no_defects_dm = c(
      "No Dulmage-Mendelsohn localization to plot: the system has full
      structural rank on both patterns.",
      "The {.val dm} plot only applies to structurally singular systems;
      see {.code plot(x, type = \"incidence\")} for the system structure."
    )
  )
}

build_probe_info <- function() {
  list(
    preflight_ok = "Structural probe: full structural rank {n} of {n};
    the closure is structurally valid.",
    probe_defective = "The system is structurally singular; see
    {.code print()} and {.code plot(x, type = \"dm\")} for the diagnosis."
  )
}

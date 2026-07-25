# probe print methods run

    Code
      print(healthy)
    Message
      -- teems structural probe ------------------------------------------------------
      condensed system: 10524 x 10524
      v structural pattern: full structural rank 10524 of 10524
      v realized pattern (nonzero at base data): full structural rank 10524 of 10524
      fine DM: 6049 strongly connected components — 28 simultaneous cores (>1
      element), largest 4341
      largest core by equation: "e_qfa ×225", "e_qfd ×225", "e_qfm ×225", "e_pfa
      ×225", "e_pfd ×225", and "e_pfm ×225"
      247 equation statements, 856 statement-variable incidences
      ordering evidence: chain none, partition none

---

    Code
      print(broken)
    Message
      -- teems structural probe ------------------------------------------------------
      condensed system: 10530 x 10530
      x structural pattern: structurally singular — rank 10527 of 10530
      (3 unmatched equations,
      3 unmatched variables)
      under-determined by variable: "dprobeb ×3"
      over-constrained by equation: "e_dprobe2 ×3"
      DM blocks: under 0 x 3, well 10524 x 10524, over 6 x 3
      x realized pattern (nonzero at base data): structurally singular — rank 10527 of 10530
      (3 unmatched equations,
      3 unmatched variables)
      under-determined by variable: "dprobeb ×3"
      over-constrained by equation: "e_dprobe2 ×3"
      DM blocks: under 0 x 3, well 10524 x 10524, over 6 x 3
      fine DM: 6049 strongly connected components — 28 simultaneous cores (>1
      element), largest 4341
      largest core by equation: "e_qfa ×225", "e_qfd ×225", "e_qfm ×225", "e_pfa
      ×225", "e_pfd ×225", and "e_pfm ×225"
      249 equation statements, 858 statement-variable incidences

# dm plot errors on a structurally valid probe

    x No Dulmage-Mendelsohn localization to plot: the system has full structural rank on both patterns.
    x The "dm" plot only applies to structurally singular systems; see `plot(x, type = "incidence")` for the system structure.

# cores plot errors without fine data

    x This probe carries no fine-decomposition (core) data.
    x Rerun with `ems_probe(cmf_path, fine = TRUE)`.

# ems_probe errors when cmf_path is missing

    x argument `cmf_path` is missing, with no default

# ems_probe errors when fine is not a logical scalar

    x `fine` must be a logical of length 1.


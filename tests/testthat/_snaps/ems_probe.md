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

# probe prints each condensation verdict

    Code
      for (v in list(list(nbacksolve = 68, nbselems = 2000, bordered = TRUE, ndblock = 35,
        netcut = 400, partition_set = "REG"), list(nbacksolve = 68, nbselems = 2000),
      list(vecsize = 1350000))) {
        .probe_print_condense(do.call(probe_stats_variant, v))
      }
    Message
      condensation: 68 backsolved variables (16% of the uncondensed system), but the
      probe finds a 35-block partition on "REG" (border 400)
      substitution densifies those blocks -- redeploy without `backsolve` and solve
      with a bordered method
      condensation: 68 backsolved variables (16% of the uncondensed system); no
      usable block partition, so this system is "LU"-bound -- the case condensation
      pays for
      condensation: none, and no usable block partition -- this "LU"-bound system is
      a candidate for `ems_model()` `backsolve`


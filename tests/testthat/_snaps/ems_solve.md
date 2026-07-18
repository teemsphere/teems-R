# ems_solve errors when cmf_path is missing

    x argument `cmf_path` is missing, with no default

# ems_solve errors when n_tasks is not integerish

    x `n_tasks` must be integer-like.

# ems_solve errors when steps is not length 3

    x `steps` must be a numeric vector of length 3.

# ems_solve errors when steps are not all even for Gragg

    x `steps` must be all even when `solution_method` is "Gragg".
    i Gragg's method guarantees its accuracy properties for even step counts only (Pearson 1991, Theorem 6.1).

---

    x `steps` must be all even when `solution_method` is "Gragg".
    i Gragg's method guarantees its accuracy properties for even step counts only (Pearson 1991, Theorem 6.1).

# ems_solve errors when steps are not increasing

    x `steps` must be strictly increasing for `solution_method` "Gragg".
    i Richardson extrapolation combines three solutions computed with distinct, increasing step counts.

---

    x `steps` must be strictly increasing for `solution_method` "Euler".
    i Richardson extrapolation combines three solutions computed with distinct, increasing step counts.

# ems_solve errors on invalid Runge-Kutta arguments

    x `steps` must be a single positive integer when `solution_method` is "RK4".
    i Runge-Kutta methods take one step count (e.g. `steps = 8L`); they use no Richardson extrapolation, so no step-count triple is involved.

---

    x `steps` must be a single positive integer when `solution_method` is "DoPri54".
    i Runge-Kutta methods take one step count (e.g. `steps = 8L`); they use no Richardson extrapolation, so no step-count triple is involved.

---

    x `adaptive` "yes" requires an embedded Runge-Kutta `solution_method` ("BoSha32" or "DoPri54").
    i Only the embedded pairs provide the per-step error estimate the adaptive controller acts on.

---

    x `n_subintervals` must be 1 when `solution_method` is "BoSha32".
    i Subintervals restart the integrator and only benefit the extrapolating methods; increase `steps` (or use `adaptive`) instead.

---

    x `eps_tolerance` must be a positive numeric of length 1.

# ems_solve errors when SBBD used with static model

    x `matrix_method` "SBBD" only applicable to intertemporal model runs.

# ems_solve errors when inmemory is not a logical scalar

    x `inmemory` must be a NULL or logical, not a string.

---

    x `inmemory` must be logical of length 1.

# ems_solve errors when verbosity is invalid

    x `verbosity` must be integer-like.

---

    x `verbosity` must be 0, 1, or 2.

# ems_solve warns when poor accuracy

    ! Only 45% of variables accurate to at least 4 digits, below the 80% threshold.
    i Adjust with `accuracy_threshold` in `teems::ems_option_set()`.


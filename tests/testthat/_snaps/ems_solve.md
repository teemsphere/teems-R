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


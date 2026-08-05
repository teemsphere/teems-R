# constructor validation aborts

    x `steps_approx_run` must be a positive integer-like numeric of length 1.

---

    x `steps_approx_run` must be a positive integer-like numeric of length 1.

---

    x `redo_steps` must be a non-missing logical of length 1.

---

    x `redo_step_min_fraction` must be a numeric of length 1 in (0, 1].

---

    x `state_bound_error` must be either "fatal" or "warn".

# both runs disabled aborts

    x `do_approx_run` and `do_acc_run` cannot both be "FALSE".
    i Skipping the approximate run takes the pre-simulation states as the accurate run's targets; skipping the accurate run keeps the approximate solution as the result (GEMPACK manual 51.6). Skipping both leaves nothing to solve.

# ems_solve rejects a non-spec complementarity

    x `complementarity` must be built by `ems_complementarity()`.
    i Example: `complementarity = ems_complementarity(steps_approx_run = 20L)`.


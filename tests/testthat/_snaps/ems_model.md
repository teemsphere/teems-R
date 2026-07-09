# ems_model requires both model_file and closure_file

    x argument `model_file` is missing, with no default

# ems_model requires closure_file when only model_file provided

    x argument `closure_file` is missing, with no default

# ems_model requires model_file when only closure_file provided

    x argument `model_file` is missing, with no default

# ems_model rejects non-character model_file

    x `model_file` must be a character, not a number.

# ems_model rejects non-character closure_file

    x `closure_file` must be a character, not `TRUE`.

---

    x `closure_file` must be a character, not a number.

# ems_model rejects non-existent model_file file

    x Cannot open file 'not_a_file': No such file.

# ems_model rejects non-existent closure_file

    x Cannot open file 'not_a_file': No such file.

# ems_model rejects non-character var_omit

    x `var_omit` must be a NULL or character, not a number.

# ems_model rejects invalid variable names in var_omit

    x "not_a_var" designated for omission not found in the model.

# ems_model rejects invalid coefficient arguments

    x `NOT_A_COEFF` is not declared in the model.

# invalid numeric to a formula

    x Directly assigned numeric values must be length 1.
    i To assign heterogeneous values, use a `data.frame` with the appropriate set columns.

# ignored tab statement

    ! The following model statements are unsupported and will be ignored: Postsim.

# invalid tab statement

    Code
      ems_model(err_model, closure_file)
    Condition
      Error in `ems_model()`:
      x teems version_number does not support Omit statements.
      i Supported statements include: File, Coefficient, Read, Update, Set, Subset, Formula, Assertion, Variable, Equation, Write, and Zerodivide.

# invalid intertemporal header

    x Intertemporal timestep header "YEAR" not found in loaded data.
    i Use `teems::ems_option_set()` `timestep_header` to set a custom timestep header.

# invalid read statement

    x Read statements missing "from file" detected.

# invalid binary set switch statement

    x Unsupported binary switch detected in a Set definition.
    i Declare sets explicitly within the Tablo file or using `...` within `teems::ems_model()`.
    i For example, Set ENDWM # mobile endowment # (capital,unsklab,sklab); not Set ENDWM # mobile endowments # = (all,e,ENDW:ENDOWFLAG(e,"mobile") ne 0);.

# identical set assignment

    x A set appears to be defined as identical to another: Set SET_B # example Set B # = SET_A;.
    i For duplicate sets, use multiple Read statements.
    i For example Set SET_A # example set A # maximum size 5 read elements from file GTAPSETS header "H2"; and Set SET_B # example set B # maximum size 5 read elements from file GTAPSETS header "H2";)

# invalid set qualifier

    x Invalid set qualifier detected: (static).

# partial read statement

    x Partial Read statements are not supported.

# data frame input missing a set

    x Input for SUBPAR is missing required columns.
    i Required: COMMc, REGr, ALLTIMEt, and Value.

# invalid var in closure

    x Closure contains variables not in the model: "not_a_var".

# closure missing exo/endo spec

    x The closure must contain both "Exogenous" and "Rest Endogenous" entries. The inverse approach is not supported.

# ems_model errors when invalid closure mixed entry present preswap

    x 3 closure entry elements in pop("zzz",ALLTIME) do not belong to the respective variable sets: 1: zzz 0, 2: zzz 1, and 3: zzz 2.

# ems_model errors when invalid closure subset entry present preswap

    x Some subsets in qe(COMM,REG,INITIME) do not belong to qe.
    x Parent sets include: ENDWMS, REG, and ALLTIME.

# ems_model errors when invalid closure pure element entry present preswap

    x The closure entry tuple pop("zzz","2") is invalid under the current set mapping.

# ems_model errors when duplicate closure entry present preswap

    x 9 tuples for "pop" in the pre-swap closure with multiple entries: 1: chn 0, 2: chn 1, 3: chn 2, 4: row 0, 5: row 1, 6: row 2, 7: usa 0, 8: usa 1, and 9: usa 2.

# ems_model errors dots passed without names

    x Coefficients to modify must be passed as named pairs: `RDLT = 1`.


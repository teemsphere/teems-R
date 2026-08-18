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

# ems_model rejects non-character omit

    x `omit` must be a NULL or character, not a number.

# ems_model rejects invalid variable names in omit

    x "not_a_var" designated for omission not found in the model.

# ems_model rejects invalid coefficient arguments

    x `NOT_A_COEFF` is not declared in the model.

# invalid numeric to a formula

    x Directly assigned numeric values must be length 1.
    i To assign heterogeneous values, use a `data.frame` with the appropriate set columns.

# unbalanced PostSim markers

    x Unbalanced PostSim section markers: 1 `PostSim (Begin)` against 0 `PostSim (End)` (GEMPACK manual 12.2).

# invalid tab statement

    Code
      ems_model(err_model, closure_file)
    Condition
      Error in `ems_model()`:
      x teems version_number does not support Display statements.
      i Supported statements include: File, Coefficient, Read, Update, Set, Subset, Formula, Assertion, Variable, Equation, Write, Zerodivide, Omit, Substitute, Backsolve, Postsim, Mapping, and Complementarity.

# invalid intertemporal header

    x Intertemporal timestep header "YEAR" not found in loaded data.
    i Use `teems::ems_option_set()` `timestep_header` to set a custom timestep header.

# invalid read statement

    x Read statements missing "from file" detected.

# a set builder on an undeclared/unread coefficient aborts

    x Set builder "ENDWM" conditions on "ENDOWFLAG", which is not Read from an input file.
    i Formula-computed operands cannot drive set resolution (the condition is evaluated ahead of formulas, GEMPACK manual 10.1.2).

# intertemporal set equality

    x Set equality involving an intertemporal set detected: Set ALLTIME2 = ALLTIME.
    i Converting between intertemporal and non-intertemporal sets via set equality is not supported.

# unparseable set definition

    x Unparseable Set definition detected: = ENDWM ENDWS.

# invalid set qualifier

    x Invalid set qualifier detected: (static).

# conditional set builders (GEMPACK manual 10.1.2)

    x Unsupported condition in the Set builder "BADX": "= (all,c,COMM: VDFB(c,\"crops\",\"chn\",\"t0\") > 0 and VDFB(c,\"food\",\"chn\",\"t0\") > 0)".
    i Supported: `(all,i,SRC: COEF(i[,"ele"...]) <op> <constant>)` and `(all,i,SRC: sum{j,S2: MAP(j) = i, COEF2(j)} <op> <constant>)` with <op> one of `= <> < > <= >=` or `eq ne lt gt le ge`; compound conditions are not supported.

---

    x Unsupported condition in the Set builder "BADX": "= (all,c,COMM: VDFB(c,\"crops\",\"chn\",\"t0\") > VDB(c,\"chn\",\"t0\"))".
    i Supported: `(all,i,SRC: COEF(i[,"ele"...]) <op> <constant>)` and `(all,i,SRC: sum{j,S2: MAP(j) = i, COEF2(j)} <op> <constant>)` with <op> one of `= <> < > <= >=` or `eq ne lt gt le ge`; compound conditions are not supported.

---

    x Set builder "BADX" conditions on "VDB", which is not Read from an input file.
    i Formula-computed operands cannot drive set resolution (the condition is evaluated ahead of formulas, GEMPACK manual 10.1.2).

---

    x Set referenced before declaration in "Set BADX = (all,c,NOSET: VDFB(c,\"crops\",\"chn\",\"t0\") > 0)": "NOSET".
    i Sets must be declared before they are used in a definition or Subset statement (GEMPACK manual 10.1).

# a Set built from an excluded coefficient aborts

    x Set "ENDWMX" depends on coefficient "ENDOWFLAG" (header "EFLG"), which is excluded from the data by `full_exclude`.
    i The flag-header approach (GTAP `ENDOWFLAG`/`SLUG`) is not supported through the R package (the solver alone accepts it): declare the set explicitly, e.g. `Set ENDWM (capital, labor);`.

# set products and $POS are rejected by name

    x Set product `x` is not supported: Set UCOM = UNITC x COMM.
    i The upstream GTAPv7 report block builds UACT/UCOM/ALLOCEFF this way (with `$POS` mapping formulas); teems' R-side aggregation supplies the identity, so drop that PostSim block or list the elements explicitly.

---

    x The `$POS` intrinsic is not supported: "Formula (all,c,COMM) UCOM2COMM(c) = $POS(c)"
    i teems' R-side aggregation supplies set-position identities (the upstream GTAPv7 UCOM2COMM/UACT2ACTS report mappings); drop the statement or the PostSim report block.

# expression IF conditions (LULC shape, manual 11.4.5/11.4.6)

    x IF condition references variable "PDS": Equation E_ifbad (all,c,COMM)(all,r,REG)(all,t,ALLTIME) ifbad(c,r,t) = IF[VDB(c,r,t)*pds(c,r,t) > 0, pds(c,r,t)].
    i Conditions are evaluated from coefficient values only (GEMPACK manual 11.4.6/11.4.8).

---

    x Unsupported IF condition detected: VDB(c,r,t) > 0 and VST(c,r,t) > 0.
    i Supported forms: <index> in <set>, <index> = "<element>", and <expression> <op> <expression> over coefficients (no AND/OR/NOT compounds).

# unsupported IF placement

    x Unsupported IF placement detected: Formula (all,r,REG)(all,t,ALLTIME) IFBAD(r,t) = 2 * IF[r in REG, VTRPROV(r,t)].
    i IF terms must enter Formula and Equation statements additively at the top level of an expression.

# multiple membership IF conditions in an equation

    x Set-membership or element IF conditions on different indices detected in one Equation: Equation E_iftest # bad # (all,c,COMM)(all,r,REG)(all,t,ALLTIME) iftest(c,r,t) = IF[c in MARG, qst(c,r,t)] + IF[r in REG, pds(c,r,t)].
    i An Equation supports any number of such conditions on one index (they partition the equation domain); comparison conditions are unrestricted.

# netcut inflation warning (roadmap 6.5 E1)

    ! Multidimensional qfe(ENDW,ACTS,REG,ALLTIME) referenced with a lead or lag in E_nctest.
    i Every element of a lead/lagged variable joins the dense border (netcut) of the bordered matrix methods (SBBD/DBBD/NDBBD); each non-time dimension multiplies the border size.
    i Link periods through a minimal intertemporal proxy instead, e.g. `capital(REG,TIME) = qo("capital",REG,TIME)`, and place the lead/lag on the proxy.

# netcut proxy rewrite (roadmap 6.5 E2)

    Code
      model <- ems_model(fix_model, closure_file)
    Message
      i Inter-period links on element slices rewritten onto minimal intertemporal proxies: NCV1 = qfe("capital","crops",r,t) and NCV2 = qfe("capital","svces",r,t).
      i Proxy variables (NCV*) and their linking equations (E_NCV*) appear in solve outputs.

# partial read statement

    x Partial Read statements are not supported.

# data frame input missing a set

    x Input for SUBPAR is missing required columns.
    i Required: COMMc, REGr, ALLTIMEt, and Value.

# invalid var in closure

    x Closure variable "not_a_var" not found among the model's variables.

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

# backsolve through a coefficient pivot synthesizes a reciprocal and warns

    ! Backsolving qgdp using E_qgdp divides by the coefficient expression GDP(r,t).
    i Ensure this expression can never be zero; a zero value will surface as a solver error.

# in-TAB Substitute executes as backsolve with a message

    Code
      model <- ems_model(sub_model, closure_file)
    Message
      i In-TAB Substitute statement for "tva" executed as backsolve.
      i Backsolved values remain available in solve outputs; plain substitution is not implemented.

# ems_model rejects invalid variable names in backsolve

    x "not_a_var" designated for backsolving not found in the model.

# ems_model rejects invalid equation names in backsolve

    x Equation "E_not_real" nominated for backsolving "qgdp" not found in the model.

# ems_model rejects unresolvable backsolve entries

    x No equation "E_pop" found to backsolve "pop".
    i Unnamed `backsolve` entries resolve their defining equation by the E_<variable> convention.
    i Name the defining equation explicitly: `backsolve = c(pop = "<equation>")`.

# ems_model rejects conflicting condensation actions

    x Variable "tva" appears in more than one condensation action (omit/backsolve).

# ems_model rejects a reused backsolve equation

    x Equation "E_tva" nominated for more than one backsolve.

# backsolve rule violations abort (GEMPACK 14.1.10)

    x Equation E_tr1 cannot be used to backsolve tvr.
    i Occurrence tvr("usa",t): an element occurs as an argument; every argument must be an index (requirement 1).
    i GEMPACK substitution requirement: see the GEMPACK manual, section 14.1.10.

---

    x Equation E_tr2 cannot be used to backsolve tvr.
    i Occurrence sum{r,REG, tvr(r,t)}: a SUM index occurs as an argument; every index must be an equation ALL index (requirement 2).
    i GEMPACK substitution requirement: see the GEMPACK manual, section 14.1.10.

---

    x Equation E_tr3 cannot be used to backsolve tvc3.
    i Equation ALL index (r) absent from occurrence tvc3(t); every equation ALL index must appear in each occurrence (requirement 3).
    i GEMPACK substitution requirement: see the GEMPACK manual, section 14.1.10.

---

    x Equation E_tr4 cannot be used to backsolve tvm.
    i Occurrence tvm(m,t) ranges over {MARG,ALLTIME} but the variable is declared over {COMM,ALLTIME}; every index must range over the full declared set (requirement 4).
    i GEMPACK substitution requirement: see the GEMPACK manual, section 14.1.10.

---

    x Equation E_tr5 cannot be used to backsolve tvrr.
    i Occurrence tvrr(r,r,t): a repeated index; all indices of one occurrence must be different (requirement 5).
    i GEMPACK substitution requirement: see the GEMPACK manual, section 14.1.10.

---

    x Equation E_tr6 cannot be used to backsolve tvr.
    i Occurrence tvr(r,t+1): an argument carries a lead/lag offset; offsets block substitution in intertemporal models (requirement 6).
    i GEMPACK substitution requirement: see the GEMPACK manual, section 14.1.10.

---

    x Equation E_tr7 cannot be used to backsolve tvrr.
    i Occurrences tvrr(r,s,t) and tvrr(s,r,t) have different index patterns; all occurrences must share one pattern (requirement 7).
    i GEMPACK substitution requirement: see the GEMPACK manual, section 14.1.10.

---

    x Equation E_trc cannot be used to backsolve tvr.
    i The occurrences of the variable cancel; no expression for it can be obtained from this equation.
    i GEMPACK substitution requirement: see the GEMPACK manual, section 14.1.10.

# backsolved variables must be endogenous in the closure

    x Backsolved "pop" is exogenous in the closure.
    i Substituted-out variables must be endogenous; swap out of the closure or drop the backsolve.

# omitted variables must be exogenous in the closure

    x Omitted "qgdp" is not exogenous in the closure.
    i Omitted variables must be exogenous and unshocked (GEMPACK manual, section 14.1).

# swaps and shocks on condensed variables abort

    x Swap variable "atall" was condensed out of the model (omit).
    i Condensed variables cannot enter the closure; drop the condensation action in `teems::ems_model()` to swap this variable.

---

    x Shock variable "atall" was condensed out of the model (omit).
    i Omitted variables must stay unshocked and backsolved variables are endogenous; drop the condensation action in `teems::ems_model()` to shock this variable.

# ems_model rejects a non-logical auto_omit

    x `auto_omit` must be "TRUE" or "FALSE".


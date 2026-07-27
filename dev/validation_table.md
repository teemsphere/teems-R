# TEEMS validation table

Single source of truth for the R-side pre-flight validation program
(prepared 2026-07-26 from the solver fatal-error sweep, the eight
`.audit/*-test-kit` negative-leg catalogs, and the A(d) fuzz crash
catalog's plausible-mistake subset).

**Implementation status (updated 2026-07-27):**
- Phase 2 DONE — `chk_solver_log.R` maps `Error:` lines via
  `solver_error_map` (sysdata) to solver_tab/closure/data/numeric
  aborts quoting the lines verbatim + manual sections;
  `solution_sing` routes to `ems_probe()`/`pre_probe`
  (test-chk_solver_log.R, 19 tests).
- Phase 3a DONE for N1-N9, Q1-Q5, B1, D1-D5 (+`default_unsupported`:
  solver-valid Defaults abort — the R pipeline cannot carry positional
  Default semantics), PS1-PS8 (PS Reads now allowed in `ps_allowed`),
  U1-U4 — `chk_tab_preflight.R`, raw-statement checks in
  `.chk_raw_statements` (parsers crash on these forms), rest at end of
  `.process_tablo()` (test-chk_tab_preflight.R, 31 tests).
- Phase 3b DONE for C1 (cls_err$unknown_var + adist candidates), C4
  (cls_err$not_square in `.check_system_square`, full arithmetic +
  gap candidates + probe routing), C8 (via solution_sing/pre_probe).
  C5 covered by existing swap checks + C4 counts; C6/C7 already in
  shock constructors. RIDE-ALONG FIX: `.eval_set_expr` `-` was
  row-level on origin/mapping pairs — `NMRG = COMM - MARG` kept the
  margin commodity (5 vs the solver's 4 elements); now element-level
  per 10.1.1.1 (test-set_expr.R).
- Phase 3c DONE (2026-07-27): (a) gated e2e negative legs
  (test-solver_err_e2e.R, 6 legs) push probe-mutated GTAPv7 models
  through real `ems_solve()` runs for the solver-only rows — Z1/Z2
  zerodivide, Z3 assertion, B2 fatal range test, I2/I3 intrinsic
  arity — exercising the Phase-2 `Error:` mapping end to end
  (requires a teems image ≥ 2026-07-26; skips otherwise). (b) curated
  fuzz corpus: 20 single-defect fixtures in
  `tests/testthat/fixtures/tab/` (provenance in its README), each
  aborting with a named message at `.process_tablo()` — never a raw
  parser crash — pinned in one combined snapshot
  (test-tab_fuzz_corpus.R). RIDE-ALONGS: `stmt_missing_equals` —
  unknown leading keywords are folded into the preceding statement as
  implicit continuations, so they surface as a math statement without
  `=` and used to crash the extract parsers; now a named abort. And
  the B-row qualifier regexes accept `le`/`lt` plus negative/decimal
  bound values (`tab_qual` in data-raw).
- OPEN: S-rows (set expression checks) R-side; C9 solver-side
  closure_read/shocks_read fail-fast sweep; '+'/'&' element-level
  set-expression semantics decision.

**Layer rule (single-source-of-truth):** a check is `R` (pre-flight)
ONLY if it is decidable from what teems-R already builds — the
`.process_tablo()` statement tibble, the loaded `sets` object, or the
closure/swap/shock inputs. No TAB *expression semantics*, no binary
data reads. The solver stays authoritative in every case: `both`
means R aborts early with a better message AND the solver still
enforces. R pre-flight failures are **hard `cli_abort`s** (user
decision 2026-07-26) — failing before the Docker round-trip is the
point.

**Layers:** `R` = R pre-flight only sensible place (solver is a wart
or has no check) · `both` = R pre-flight + solver fatal ·
`solver` = solver-only (data-dependent or needs expression
semantics); surfaced in R via the Phase-2 log mapping ·
`log-map` annotation = row is covered by the `chk_solver_log.R`
"Error:" mapping even when no pre-flight exists.

**R hook points** (from the infra survey):
- statement-level checks: after `.process_tablo()` in
  `.implement_model()` (`R/model_implement.R:13`), alongside
  `.check_closure()` (`:33`).
- deploy-time checks needing sets/closure/counts: in `.finalize()`
  (`R/finalize.R`) after `.compute_size_metadata()` (`:42`).
- messages: `data-raw/R/msg_*.R` -> `data-raw/teems_data-raw.R` ->
  `R/sysdata.rda` (rerun the script; new categories added in both the
  build block ~`:262` and `use_data()` ~`:282`).

**Mirrored-data maintenance note:** three vocabularies must be
mirrored from solver source as R data (comment the origin line in
`data-raw`): reserved words (`tab_parse.c:1620`, 26 words), the
qualifier vocabulary (`tab_qualifiers_parse`, `tab_parse.c:~2200`),
and the Default-value vocabulary (`tab_defaults_validate`,
`cmf_io.c:~1250`). Solver buffer cap NAMESIZE=256; the user-facing
name-length rule is the manual 11.2.1 length table (look up exact
limits at implementation time).

---

## N — Names (manual 11.2.1)

Solver: `names_validate` (`tab_parse.c:1626-1657`), first-offender
fatal. R: **new** `chk_names.R` over the statement tibble
(`type`/`name` columns, case-folded). Kit: `names-test-kit`.

| ID | Invariant | Trigger example | Solver msg (loc) | Layer | R msg | Test source |
|---|---|---|---|---|---|---|
| N1 | No name declared as both coefficient and variable (case-insensitive) | `Coefficient PSAVE` vs `Variable psave` | `name %s is declared as both a coefficient and a variable...` (tab_parse.c:1626) | both | `model_err$name_coef_var` | kit `coefvar`; snapshot |
| N2 | No name declared as both coefficient and set | `Coefficient REG` vs set REG | tab_parse.c:1630 | both | `model_err$name_coef_set` | kit `coefset` |
| N3 | No name declared as both variable and set | `Variable marg` vs set MARG | tab_parse.c:1644 | both | `model_err$name_var_set` | kit `varset` |
| N4 | No duplicate coefficient declaration | `Coefficient DUPX` twice | tab_parse.c:1634 | both | `model_err$name_dup` | kit `dupcoef` |
| N5 | No duplicate variable declaration | | tab_parse.c:1648 | both | `model_err$name_dup` | new snapshot |
| N6 | Coefficient/variable/set names not reserved words | `Coefficient MAX` | tab_parse.c:1638/1652/1657 | both | `model_err$name_reserved` | kit `reserved` |
| N7 | `c_`/`C_` prefix reserved for change variables | `Coefficient c_foo` | tab_parse.c:2597 | both | `model_err$name_c_prefix` | new snapshot |
| N8 | Coef and var must not share a base name after `p_`/`c_` stripping | `Coefficient foo` + `Variable p_foo` | main.c:1124 | both | `model_err$name_prefix_clash` | new snapshot |
| N9 | Name length within manual 11.2.1 limits | 300-char set name (fuzz class: over-length names) | buffer-guarded, no named msg | R | `model_err$name_too_long` | fuzz catalog; snapshot |

## Q — Declaration qualifiers (manual 10.3)

Solver: `tab_qualifiers_parse` (`tab_parse.c:2200-2282`). R: extend
statement parsing — `qualifier_list` column vs mirrored vocabulary.
Kit: `quals-test-kit`.

| ID | Invariant | Trigger example | Solver msg (loc) | Layer | R msg | Test source |
|---|---|---|---|---|---|---|
| Q1 | Every qualifier token in the known vocabulary | `Variable (foo) x;` | `unknown %s qualifier '%s'` (tab_parse.c:2282) | both | `model_err$qual_unknown` | kit `unknown` |
| Q2 | `NO_SPLIT` unsupported | `Variable (no_split) x;` | tab_parse.c:2241 | both | `model_err$qual_no_split` | kit `nosplit` |
| Q3 | `LINEAR_NAME=`/`LINEAR_VAR=` unsupported | `Variable (levels, linear_name=xlin) x;` | tab_parse.c:2245 | both | `model_err$qual_linear_name` | kit `linname` |
| Q4 | No empty `()` qualifier | `Variable () x;` | tab_parse.c:2216 | both | `model_err$qual_empty` | new snapshot |
| Q5 | Balanced parens in qualifier list | | tab_parse.c:2200 | both | `model_err$qual_unbalanced` | new snapshot |

## D — Default statements (manual 10.19)

Solver: `tab_defaults_validate` (`cmf_io.c:1256-1272`), accumulates
all offenders. Kit: `defaults-test-kit`.
**PREREQUISITE:** `Default` is NOT in `supported_state`
(`data-raw/teems_data-raw.R:35`) — teems-R currently aborts on ALL
Default statements via `model_err$unsupported_tab`, including ones the
solver accepts. Fix = add `Default` to `supported_state` + implement
D1-D5 value validation.

| ID | Invariant | Trigger example | Solver msg (loc) | Layer | R msg | Test source |
|---|---|---|---|---|---|---|
| D1 | `Equation (default=levels)` unsupported | | cmf_io.c:1267 | both | `model_err$default_levels` | kit `eqlevels` |
| D2 | `Equation (default=add_homotopy)` unsupported | | cmf_io.c:1269 | both | `model_err$default_homotopy` | new snapshot |
| D3 | Coefficient bound defaults unsupported | `Coefficient (default=lower_bound ge 0);` | cmf_io.c:1256 | both | `model_err$default_bound` | kit `coefbnd` |
| D4 | Default value in the per-keyword vocabulary | `Variable (default=foo);` | cmf_io.c:1257/1260/1263/1270 | both | `model_err$default_unknown` | kit `unknown` |
| D5 | Default only on Coefficient/Variable/Formula/Equation | `Update (default=always);` | cmf_io.c:1272 | both | `model_err$default_keyword` | kit `badkw` |

## B — Bounds (manual 10.19.1 / 25.4.4)

Solver: slot-2 machinery (`tab_parse.c:2271`, `formula.c:698/2124`).
Kit: `bounds-test-kit`, `quals-test-kit` `dblbound`.

| ID | Invariant | Trigger example | Solver msg (loc) | Layer | R msg | Test source |
|---|---|---|---|---|---|---|
| B1 | At most one lower (GE/GT) + one upper (LE/LT) bound per declaration | `Coefficient (ge 0, ge 1) X;` | `duplicate %s bound on a %s declaration...` (tab_parse.c:2271) | both | `model_err$bound_dup` | kit `dup` |
| B2 | Coefficient values within declared bounds (CMF `range test ... = yes`) | value 20 vs `(le 10)` | formula.c:698 (fatal at 2124 when mode=2, else Warning) | solver (log-map) | `solve_err$range_violation` | kit `upper`/`dblbound` |

## PS — PostSim sections (manual 12.2.1-12.2.3)

Solver: `tab_postsim_split` (`cmf_io.c:1414-1487`),
`postsim_reads_execute` (`tab_parse.c:1713-1728`), PS-LHS
(`formula.c:1883/1887`). R: PS1 already DONE
(`model_err$postsim_invalid`, `tablo_process.R:200`); `ps_decl_names`
already captured (`tablo_process.R:22-36`) — PS2-PS8 are statement-
table/name-level. Kit: `postsim-test-kit`, `names-test-kit` `popvar`.

| ID | Invariant | Trigger example | Solver msg (loc) | Layer | R msg | Test source |
|---|---|---|---|---|---|---|
| PS1 | Only set/subset/coefficient/file/formula/assertion/zerodivide/read in PS sections | `Variable` in PS block | cmf_io.c:1434 | both (**R DONE**) | `model_err$postsim_invalid` | test-postsim.R:68 |
| PS2 | Ordinary statements must not reference PS-declared names | ordinary `Formula ORDX = PSCALC + 1;` | cmf_io.c:1414 | both | `model_err$postsim_scope` | kit `scope`, `popvar` |
| PS3 | A data file read in ordinary part must not be read in PS part | PS `Read ... from file GTAPDATA` | cmf_io.c:1487 | both | `model_err$postsim_same_file` | kit `samefile` |
| PS4 | PS Read target must be a PS coefficient (not ordinary) | PS `Read SAVE from file PSDATA` | tab_parse.c:1713 | both | `model_err$postsim_read_ord` | kit `readord` |
| PS5 | PS Read target must not be a variable | PS `Read psave ...` | tab_parse.c:1723 | both | `model_err$postsim_read_var` | kit `readvar` |
| PS6 | PS Read target must be declared | | tab_parse.c:1728 | both | `model_err$postsim_read_undecl` | new snapshot |
| PS7 | PS Formula LHS must not be a variable | PS `Formula psave(r) = 1;` | formula.c:1883 | both | `model_err$postsim_lhs_var` | kit `lhsvar` |
| PS8 | PS Formula LHS must not be an ordinary coefficient | PS `Formula SAVE(r) = 1;` | formula.c:1887 | both | `model_err$postsim_lhs_ord` | kit `lhsord` |

## S — Sets (manual 10.1.1.1 / 10.1.2.1)

Solver: set readers/builders (`tab_parse.c:3343-4117`). R layering:
name-level and textual rows pre-flight from the statement tibble;
element-level rows pre-flight **at deploy** where `sets$ele` exists
(`.finalize()`); pure size-semantics rows stay solver-only. Fuzz
plausible-mistake classes: self-ref exprs, difference direction,
empty/inverted intertemporal ranges, empty element lists.

| ID | Invariant | Trigger example | Solver msg (loc) | Layer | R msg | Test source |
|---|---|---|---|---|---|---|
| S1 | Set must not reference itself in its defining expression | `Set A = A + B;` | tab_parse.c:3679 etc. | both | `model_err$set_self_ref` | fuzz; snapshot |
| S2 | Sets referenced in definitions/subsets must be declared first | `Set NMRG = COMM - MRG;` (MRG undeclared) | tab_parse.c:3473/3881/4111/4116 | both | `model_err$set_undeclared` | fuzz; snapshot |
| S3 | `Set A = A;` self-equality invalid | | tab_parse.c:3581 | both | `model_err$set_self_eq` | fuzz; snapshot |
| S4 | Set-equality RHS must be a declared set (no products/data-dependent) | `Set A = B x C;` | tab_parse.c:3596 | both | `model_err$set_eq_rhs` | new snapshot |
| S5 | Element-range abbreviation `(first - last)` unsupported | | tab_parse.c:3645 | both | `model_err$set_ele_range` | new snapshot |
| S6 | Set difference must not subtract a larger set | `COMM - MARG` with MARG ⊄ COMM | tab_parse.c:3519 | both (deploy) | `model_err$set_diff_dir` | fuzz; snapshot |
| S7 | `+` operands disjoint; `-` removes only present elements | | tab_parse.c:3904/3915 | both (deploy) | `model_err$set_op_elems` | new snapshot |
| S8 | Intertemporal time range non-empty, not inverted, within cap | `p[5 - 2]` | tab_parse.c:3343/3350 | both (deploy; CYRS) | `model_err$set_int_range` | fuzz; snapshot |
| S9 | Element lists non-empty and well-formed | `Set A (a1,);` / empty list | main.c:1013/1021 | both | `model_err$set_ele_list` | fuzz; snapshot |
| S10 | Header in set declaration within HEADERSIZE (4 chars) | | tab_parse.c:3629 | both | `model_err$set_header_len` | new snapshot |
| S11 | Subset elements contained in claimed superset | | tab_parse.c:4117 | both (deploy) | `model_err$subset_not_contained` | new snapshot |
| S12 | Set expression fits declared size; malformed set decl forms | | tab_parse.c:3682/3859/3869/3962/4041 | solver (log-map) | `model_err$solver_reported` | Phase-2 mapping |

## I — Intrinsic functions (manual 11.5)

Solver: formula compiler (`formula.c:442-553`). Arity needs expression
parsing -> solver-only per the layer rule; `$POS` is a textual scan.
Kit: `intrinsics-test-kit`.

| ID | Invariant | Trigger example | Solver msg (loc) | Layer | R msg | Test source |
|---|---|---|---|---|---|---|
| I1 | `$POS` unsupported | `$POS(r)` in a formula | formula.c:442 | both (textual) | `model_err$pos_unsupported` | kit `pos` |
| I2 | `ID0V` takes exactly 2 args | `ID0V(1,2,3)` | formula.c:549 | solver (log-map) | — | kit `id0v3` |
| I3 | `MAX`/`MIN` take >= 2 args | `MAX(5)` | formula.c:553 | solver (log-map) | — | kit `maxone` |
| I4 | Intrinsic arg count <= 16, no empty args | | formula.c:516/521 | solver (log-map) | — | Phase-2 mapping |

## U — Unsupported-by-design / statement forms

R: `.check_statements()` already rejects `invalid_state`
(Loop/Display/Break/Mapping/Cycle/Complementarity/Transfer). These
rows extend the same scan. Kit: `legacyq-test-kit`.

| ID | Invariant | Trigger example | Solver msg (loc) | Layer | R msg | Test source |
|---|---|---|---|---|---|---|
| U1 | `Formula & Equation` unsupported (needs levels equation, 10.9.1) | | formula.c:1646 | both | `model_err$formula_equation` | kit `feq` |
| U2 | `Read ... from terminal` unsupported | | tab_parse.c:510 | both | `model_err$read_terminal` | new snapshot |
| U3 | Every Read must carry a header | `Read (IfHeaderExists) ELX from file GTAPDATA;` | tab_parse.c:537/883 | both | `model_err$read_no_header` | kit `nohdr` |
| U4 | Read target must be a declared coefficient/parameter | | tab_parse.c:861/1259 | both | `model_err$read_undeclared` | new snapshot |
| U5 | `Omit`/`Substitute` must not reach the solver (resolved by `ems_model()`) | | tab_parse.c:4556/4564 | R (by construction) | existing condense path | test-ems_model.R |

## E — Equations / backsolve (manual 14.1.x)

R already validates the eight 14.1.10 substitution requirements at
condensation time (commit f3b9e4f); solver re-validates
(`tab_parse.c:4496-4643`, `jacobian.c:1013-1108`). No new R work;
rows retained for log-mapping completeness.

| ID | Invariant | Layer | Test source |
|---|---|---|---|
| E1 | Backsolve nomination rules (declared var/eq, 1:1, endogenous, eliminated everywhere) | both (**R DONE**) | condensation tests |
| E2 | Undeclared variable referenced in an equation (`p_` token with no declaration) | solver (log-map) — needs expression tokenization; revisit as R candidate later | fuzz; Phase-2 mapping |
| E3 | Zero pivot backsolving / defining-equation singular at a step | solver (log-map) | Phase-2 mapping |

## Z — Zerodivide / assertions / runtime formula errors

Data-dependent -> solver-only; the Phase-2 log mapping is the R
surface. Kit: `zdiv-test-kit`.

| ID | Invariant | Solver msg (loc) | Layer | Test source |
|---|---|---|---|---|
| Z1 | No 0/0 in formulas while `Zerodivide (zero_by_zero)` off | formula.c:878 | solver (log-map) | kit `zbzoff` |
| Z2 | No nonzero/0 while `nonzero_by_zero` off | formula.c:884 | solver (log-map) | kit `nbzoff` |
| Z3 | Assertions hold (`Assertions = yes`); `# message #` preserved | formula.c:3191 | solver (log-map) | kit `values` leg |
| Z4 | No fractional power of a negative number | formula.c:1092 | solver (log-map) | Phase-2 mapping |

## C — CLOSURE / SWAP / SHOCK (user requirement 2026-07-26)

**Solver-side status is the strongest argument for this section:**
`closure_read` (`tab_parse.c:1765-1928`) is entirely
print-and-continue — every diagnostic is a wart, the fopen is
unchecked, and a malformed closure solves silently wrong.
`shocks_read` is mostly fail-fast but its fopen wart falls through on
a NULL handle and scalar shocks on undeclared variables are silently
dropped. **R pre-flight is therefore authoritative for closure
hygiene**, with the solver sweep recorded as follow-up (row C9).

New R category: **`cls_err`** (does not exist yet — add to
`data-raw`). USER REQUIREMENT: every cls_err names the offending
variables/elements AND renders a **candidate list** (cli bullets) of
probable fixes.

| ID | Invariant | Layer | R msg + candidate source | Existing infra | Test source |
|---|---|---|---|---|---|
| C1 | Every closure entry names a declared variable | R (solver WART tab_parse.c:1825/1928) | `cls_err$unknown_var` — candidates = nearest matches by `utils::adist` over declared variable names | `.check_closure` `model_err$no_var` (migrate) | new snapshots |
| C2 | Closure entry index sets/elements valid for the variable | R (solver WART :1849-1897) | `cls_err$bad_component` — candidates = the variable's actual sets/elements | `.classify_cls`/`.exp_cls_entry` (`model_err$entry_type`) | new snapshots |
| C3 | No duplicate/overlapping exogenous tuples | R (no solver check) | existing `model_err$pre_overlap_ele` (consider migrate to cls_err) | `.validate_closure` (`R/val_closure.R:6`) | existing |
| C4 | **Count squaring**: n_exo_ele == n_var_ele - n_eq_ele (system square) | R (solver: unnamed downstream MA48 failure) | `cls_err$not_square` — message states the arithmetic (n_var_ele, n_eq_ele, n_exo_ele, gap); candidates = variables whose element counts equal/sum to the gap, conventional swap-partners first | `.compute_size_metadata` (`R/size_metadata.R:5`) has n_var_ele/n_exo_ele; **n_eq_ele must be added** (prod of equation quantifier set sizes from `ls_upper_idx`) | new snapshots + gated e2e |
| C5 | Swap pairs consistent: swap_in currently endogenous, swap_out currently exogenous, both exist, element counts match | R | `swap_err$*` extensions — candidates = current status of each side + count arithmetic | `.check_swap`, `.finalize_closure` overlap checks | new snapshots |
| C6 | Shocked variables/elements actually exogenous after swaps | R (solver silently ignores or misapplies) | `shk_err$not_exogenous` — candidates = the variable's exogenous subset (or its swap route) | `.check_shock` (exists: existence+tuples; exogeneity NOT checked) | new snapshots |
| C7 | Scalar shock on undeclared variable | R (solver WART: silent drop) | `shk_err$not_a_var` **already covers** | `chk_shock.R:13` | existing |
| C8 | Square-but-singular closure (wrong partition) | route to probe | `ems_solve(pre_probe=)` named abort + `ems_probe()` guidance; cls_err count/name failures should point to pre_probe when counts pass | probe infra DONE (4d209f2) | test-ems_probe.R |
| C9 | **Solver follow-up** (out of R scope): closure_read fail-fast sweep (6 warts + unchecked fopen), shocks_read fopen wart + scalar silent-drop | solver | — | recorded in solver memory | solver kits later |

## X — CLI / solver configuration

`main.c:723-1351` CLI validation is unreachable from teems-R (R
constructs commands and already validates via `val_solver_args.R`);
matrix-method structure errors (DBBD/NDBBD/SBBD partition/chain
requirements, main.c:1273-1351) are data/structure-dependent ->
solver (log-map) only.

---

## Phase-2 design note (chk_solver_log.R)

Current: greps `singular` / `error` -> two generic aborts
(`chk_solver_log.R:11/18`). Target:
1. Collect ALL `Error:`-prefixed lines from the diag log (the solver
   convention is uniform after the fail-fast sweeps).
2. Match against a patterns table (data-raw data object:
   `solver_error_map` — regex, category, R message name, manual
   section) built from the rows above marked `log-map`; matched lines
   get a categorized cli_abort with the solver line quoted verbatim
   and the manual section rendered as info.
3. Unmatched `Error:` lines fall back to the current generic
   `solve_err$solution_err` with the lines quoted.
4. Keep the `singular` grep as the pre-probe routing trigger
   (C8: suggest `ems_probe()`/`pre_probe=TRUE` in the abort).

## Phase-3 priority order (user-confirmed)

1. Names / qualifiers / defaults / PostSim (the custom-model
   `ems_model()` + user `.tab` path is where malformed input actually
   arrives; TEEMS-generated TABs are well-formed by construction).
2. Closure section C1-C6 (cls_err category + candidate lists).
3. Sets / reads / unsupported forms.
4. Negative corpus: (a) kit legs as gated e2e (test-postsim.R
   pattern); (b) curated reduced fuzz inputs as small text fixtures
   expecting model_err BEFORE any solver call.

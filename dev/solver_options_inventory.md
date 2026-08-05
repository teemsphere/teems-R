# Solver interface inventory: every knob vs. its R surface

Compiled 2026-08-04 (updated 2026-08-05; solver @ c6e7e80) by
grepping every `PetscOptionsGet*` call and every CMF statement parser
in the solver, cross-referenced against what `ems_solve()` /
`ems_probe()` actually pass. Purpose: each row should get an explicit
disposition — a designed R surface, a documented escape hatch, or
removal — instead of existing by historical accident. Dispositions
marked (?) are open decisions.

DECIDED 2026-08-05 (the CMF-vs-record discussion): the CMF stays a
FILE MANIFEST — run controls never go into it. Controls travel on the
solver invocation (CLI flags); the POSTERITY RECORD of the effective
configuration (defaults, validation and forced changes applied) is
the `options` object the solver writes into `sol.stats.json`,
rendered into `model_diagnostics.txt` by R after every solve. Section 1 is now FULLY MIGRATED (no CMF statement parsers remain in
the solver — the CMF is purely a file manifest). The six
complementarity controls were the first fully-plumbed example:
`ems_complementarity()` spec → `ems_solve(complementarity = )` →
`-comp_*` flags → stats.json record → diagnostics appendix. The
complementarity CMF statements were REMOVED (09bf33b) and the
assertions/range-test/postsim statements followed (solver c099d5f,
same pattern: flags + `ems_solve()` named args + record).

## 1. CMF statements the solver parses that R NEVER writes

These are reachable today only by hand-editing the deployed `.cmf`.
None have any R plumbing.

| Statement | Parser | Default | Meaning | Disposition |
|---|---|---|---|---|
| ~~`Assertions = …`~~ | REMOVED c099d5f | fatal | now `-assertions 0\|1\|2` | **DONE**: `ems_solve(assertions = "fatal"\|"warn"\|"off")`; recorded |
| ~~`range test initial values = …`~~ | REMOVED c099d5f | warn | now `-range_test_initial 0\|1\|2` | **DONE**: `ems_solve(range_test_initial = )`; recorded |
| ~~`range test updated values = …`~~ | REMOVED c099d5f | warn | now `-range_test_updated 0\|1\|2` | **DONE**: `ems_solve(range_test_updated = )`; recorded |
| ~~`postsim = yes\|no ;`~~ | REMOVED c099d5f | yes | now `-postsim 0\|1` | **DONE**: `ems_solve(postsim = )`; recorded |
| ~~`complementarity …` (six statements)~~ | REMOVED 09bf33b | — | now `-comp_steps`/`-comp_redo`/`-comp_redo_min_frac`/`-comp_do_approx`/`-comp_do_acc`/`-comp_sberr_warn` CLI flags | **DONE**: `ems_complementarity()` → `ems_solve(complementarity = )`; effective values recorded in stats.json + model_diagnostics.txt |

Note: `zerodivide ... ;` statements are TAB statements (model text),
not CMF — they belong to the model author and are out of scope here.

## 2. Solver CLI flags vs. `ems_solve()` / `ems_probe()`

### Exposed through named R arguments (designed path)

| Flag | R argument |
|---|---|
| `-solmed` | `solution_method` |
| `-matsol` | `matrix_method` |
| `-step1/2/3` | `steps` |
| `-nsubints` | `n_subintervals` |
| `-adaptive` | `adaptive` |
| `-epstol` | `eps_tolerance` |
| `-laA` / `-laDi` / `-laD` | `laA` / `laDi` / `laD` |
| `-inmemory` | `inmemory` |
| `-verbosity` | `verbosity` |
| `-n` (mpiexec ranks) | `n_tasks` |
| `-probefine` | `ems_probe(fine = )` |
| `-cmdfile`, `-nox` | internal (always passed) |

### Accepted by the solver but NOT exposed (reachable only via `append_args`)

| Flag | Default | Meaning | Disposition |
|---|---|---|---|
| `-maxthreads` | 1 (R hardcodes 1) | OpenMP threads per rank | (?) |
| `-smllthreads` | = maxthreads | OpenMP threads for small sections | (?) |
| `-fastrefac` | 0 (off) | persistent-pivot refactorization (adoption plan 0bdd621; force-cleared for complementarity runs) | (?) |
| `-gpzerodivide` | 0 (legacy) | GEMPACK dual-class ZERODIVIDE semantics (parity plan A1; adoption = re-anchor-class change) | (?) |
| `-maxretries` | driver default | RK adaptive: retry cap | (?) — `adaptive`/`eps_tolerance` exposed, retry tuning not |
| `-retryadj` | driver default | RK adaptive: step-shrink factor on retry | (?) |
| `-cntl_3` | HSL default | MA48 iterative/pivot threshold | (?) — expert HSL knob |
| `-cntl_6` | HSL default | ordering CNTL(6) threshold | (?) — expert HSL knob |
| `-nsbbdblocks` | derived | SBBD block-count override | (?) |
| `-withmc66` | build/default | MC66 ordering toggle for SBBD | (?) |
| `-nowrites` | 0 | suppress solver-side coefficient dumps (distinct from R's `suppress_outputs`, which only skips R-side composition) | (?) |
| `-tempdir` | scratch default | solver scratch directory override | (?) |

## 3. Suggested buckets (recommendations only, nothing decided)

- **Designed surface** (DONE for all of section 1): the six
  complementarity controls via `ems_complementarity()`, and
  `assertions`/`range_test_initial`/`range_test_updated`/`postsim` as
  `ems_solve()` named arguments.
- **Documented escape hatch**: expert HSL/ordering knobs (`-cntl_3`,
  `-cntl_6`, `-nsbbdblocks`, `-withmc66`) and performance toggles
  (`-fastrefac`, `-maxthreads`, `-smllthreads`) — document
  `append_args` as the supported route, or promote individually.
- **Review for exposure or removal**: `-nowrites` vs
  `suppress_outputs` overlap; `-tempdir` vs the R-managed scratch;
  `-gpzerodivide` (parity-plan A1 says adoption is a deliberate
  re-anchor decision, not a flag users should casually flip);
  `-maxretries`/`-retryadj` (arguably belong next to `adaptive`).

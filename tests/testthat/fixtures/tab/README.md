# Curated TAB fuzz corpus

Reduced plausible-mistake TAB inputs distilled from the solver fuzzing
campaigns (teems-solver A(d) crash catalog) and the solver fatal-error
sweep, per `dev/validation_table.md` Phase 3c(b). Each fixture is a
small standalone model carrying exactly one defect and must abort with
a named teems error during `.process_tablo()` — before any solver
call — never with a raw parser error. Consumed by
`test-tab_fuzz_corpus.R`; messages are pinned in a combined snapshot.

Formatting is deliberately hostile in places (CRLF endings, mixed-case
keywords, `!` comments, two statements per line, missing trailing
newline) — the corpus checks the pre-flight path tolerates fuzz-shaped
input, not just clean strings.

| Fixture | Row (validation_table.md) | Defect |
|---|---|---|
| name_coef_var_clash | N1 | coefficient/variable name clash (case-insensitive) |
| name_coef_set_clash | N2 | coefficient/set name clash |
| name_reserved | N6 | reserved intrinsic name declared |
| name_dup | N4 | duplicate coefficient declaration |
| name_c_prefix | N7 | `c_` prefixed coefficient |
| name_overlength | N9 | 300-char identifier (fuzz class) |
| qual_unknown | Q1 | mistyped declaration qualifier |
| qual_empty | Q4 | empty `()` qualifier list |
| qual_linear_name | Q3 | `linear_name=` qualifier |
| bound_dup | B1 | duplicate lower bound |
| default_positional | D (pipeline) | solver-valid positional Default |
| default_eq_levels | D1 | `Equation (default=levels)` |
| read_no_header | U3 | headerless Read |
| read_terminal | U2 | Read from terminal |
| read_undeclared | U4 | Read into undeclared name |
| formula_and_equation | U1 | `Formula & Equation` statement |
| postsim_scope | PS2 | ordinary statement references PS name |
| postsim_unbalanced | PS (markers) | lone `PostSim (Begin)` |
| stmt_unknown_keyword | statement form | unknown keyword folded as implicit continuation |
| formula_no_equals | statement form | Formula without `=` |

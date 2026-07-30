build_model_err <- function() {
  list(
    # test-ems_model.R: "postsim sections reject forbidden statements"
    postsim_invalid = c(
      "Statement type{?s} {.val {ps_bad_types}} {?is/are} not allowed in
      a PostSim section.",
      "PostSim sections may contain Set, Subset, Coefficient, File,
      Read, Formula, Assertion, and Zerodivide statements (GEMPACK
      manual 12.2.1)."
    ),
    # pre-flight TAB validators (chk_tab_preflight.R); solver
    # counterparts inventoried in dev/validation_table.md
    # test-chk_tab_preflight.R: "name collisions abort"
    name_coef_var = c(
      "{cli::qty(clash)}Name{?s} declared as both a coefficient and a
      variable: {.val {clash}}.",
      "TABLO names are case-insensitive and must be unique (GEMPACK
      manual 11.2.1)."
    ),
    name_coef_set = c(
      "{cli::qty(clash)}Name{?s} declared as both a coefficient and a
      set: {.val {clash}}.",
      "TABLO names are case-insensitive and must be unique (GEMPACK
      manual 11.2.1)."
    ),
    name_var_set = c(
      "{cli::qty(clash)}Name{?s} declared as both a variable and a
      set: {.val {clash}}.",
      "TABLO names are case-insensitive and must be unique (GEMPACK
      manual 11.2.1)."
    ),
    # test-chk_tab_preflight.R: "duplicate declarations abort"
    name_dup = "{cli::qty(dup_names)}Duplicate {dup_type} declaration{?s}:
    {.val {dup_names}} (GEMPACK manual 11.2.1).",
    # test-chk_tab_preflight.R: "reserved words abort"
    name_reserved = "{cli::qty(res_names)}Declaration name{?s}
    {.val {res_names}} {?is a reserved word/are reserved words} (GEMPACK
    manual 11.2.1).",
    name_c_prefix = "{cli::qty(bad_names)}The {.code c_} prefix is
    reserved for change variables; rename coefficient{?s}
    {.val {bad_names}}.",
    name_prefix_clash = c(
      "{cli::qty(clash)}Variable pair{?s} sharing a base name:
      {.val {clash}}.",
      "A variable X cannot coexist with a variable p_X/c_X: the
      reference token {.code p_X} is ambiguous. Rename one of each
      pair (a coefficient X paired with a variable p_X is fine -- the
      hand-linearized pair idiom)."
    ),
    name_too_long = "{cli::qty(long_names)}Declaration name{?s} longer
    than {max_len} characters: {.val {long_names}}.",
    # test-chk_tab_preflight.R: "unknown qualifiers abort"
    qual_unknown = c(
      "{cli::qty(bad_quals)}Unknown declaration qualifier{?s}:
      {.val {bad_quals}}.",
      "See GEMPACK manual 10.3/10.4 for the recognized variable and
      coefficient qualifiers."
    ),
    qual_no_split = "The variable qualifier {.code no_split} (full shock
    at every step) is not supported: {.val {bad_stmt}}",
    qual_linear_name = "The variable qualifiers {.code linear_name=} and
    {.code linear_var=} are not supported; use the default
    {.code p_}/{.code c_} linear name: {.val {bad_stmt}}",
    qual_empty = "Empty qualifier {.code ()} in declaration:
    {.val {bad_stmt}}",
    qual_unbalanced = "Unbalanced parentheses in the qualifier list of:
    {.val {bad_stmt}}",
    # test-chk_tab_preflight.R: "duplicate bounds abort"
    bound_dup = c(
      "Duplicate {bound_dir} bound in declaration: {.val {bad_stmt}}",
      "One lower ({.code ge}/{.code gt}) and one upper
      ({.code le}/{.code lt}) bound are allowed per declaration (GEMPACK
      manual 10.19.1)."
    ),
    # test-chk_tab_preflight.R: "invalid Default statements abort"
    default_levels = "Equation {.code (default=levels)} is not supported;
    the solver handles linearized equations only (GEMPACK manual 10.19):
    {.val {bad_stmt}}",
    default_homotopy = "Equation {.code (default=add_homotopy)} is not
    supported (GEMPACK manual 10.19): {.val {bad_stmt}}",
    default_bound = "Coefficient bound defaults are not supported
    (GEMPACK manual 10.19): {.val {bad_stmt}}",
    default_unknown = "Unknown {default_kw} default {.val {bad_val}}
    (GEMPACK manual 10.19): {.val {bad_stmt}}",
    default_keyword = "Default statements apply only to Coefficient,
    Variable, Formula, and Equation declarations (GEMPACK manual 10.19):
    {.val {bad_stmt}}",
    default_unsupported = c(
      "Default statements are not supported by the teems pipeline:
      {.val {bad_stmt}}",
      "Declare the qualifier on each affected statement instead; the
      positional Default semantics (GEMPACK manual 10.19) cannot be
      carried through model preparation."
    ),
    # test-ems_model.R: "unbalanced PostSim markers"
    postsim_unbalanced = "Unbalanced PostSim section markers:
    {ps_begin} {.code PostSim (Begin)} against {ps_end}
    {.code PostSim (End)} (GEMPACK manual 12.2).",
    # test-chk_tab_preflight.R: "PostSim scope violations abort"
    postsim_scope = c(
      "{cli::qty(bad_refs)}Ordinary statement{?s} reference{?s/}
      PostSim-declared name{?s}: {.val {bad_refs}}.",
      "PostSim declarations are only visible inside PostSim sections
      (GEMPACK manual 12.2.1)."
    ),
    postsim_same_file = c(
      "{cli::qty(bad_files)}File{?s} {.val {bad_files}} read in both the
      ordinary and PostSim parts.",
      "Split the data across two files (GEMPACK manual 12.2.3)."
    ),
    postsim_read_ord = "{cli::qty(bad_targets)}PostSim Read{?s} into
    ordinary coefficient{?s} {.val {bad_targets}}; targets must be
    PostSim coefficients (GEMPACK manual 12.2.3).",
    postsim_read_var = "{cli::qty(bad_targets)}PostSim Read{?s} into
    variable{?s} {.val {bad_targets}}; simulation results cannot be
    changed (GEMPACK manual 12.2.3).",
    postsim_read_undecl = "{cli::qty(bad_targets)}PostSim Read
    target{?s} {.val {bad_targets}} not declared (GEMPACK manual
    12.2.3).",
    postsim_lhs_var = "{cli::qty(bad_lhs)}PostSim Formula{?s}
    assign{?s/} variable{?s} {.val {bad_lhs}}; simulation results cannot
    be changed (GEMPACK manual 12.2.2).",
    postsim_lhs_ord = "{cli::qty(bad_lhs)}PostSim Formula{?s}
    assign{?s/} ordinary coefficient{?s} {.val {bad_lhs}}; the LHS must
    be a PostSim coefficient (GEMPACK manual 12.2.2).",
    # test-tab_levels.R: "malformed Formula & Equation aborts"
    formula_equation = "Malformed {.code Formula & Equation} statement:
    expected {.code Formula [(initial)] & Equation [(levels)] name
    [quantifiers] lhs = rhs} (GEMPACK manual 10.9.1): {.val {bad_stmt}}",
    # test-tab_levels.R: "p_/c_-leading levels variable name aborts"
    levels_prefix_name = c(
      "{cli::qty(bad_names)}Levels variable{?s} {.val {bad_names}}
      start{?s/} with {.code p_}/{.code c_}, colliding with the
      linear-variable reference prefixes; the solver cannot carry such
      names yet.",
      "Rename the {cli::qty(bad_names)}variable{?s}."
    ),
    # test-chk_tab_preflight.R: "math statements without = abort"
    stmt_missing_equals = c(
      "{stmt_kw} statement without {.code =}: {.val {bad_stmt}}",
      "Either the statement is malformed or its leading token is an
      unrecognized keyword that was read as an implicit {stmt_kw}
      continuation."
    ),
    read_terminal = "Read from terminal is not supported; read from a
    file instead: {.val {bad_stmt}}",
    read_no_header = "{cli::qty(bad_reads)}Read{?s} without a header
    {?is/are} not supported (GEMPACK manual 11.11.8): {.val {bad_reads}}",
    read_undeclared = "{cli::qty(bad_targets)}Read target{?s}
    {.val {bad_targets}} not declared as {?a coefficient/coefficients}.",
    # Mapping statements (GEMPACK manual 11.9); solver counterparts in
    # tab_parse.c mapping machinery (teems-solver M1-M3)
    # test-chk_tab_preflight.R: "malformed mapping declarations abort"
    map_malformed = c(
      "Malformed {.field Mapping} statement: {.val {bad_stmt}}",
      "Expected {.code Mapping [(onto)] <name> from <set> to <set>;}
      (GEMPACK manual 11.9.1)."
    ),
    # test-chk_tab_preflight.R: "mapping with undeclared sets aborts"
    map_undeclared_set = "{cli::qty(bad_sets)}Set{?s} {.val {bad_sets}}
    in the {.field Mapping} declaration of {.val {map_name}}
    {?is/are} not declared in the model.",
    # test-chk_tab_preflight.R: "mapping name clashes abort"
    name_map_clash = c(
      "{cli::qty(clash)}Name{?s} declared as both a mapping and a
      {clash_kind}: {.val {clash}}.",
      "TABLO names are case-insensitive and must be unique (GEMPACK
      manual 11.2.1)."
    ),
    # test-chk_tab_preflight.R: "by_elements read of a non-mapping aborts"
    byele_nonmap = "{cli::qty(bad_targets)}{.code Read (by_elements)}
    target{?s} {.val {bad_targets}} {?is/are} not {?a declared
    mapping/declared mappings} (GEMPACK manual 11.9.3).",
    # test-chk_tab_preflight.R: "plain read of a mapping aborts"
    map_read_plain = "{cli::qty(bad_targets)}Mapping{?s}
    {.val {bad_targets}} must be read with the
    {.code (by_elements)} qualifier (GEMPACK manual 11.9.3).",
    # test-chk_tab_preflight.R: "mapping without a read aborts"
    map_read_missing = "{cli::qty(bad_maps)}Mapping{?s}
    {.val {bad_maps}} {?has/have} no {.code Read (by_elements)}
    statement assigning {?its/their} values.",
    # test-ems_model.R: "ems_model rejects invalid variable names in omit"
    invalid_omit = "{.val {invalid_var}} designated for omission not found in the model.",
    # test-ems_model.R: "ems_model rejects invalid variable names in backsolve"
    invalid_backsolve_var = "{.val {invalid_var}} designated for backsolving not found in the model.",
    # test-ems_model.R: "ems_model rejects invalid equation names in backsolve"
    invalid_backsolve_eq = "Equation {.val {invalid_eq}} nominated for backsolving {.val {bs_var}} not found in the model.",
    # test-ems_model.R: "ems_model rejects unresolvable backsolve entries"
    backsolve_unresolvable = c(
      "No equation {.val {conv_eq}} found to backsolve {.val {bs_var}}.",
      "Unnamed {.arg backsolve} entries resolve their defining equation by the {.field E_<variable>} convention.",
      "Name the defining equation explicitly: {.code backsolve = c({bs_var} = \"<equation>\")}."
    ),
    # test-ems_model.R: "ems_model rejects conflicting condensation actions"
    condense_conflict = "Variable{?s} {.val {conflict_var}} appear{?s/} in more than one condensation action (omit/backsolve).",
    # test-ems_model.R: "ems_model rejects a reused backsolve equation"
    condense_eq_reused = "Equation{?s} {.val {reused_eq}} nominated for more than one backsolve.",
    # test-ems_model.R: "backsolve rule violations" (GEMPACK manual 14.1.10)
    condense_rule = c(
      "Equation {.field {eq_name}} cannot be used to backsolve {.field {var_name}}.",
      "{rule_text}",
      "GEMPACK substitution requirement: see the GEMPACK manual, section 14.1.10."
    ),
    # test-ems_model.R: "unrearrangeable backsolve equation"
    condense_rearrange = c(
      "Equation {.field {eq_name}} could not be rearranged into the form {.field {var_name} = ...}.",
      "Occurrences of {.field {var_name}} must enter the equation as top-level additive terms, optionally multiplied by coefficient expressions or enclosed in sums over indices the variable does not carry."
    ),
    # test-ems_model.R: "unparseable equation nominated for backsolve"
    condense_parse = c(
      "Failed to parse {.field {eq_name}} into linear terms while condensing: {parse_reason}.",
      "Statement: {.field {statement}}."
    ),
    # test-ems_model.R: "backsolved variables must be endogenous in the closure"
    condense_endo = c(
      "Backsolved {.val {bs_exo}} {cli::qty(length(bs_exo))}{?is/are} exogenous in the closure.",
      "Substituted-out variables must be endogenous; swap out of the closure or drop the backsolve."
    ),
    # test-ems_model.R: "omitted variables must be exogenous in the closure"
    condense_exo = c(
      "Omitted {.val {omit_endo}} {cli::qty(length(omit_endo))}{?is/are} not exogenous in the closure.",
      "Omitted variables must be exogenous and unshocked (GEMPACK manual, section 14.1)."
    ),
    # test-ems_model.R: "ems_model rejects invalid coefficient arguments"
    invalid_coeff = "{.arg {nme}} is not declared in the model.",
    # test-ems_model.R: "partial read statement"
    invalid_read = "Partial {.field Read} statements are not supported.",
    # leaving this in but there is not possible?
    invalid_mod = "{.arg nme} is neither read in nor appearing on the LHS of a formula.",
    # test-ems_model.R: "invalid numeric to a formula"
    invalid_numeric = c("Directly assigned numeric values must be length 1.",
                        "To assign heterogeneous values, use a {.code data.frame} with the appropriate set columns."),
    # test-ems_model.R: "invalid tab statement"
    invalid_state = c("teems {version} does not support {.field {inv_state}} statements.",
                      "Supported statements include: {.field {supported_state}}."),
    # probably a redundant check but if a weird unrecognized statement is found then there needs to be a way of distinguishing between an implied statement and an unrecognized statement
    unsupported_tab = "Unsupported Tablo declarations detected: {.field {unsupported}}.",
    # test-ems_model.R: "invalid intertemporal header"
    invalid_int_header = c(
      "Intertemporal {header_descr} {.val {timestep_header}} not found in loaded data.",
      "Use {.fun teems::ems_option_set} {.arg {arg_name}} to set a custom {header_descr}."
    ),
    # test-ems_model.R: "invalid read statement"
    missing_file = "Read statements missing \"from file\" detected.",
    # test-ems_model.R: "invalid binary set switch statement"
    binary_switch = c("Unsupported binary switch detected in a {.field Set} definition.",
                      "Declare sets explicitly within the Tablo file or using {.arg ...} within {.fun teems::ems_model}.",
                      "For example, {.field Set ENDWM # mobile endowment # (capital,unsklab,sklab);} {.emph not} {.field Set ENDWM # mobile endowments # = (all,e,ENDW:ENDOWFLAG(e,\"mobile\") ne 0);}."),
    # test-ems_model.R: "intertemporal set equality"
    int_set_eq_fail = c(
      "Set equality involving an intertemporal set detected: {.field {eq_statement}}.",
      "Converting between intertemporal and non-intertemporal sets via set equality is not supported."
    ),
    # test-ems_model.R: "unparseable set definition"
    invalid_set_def = "Unparseable {.field Set} definition detected: {.field {bad_def}}.",
    # test-chk_tab_preflight.R: "self-referential set expressions abort"
    set_self_ref = c(
      "Set {.field {bad_set}} references itself in its defining
      expression: {.val {bad_def}}.",
      "Define a set from other sets and quoted elements only (GEMPACK
      manual 10.1.1.1)."
    ),
    # test-chk_tab_preflight.R: "undeclared set references abort"
    set_undeclared = c(
      "{cli::qty(bad_refs)}Set{?s} referenced before declaration in
      {.val {bad_stmt}}: {.val {bad_refs}}.",
      "Sets must be declared before they are used in a definition or
      {.field Subset} statement (GEMPACK manual 10.1)."
    ),
    # test-chk_tab_preflight.R: "set self-equality aborts"
    set_self_eq = "Set {.field {bad_set}} is defined as equal to
    itself (GEMPACK manual 10.1.2.1).",
    # test-chk_tab_preflight.R: "element range abbreviations abort"
    set_ele_range = c(
      "Element range abbreviation in set {.field {bad_set}}:
      {.val {bad_ele}}.",
      "The {.code (first - last)} form is not supported; list the
      elements explicitly."
    ),
    # test-chk_tab_preflight.R: "malformed element lists abort"
    set_ele_list = "Malformed element list for set
    {.field {bad_set}}: {.val {bad_def}} contains
    {empty_or_malformed} elements.",
    # test-chk_tab_preflight.R: "over-length set headers abort"
    set_header_len = "Header longer than 4 characters in the
    declaration of set {.field {bad_set}}: {.val {bad_header}}.",
    # test-int_sets.R
    set_int_range = c(
      "Intertemporal set {.field {bad_set}} has {range_defect} time
      range: {.val {bad_def}} resolves to {resolved_txt}.",
      "With {n_timestep} time step{?s} the valid indices are
      {.code p[0]} through {.code p[{n_timestep - 1}]}."
    ),
    # test-int_sets.R
    set_int_malformed = "Malformed intertemporal set definition for
    {.field {bad_set}}: {.val {bad_def}}.",
    # test-chk_subset_containment.R
    subset_not_contained = c(
      "Subset {.field {bad_sub}} is not contained in
      {.field {bad_super}}: {cli::qty(missing_ele)}element{?s}
      {.val {missing_ele}} {cli::qty(missing_ele)}{?is/are} missing
      from the superset.",
      "Check the {.field Subset} statement and the aggregation
      mappings that build both sets."
    ),
    # test-ems_model.R: "unsupported IF placement"
    invalid_if_placement = c(
      "Unsupported {.field IF} placement detected: {.field {if_statement}}.",
      "{.field IF} terms must enter {.field Formula} and {.field Equation} statements additively at the top level of an expression."
    ),
    # test-ems_model.R: "unsupported IF condition"
    invalid_if_cond = c(
      "Unsupported {.field IF} condition detected: {.field {if_cond}}.",
      "Supported forms: {.field <index> in <set>}, {.field <index> = \"<element>\"}, and {.field <coefficient> <op> <constant>}."
    ),
    # test-ems_model.R: "multiple membership IF conditions in an equation"
    invalid_if_multi = c(
      "Multiple set-membership or element {.field IF} conditions detected in one {.field Equation}: {.field {if_statement}}.",
      "An {.field Equation} supports one such condition (it splits the equation domain); comparison conditions are unrestricted."
    ),
    # test-ems_model.R: "invalid set qualifier"
    invalid_set_qual = "Invalid set qualifier detected: {.field {invalid_qual}}.",
    # not in tests
    set_parse_fail = "Remnant set label detected during Tablo parsing.",
    # test-ems_model.R: "data frame input missing a set"
    injection_missing_col = c(
      "Input for {.field {nme}} is missing required columns.",
      "Required: {.field {req_col}}."
    ),
    # test-ems_model.R: "invalid var in closure"
    no_var = "Closure contains variables not in the model: {.val {var_discrepancy}}.",
    # the following error should never be issued (full will be assigned)
    entry_type = "The following closure entries have not been classified properly: {invalid_entry}.",
    # test-ems_model.R: "closure missing exo/endo spec"
    missing_specification = "The closure must contain both {.val Exogenous} and {.val Rest Endogenous} entries. The inverse approach is not supported.",
    # test-ems_model.R: "ems_model errors when invalid closure mixed entry present preswap"
    mixed_invalid = "{n_invalid_entries} closure entry element{?s} in {.field {cls_entry}} do not belong to the respective variable sets: {invalid_entries}.",
    # test-ems_model.R: "ems_model errors when duplicate closure entry present preswap"
    pre_overlap_ele = "{n_overlap} tuple{?s} for {.val {e}} in the pre-swap closure with multiple entries: {overlap}.",
    # test-ems_model.R: "ems_model errors when invalid closure pure element entry present preswap"
    ele_invalid = "The closure entry tuple {.field {cls_entry}} is invalid under the current set mapping.",
    # test-ems_model.R: "ems_model errors when invalid closure subset entry present preswap"
    subset_invalid = c("Some subsets in {.field {cls_entry}} do not belong to {.field {var_name}}.",
                       "Parent sets include: {.field {var_sets}}."),
    no_name_coeff = "Coefficients to modify must be passed as named pairs: {.code RDLT = 1}."
    )
}

build_model_info <- function() {
  list(
    # test-ems_model.R: "netcut proxy rewrite (roadmap 6.5 E2)"
    netcut_rewrite = c(
      "Inter-period links on element slices rewritten onto minimal intertemporal proxies: {.field {proxy_summary}}.",
      "Proxy variables (NCV*) and their linking equations (E_NCV*) appear in solve outputs."
    ),
    # test-ems_model.R: "in-TAB Substitute executes as backsolve"
    substitute_as_backsolve = c(
      "In-TAB {.field Substitute} statement{?s} for {.val {sub_var}} executed as backsolve{?s}.",
      "Backsolved values remain available in solve outputs; plain substitution is not implemented."
    ),
    # test-ems_model.R: "ignore_condense disables in-TAB condensation"
    condense_ignored = "{n_ignored} in-TAB condensation statement{?s} ignored ({.code ignore_condense = TRUE})."
  )
}

build_model_wrn <- function() {
  list(
    # test-ems_model.R: "ignored tab statement"
    ignored_state = "The following model statements are unsupported and will be ignored: {.field {ign_state}}.",
    # test-ems_model.R: "backsolve pivot divide warning"
    condense_pivot_zero = c(
      "Backsolving {.field {var_name}} using {.field {eq_name}} divides by the coefficient expression {.field {pivot_expr}}.",
      "Ensure this expression can never be zero; a zero value will surface as a solver error."
    ),
    # test-ems_model.R: "netcut inflation warning"
    netcut_inflation = c(
      "Multidimensional {.field {offenders}} referenced with a lead or lag in {.field {lag_eqs}}.",
      "Every element of a lead/lagged variable joins the dense border (netcut) of the bordered matrix methods (SBBD/DBBD/NDBBD); each non-time dimension multiplies the border size.",
      "Link periods through a minimal intertemporal proxy instead, e.g. {.code capital(REG,TIME) = qo(\"capital\",REG,TIME)}, and place the lead/lag on the proxy."
    )
  )
}

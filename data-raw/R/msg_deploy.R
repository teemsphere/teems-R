build_deploy_err <- function() {
  list(
    # test-ems_deploy.R: "ems_deploy errors when read-in headers not present in data"
    missing_header = "Read-in headers missing from loaded data: {.val {missing_headers}}.",
    # not in tests
    while_loop = "Construction of dependent sets has failed on: {null_sets}.",
    # conditional set builders evaluated at deploy (.eval_set_builder,
    # mirror of the solver's tab_setbuilder_transform fatals);
    # test-ems_deploy.R: "conditional set builders"
    set_builder_data = "{.field Set} builder {.val {bad_set}}: no loaded data for its condition coefficient {.val {cond_coef}}.",
    set_builder_args = "{.field Set} builder {.val {bad_set}}: condition coefficient {.val {cond_coef}} has {n_dims} dimension{?s} but {n_args} argument{?s} were given.",
    set_builder_ele = "{.field Set} builder {.val {bad_set}}: element {.val {bad_ele}} is not in the {.field {dim_set}} dimension of {.val {cond_coef}} under the current aggregation.",
    set_builder_dim = "{.field Set} builder {.val {bad_set}}: the loop index {.val {loop_idx}} must range over {.val {cond_coef}}'s dimension set {.field {dim_set}} exactly (source set {.field {src_set}} differs).",
    set_builder_empty = c(
      "{.field Set} builder {.val {bad_set}} selected no elements of {.field {src_set}} with {.code {builder_cond}}.",
      "An empty set cannot enter the model (GEMPACK manual 10.1.2); check the condition against the aggregated data."
    ),
    set_builder_mapsum = c(
      "{.field Set} builder {.val {bad_set}} uses the mapping-conditional sum form, which teems cannot evaluate at deploy yet.",
      "The solver evaluates it; teems needs the elements ahead of the run for closure/shock validation. Declare the set explicitly for now."
    ),
    # test-ems_deploy.R: "ems_deploy errors when read-in headers are missing mapping"
    missing_mapping = "Some read-in model sets have no mappings: {.field {m_map}}.",
    # test-ems_deploy.R: "ems_deploy errors when timesteps provided to static model"
    nonreq_tsteps = "{.arg time_steps} provided but no intertemporal sets detected in the model. See {.fun teems::ems_data}.",
    # test-ems_deploy.R: "ems_deploy errors when timesteps not provided to a dynamic model"
    missing_tsteps = "{.arg time_steps} required for intertemporal models. See {.fun teems::ems_data}.",
    # test-ems_deploy.R: "ems_deploy errors when set-calculated number of entries does not match a finalized data header"
    data_set_mismatch = "{.field {class(dt)[1]}} has {.val {nrow(dt)}} entries; {.val {expected}} expected.",
    # not in tests
    invalid_plus = "Set operator {.code +} requires disjoint sets; overlapping elements: {.field {d}}.",
    # test-ems_model.R: "set expression operator validity"
    invalid_minus = "Set operator {.code -} may only remove elements that are present; missing: {.field {d}}.",
    # test-tab_mapping.R: "a mapping over a conflicted intersection set aborts"
    # INTERSECT itself is permissive (element-level, manual 10.1.1);
    # the origin_conflict stamp set by .eval_set_expr aborts here, at
    # the one consumer that reads origin rows
    map_origin_conflict = c(
      "The {loc} set {.field {set_name}} of mapping {.val {map_name}}
      is built by an INTERSECT whose operands disagree about the
      source composition of {cli::qty(conflict)}element{?s}
      {.val {conflict}}.",
      "The by_elements composition depends on which source elements
      aggregate into {cli::qty(conflict)}{?this/these} element{?s};
      align the aggregation mappings (or the set definitions) so both
      operands agree."
    ),
    # test-ems_deploy.R: "ems_deploy errors when aggregated inputs are incomplete"
    agg_missing_tup = "{n} tuple{?s} in the provided input file for {.val {nme}} were missing: {.field {missing}}.",
    # Mapping data build (GEMPACK manual 11.9.3); mirrors the solver
    # by_elements read fatals ahead of the deploy round-trip
    # test-ems_deploy.R: "mapping header missing from the data aborts"
    map_data_missing = c(
      "No header {.val {header}} found in the input data for mapping
      {.val {map_name}}.",
      "{.code Read (by_elements)} data must be supplied as a character
      header in the {.fun teems::ems_data} inputs."
    ),
    # test-ems_deploy.R: "mapping header count mismatch aborts"
    map_data_count = "Mapping {.val {map_name}} header {.val {header}}
    holds {.val {n_vals}} value{?s}; the domain set {.field {dom}} has
    {.val {n_dom}} element{?s} in the input data.",
    # test-ems_deploy.R: "mapping values outside the codomain abort"
    map_data_ele = "{cli::qty(bad_vals)}Mapping {.val {map_name}}
    value{?s} {.val {bad_vals}} {?is/are} not {?an element/elements}
    of the codomain set {.field {cod}}.",
    # test-ems_deploy.R: "split mapping under aggregation aborts"
    map_agg_split = c(
      "Aggregated {.field {dom}} element {.val {agg_ele}} merges
      source elements with different {.field {cod}} values:
      {.field {split_detail}}.",
      "Mapping {.val {map_name}} cannot be composed under this
      aggregation; revise the {.field {dom}} aggregation or the
      {.val {header}} data."
    ),
    # test-ems_deploy.R: "onto violation after aggregation aborts"
    map_onto = c(
      "{cli::qty(missing_cod)}Codomain element{?s} {.val {missing_cod}}
      of the {.code (onto)} mapping {.val {map_name}} {?is/are} not
      covered after aggregation.",
      "Every {.field {cod}} element must be the value of at least one
      {.field {dom}} element (GEMPACK manual 11.9.1)."
    ),
    # test-ems_deploy.R: "ems_deploy errors when shock_file and shock are both provided"
    shk_file_shocks = c(
      "No additional shocks are accepted if a shock file is provided."
    ),
    # test-ems_deploy.R: "write_coefficients must be a logical scalar"
    write_coefficients = "{.arg write_coefficients} must be logical of length 1."
  )
}

build_deploy_info <- function() {
  list(
    # test-ems_deploy.R: "auto_omit drops unshocked exogenous variables"
    auto_omit = c(
      "{.arg auto_omit}: {n_auto_omit} unshocked exogenous variable{?s}
      omitted ({.val {omitted_shown}}).",
      "Omitted variables are absent from solve outputs."
    ),
    auto_omit_none = "{.arg auto_omit}: no variable qualifies for omission.",
    # test-ems_deploy.R: "auto_omit is skipped for a supplied shock file"
    auto_omit_shock_file = c(
      "{.arg auto_omit} is skipped when {.arg shock_file} is supplied:
      the file's shocked variables are not parsed.",
      "Nominate omissions with {.arg omit} in {.fun ems_model} instead."
    )
  )
}

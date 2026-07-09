build_deploy_err <- function() {
  list(
    # test-ems_deploy.R: "ems_deploy errors when read-in headers not present in data"
    missing_header = "Read-in headers missing from loaded data: {.val {missing_headers}}.",
    # not in tests
    while_loop = "Construction of dependent sets has failed on: {null_sets}.",
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
    # test-ems_deploy.R: "ems_deploy errors when aggregated inputs are incomplete"
    agg_missing_tup = "{n} tuple{?s} in the provided input file for {.val {nme}} were missing: {.field {missing}}.",
    # test-ems_deploy.R: "ems_deploy errors when shock_file and shock are both provided"
    shk_file_shocks = c(
      "No additional shocks are accepted if a shock file is provided."
    )
  )
}

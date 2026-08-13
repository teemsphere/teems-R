# ems_deploy errors when .data is missing

    x argument `.data` is missing, with no default

# ems_deploy errors when model is missing

    x argument `model` is missing, with no default

# ems_deploy errors when invalid variable provided for swap-in

    x Swap variable "not_a_var" not found in the model.

# ems_deploy errors when invalid variable provided for swap-out

    x Swap variable "not_a_var" not found in the model.

# ems_deploy errors when shock_file and shock are both provided

    x No additional shocks are accepted if a shock file is provided.

# ems_deploy errors when read-in headers not present in data

    x Read-in headers missing from loaded data: "SAVE".

# ems_deploy errors when read-in headers are missing mapping

    x Some read-in model sets have no mappings: REG.

# ems_deploy errors when timesteps provided to static model

    x `time_steps` provided but no intertemporal sets detected in the model. See `teems::ems_data()`.

# ems_deploy errors when timesteps not provided to a dynamic model

    x `time_steps` required for intertemporal models. See `teems::ems_data()`.

# ems_deploy errors when set-calculated number of entries does not match a finalized data header

    x ESBT has 45 entries; 30 expected.

# ems_deploy errors when aggregated inputs are incomplete

    x 7 tuples in the provided input file for "SAVE" were missing: 1: chn 1, 2: chn 2, 3: row 1, 4: row 2, 5: usa 0, 6: usa 1, and 7: usa 2.

# auto_omit drops unshocked exogenous variables (roadmap 6.2)

    Code
      cmf_path <- ems_deploy(dat, auto_model)
    Message
      i `auto_omit`: 51 unshocked exogenous variables omitted ("pop", "tinc", "endwslack", ..., "psaveslack", and "pfactwld").
      i Omitted variables are absent from solve outputs.

# auto_omit is skipped when a shock file is supplied

    Code
      cmf_path <- ems_deploy(dat, auto_model, shock_file = shf)
    Message
      i `auto_omit` is skipped when `shock_file` is supplied: the file's shocked variables are not parsed.
      i Nominate omissions with `omit` in `ems_model()` instead.


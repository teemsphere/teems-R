library(targets)

targets::tar_config_set(store = "./data-raw/_targets")

# Set target options:
targets::tar_option_set(
  packages = c("data.table", "usethis", "purrr", "tabulapdf"),
  format = "qs",
  cue = tar_cue("always")
)



# functions
targets::tar_source("./data-raw/R")

list(
  tar_target(db_version, c("v9", "v10", "v11")),
  tar_target(data_format, c("v6.2", "v7.0")),
  # mapping related --------------------------------------------------
  tar_target(mapping_files, {
    list.files(
      "../teems-mappings",
      pattern = "\\.csv",
      recursive = TRUE,
      full.names = TRUE
    )
  }),
  tar_target(
    mappings,
    process_mappings(
      mapping_files = mapping_files,
      db_version = db_version,
      data_format = data_format
    )
  ),
  # tab related ------------------------------------------------------
  tar_target(tab_files, {
    list.files(
      path = "../teems-models",
      pattern = "\\.tab",
      full.names = T,
      recursive = T
    )
  }),
  tar_target(tab_names, {
    lapply(tab_files, function(tab) {
      tools::file_path_sans_ext(x = basename(path = tab))
    })
  }),
  tar_target(internal_tab, {
    tabs <- purrr::map2(tab_files, tab_names, function(tab, nme) {
      tab <- readChar(con = tab, nchars = file.info(tab)[["size"]])
      class(tab) <- nme
      return(tab)
    })

    names(x = tabs) <- tab_names
    return(tabs)
  }),
  tar_target(tab_qual, {
    c(
      "nonzero_by_zero",
      "zero_by_zero",
      "\\bchange\\b",
      "linear",
      "orig_level",
      "ge [[:digit:]]",
      "gt [[:digit:]]",
      "integer",
      "parameter",
      "initial",
      "\\breal\\b",
      "levels"
    )
  }),

  # parameter related ------------------------------------------------
  # static ===========================================================
  tar_target(v6.2_weights, {
    list(
      ESBD = c("VDPA", "VIPA", "VDGA", "VIGA", "VDFA", "VIFA"),
      ESBM = c("VIPA", "VIGA", "VIFA"),
      ESBT = c("VDFM", "VIFM", "VFM", "FTRV", "-FBEP", "-ISEP"),
      ESBV = "EVFA",
      INCP = c("VDPA", "VIPA"),
      SUBP = c("VDPA", "VIPA")
    )
  }),
  tar_target(v7.0_weights, {
    list(
      ESBD = c("VDPP", "VMPP", "VMGP", "VDGP", "VDFP", "VMFP"),
      ESBM = c("VMPP", "VMGP", "VMFP"),
      ESBT = c("VDFB", "VMFB", "EVFB", "FTRV", "-FBEP", "-ISEP"),
      ESBV = "EVFP",
      INCP = c("VDPP", "VMPP"),
      SUBP = c("VDPP", "VMPP"),
      ESBC = c("VDFB", "VMFB")
    )
  }),
  tar_target(param_weights, {
    list(v6.2 = v6.2_weights, v7.0 = v7.0_weights)
  }),

  # closures ---------------------------------------------------------
  tar_target(closure_repo, {
    list.files(
      path = file.path("../teems-closures/"),
      full.names = TRUE
    )
  }),
  tar_target(closure_files, {
    list.files(
      "../teems-models",
      pattern = "\\.cls",
      recursive = TRUE,
      full.names = TRUE
    )
  }),
  tar_target(internal_cls, {
    cls <- lapply(
      X = closure_files,
      FUN = function(closure) {
        readLines(closure)
      }
    )

    cls_names <- lapply(closure_files, function(cls) {
      tools::file_path_sans_ext(x = basename(path = cls))
    })

    names(x = cls) <- cls_names
    return(cls)
  }),
  # v6.2 <> v7 conversion
  tar_target(
    GTAPv7_manual,
    "./data-raw/Corong and Tsigas - 2017 - The Standard GTAP Model, Version 7.pdf",
    format = "file"
  ),
  # tables here are not even concordance, just semi related lists
  tar_target(set_conversion, {
    set_table <- tabulapdf::extract_tables(file = GTAPv7_manual, pages = 83)
    c_names <- c("name", "header", "description")

    v6.2_sets <- set_table[[1]][, c(1:4)]
    colnames(x = v6.2_sets) <- c("idx", paste0("v6.2", c_names))
    v7.0_sets <- set_table[[1]][, c(5:8)]
    colnames(x = v7.0_sets) <- c("idx", paste0("v7.0", c_names))

    # always inconsistencies in GTAP outputs
    v7.0_sets[5:13, "idx"] <- 5:13
    v7.0_sets[12:13, "idx"] <- 13:14

    sets <- merge(v6.2_sets, v7.0_sets, by = "idx", all = TRUE)
  }),
  tar_target(param_conversion, {
    param_table <- tabulapdf::extract_tables(file = GTAPv7_manual, pages = 84)

    v7.0_param <- param_table[[1]][, c(5:8)]

    # missing ESBQ
    ESBQ <- tibble::tibble(
      14,
      "ESBQ",
      "COMM*REG",
      "1/CES elasticity for commodity sourcing"
    )
    colnames(x = ESBQ) <- colnames(x = v7.0_param)

    # missing ESBI
    # ESBI <- tibble::tibble(15, "ESBI", "REG", "Investment expenditure CES elasticity")
    # colnames(x = ESBI) <- colnames(x = v7.0_param)

    v7.0_param <- rbind(v7.0_param, ESBQ)

    v7.0_param <- .table_fix(
      single = c(11, 12, 27),
      double = c(1, 3, 5, 7, 9, 13, 15, 17, 19, 25),
      trebble = c(19, 22),
      table = v7.0_param,
      prefix = "v7.0",
      data_type = "par"
    )

    v7.0_param[10:14, "idx"] <- 11:15

    v6.2_param <- param_table[[1]][, c(1:4)]

    v6.2_param <- .table_fix(
      single = c(11, 12),
      double = c(1, 3, 5, 7, 9, 13, 15, 17),
      table = v6.2_param,
      prefix = "v6.2",
      data_type = "par"
    )

    param <- merge(v6.2_param, v7.0_param, by = "idx", all = TRUE)
    param[["data_type"]] <- "par"
    return(param)
  }),
  tar_target(dat_conversion, {
    coeff_table <- tabulapdf::extract_tables(file = GTAPv7_manual, pages = 85:86)

    coeff_table <- data.table::rbindlist(l = coeff_table)

    v7.0_coeff <- coeff_table[, c(4:6)]
    double <- c(3, 18, 20, 22, 24, 26, 30, 32, 35, 37, 41, 49, 51, 54)
    single <- setdiff(seq(1, nrow(v7.0_coeff)), double)
    NAs <- which(is.na(v7.0_coeff[, 1]))
    single <- setdiff(single, NAs)
    v7.0_coeff <- .table_fix(
      single = single,
      double = double,
      table = v7.0_coeff,
      prefix = "v7.0",
      data_type = "dat"
    )

    v6.2_coeff <- coeff_table[, c(1:3)]
    double <- c(18, 20, 22, 24, 26, 30, 32, 35, 37, 41)
    single <- setdiff(seq(1, nrow(v6.2_coeff)), double)

    NAs <- which(is.na(v6.2_coeff[, 1]))
    single <- setdiff(single, NAs)
    v6.2_coeff <- .table_fix(
      single = single,
      double = double,
      table = v6.2_coeff,
      prefix = "v6.2",
      data_type = "dat"
    )

    coeff <- merge(v6.2_coeff, v7.0_coeff, by = "idx", all = TRUE)
    coeff[["v6.2set"]] <- NA
    coeff[["v7.0set"]] <- NA
    coeff[["data_type"]] <- "dat"
    return(coeff)
  }),
  tar_target(coeff_conversion, {
    rbind(dat_conversion, param_conversion)
  }),
  # messages
  tar_target(gen_err, {
    list(
      class = "{.arg {arg_name}} must be a {.or {check}}, not {.obj_type_friendly {arg}}.",
      dir_not_file = "A filepath is expected, not the directory {.file {file}}.",
      no_file = "Cannot open file {.file {file}}: No such file.",
      invalid_file = "{.arg {arg}} must be a {.or {.val {valid_ext}}} file, not {?a/an} {.val {file_ext}} file.",
      invalid_internal = c(
        "The specified internal {file_type} file: {.val {file}} is not supported.",
        "Currently supported {file_type} files include: {.val {valid_internal_files}}.",
        "Alternatively, path to a user-provided {file_type} file is supported (e.g., \"/my/{file_type}/path.{ext}\")",
        "Note that user-provided {file_type} files may need to be modified for compatibility with various {.pkg teems} functions."
      )
    )
  }),
  tar_target(gen_wrn, {
    list(
      db_version = c(
        "{.pkg teems} version: {teems_version} has only been vetted on GTAP Data Base versions: {vetted}.",
        "The {.fn teems::emssolve} function can bypass the pipeline and be called on solver-ready input files."
      )
    )
  }),
  tar_target(gen_info, {
    list(
      dat = c(
        "GTAP Data Base version: {.field {full_database_version}}",
        "Reference year: {.field {reference_year}}",
        "Data format: {.field {data_format}}"
      )
    )
  }),
  tar_target(gen_url, {
    list(internal_files = NULL)
  }),
  tar_target(data_err, {
    list(
      missing_tar = "If {.arg target_format} is provided for conversion, a Tablo file of the desired target format must be provided.",
      invalid_convert = "The HAR file provided for {.arg dat_input} is already of the {.arg target_format} {.val {target_format}}.",
      # missing_tab = "If {.arg time_steps} is provided for a dynamic model, a Tablo file must be provided.",
      wrong_input = "It appears that a non-dat file has been provided as a  {.arg dat_input}.",
      invalid_dat_har = "The header array file provided for {.arg dat_input} appears to be of type {.val {inferred_type}}, not {.val dat}.",
      invalid_par_har = "The header array file provided for {.arg par_input} appears to be of type {.val {inferred_type}}, not {.val par}.",
      invalid_set_har = "The header array file provided for {.arg set_input} appears to be of type {.val {inferred_type}}, not {.val set}.",
      invalid_time_step = "One or more {.arg time_steps} does not progress into the future.",
      data_set_mismatch = "The expected number of data entries on {.field {class(dt)[1]}} ({.val {expected}}) is not equal to the number found ({.val {nrow(dt)}}).",
      missing_tsteps = "{.arg time_steps} have not been provided to an intertemporal model. See {.fun teems::ems_data}.",
      nonreq_tsteps = "{.arg time_steps} have been provided yet no intertemporal sets have been detected in the provided model. See {.fun teems::ems_data}.",
      missing_mapping = "Some model sets that are read as headers are missing mappings: {.field {m_map}}.",
      missing_header = "The following headers are designated as read-in but are missing from the loaded data: {.val {missing_headers}}."
    )
  }),
  tar_target(data_wrn, {
    list(time_steps = "The initial timestep provided is neither {.val {as.numeric(0)}} nor the reference year corresponding to the {.field dat} file loaded: {.val {t0}}.")
  }),
  tar_target(load_err, {
    list(
      # nested_class = "Input data must be provided as a {.or {data_class}}, not {.obj_type_friendly {errant_class}}.",
      invalid_input = "The input header provided {.field {nme}} is not among loaded data headers: {.field {existing_headers}}.",
      no_val_col = "Input data for the header {.field {nme}} does contain {.val Value} as the final column.",
      unagg_missing_tup = "{n_missing_tuples} tuple{?s} in the provided input file for {.val {nme}} were missing: {.field {missing_tuples}}.",
      unagg_missing_col = c(
        "Input data for the header {.field {nme}} does not contain all required columns (sets).",
        "The required columns are {.field {req_col}}."
      ),
      missing_eqm_input = "If {.arg eqm_input} is not provided, both {.arg dat_input} and {.arg par_input} are required inputs.",
      missing_set_mappings = "Set mappings passed to {.arg ...} as a pairwise list are required.",
      invalid_internal_mapping = c(
        "The internal mapping selected: {.val {set_map}}, for set {.val {map_name}} does not exist.",
        "Available internal mappings for {.val {map_name}} include {.val {available_map_names}}"
      ),
      missing_ele_ext_mapping = "The set mapping for {.val {map_name}} is missing mappings for {.val {missing_ele}}.",
      no_ele_ext_mapping = "No required elements were found for the {.field {data_format}} set {.val {map_name}} indicating an invalid set name.",
      no_internal_mapping = "No internal mappings were found for the set {.field {map_name}}.",
      extra_input = "If {.arg eqm_input} is provided, {.arg dat_input}, {.arg par_input}, and {.arg set_input} arguments are unnecessary.",
      invalid_set_qual = "Invalid set qualifier detected: {.val {invalid_qual}}.",
      set_parse_fail = "Remnant set label detected during Tablo parsing.",
      set_op_fail = c(
        "Multiple {.val +} and/or {.val -} were detected within a single Tablo Set statement.",
        "For compatibility, split into multiple statements: Instead of A123=A1+A2+A3, A12=A1+A2 then A123=A12+A3"
      ),
      identical_set_fail = c(
        "It appears that one set has been defined as identical to a second set (e.g., Set SET_B # example set B # = SET_A;).",
        "If duplicate sets are desired, multiple Read statements should be implemented (e.g., Set SET_A # example set A # maximum size 5 read elements from file GTAPSETS header \"H2\";Set SET_B # example set B # maximum size 5 read elements from file GTAPSETS header \"H2\";)"
      )
    )
  }),
  tar_target(model_wrn, {
    list(ETRE = "The {.arg ETREtoENDW} value for {.fun teems::ems_option_get()} is set to `TRUE` however no dedicated sluggish endowment set has been detected for {.field ETRE}.")
  }),
  tar_target(model_err, {
    list(
      unsupported_tab = "Unsupported Tablo declarations detected: {.val {unsupported}}.",
      invalid_int_header = c(
        "The {header_descr} required is currently set as {.val {timestep_header}} but this header is not loaded within the Tablo file.",
        "See {.fun teems::ems_option_set} {.arg {arg_name}} for setting the {header_descr}."
      ),
      missing_file = "Read statements missing \"from file\" detected",
      binary_switch = "A colon was detected within a {.field Set} definition indicating the use of an unsupported binary switch. Declare sets explicitly within the Tablo file (e.g.,  ENDWM # mobile endowment # (capital,unsklab,sklab); {.emph not} ENDWM # mobile endowments # = (all,e,ENDW:ENDOWFLAG(e,\"mobile\") ne 0);"
    )
  }),
  tar_target(deploy_err, {
    list(invalid_write_dir = "The path provided for {.arg write_dir}, {.path {a$write_dir}}, does not exist.")
  }),
  tar_target(set_err, {
    list(
      while_loop = "Construction of dependent sets has failed on: {null_sets}.",
      invalid_plus = "The set operator `+` was used where there are overlapping set elements {.field {d}}, violating the condition that the sets be disjoint."
    )
  }),
  tar_target(cls_err, {
    list(
      invalid_internal = c(
        "The closure file inferred from the provided {.arg tab_file}: {.val {file}} does not exist.",
        "Currently supported internal {file_type} files are available for: {.val {valid_internal_files}}.",
        "Alternatively, path to a user-provided {file_type} file is supported (e.g., \"/my/{file_type}/path.{ext}\")",
        "Note that user-provided {file_type} files may need to be modified for compatibility with various {.pkg teems} functions."
      ),
      no_var = "{l_var} variable{?s} from the closure file not present in the Tablo file: {.val {var_discrepancy}}.",
      no_cls = "A closure file must be provided when a user-provided Tablo file has been supplied.",
      entry_type = "The following closure entries have not been classified properly: {invalid_entry}.",
      ele_invalid = "The closure entry tuple {.field {cls_entry}} is invalid under the current set mapping.",
      mixed_invalid = "{n_invalid_entries} closure entry element{?s} in {.field {cls_entry}} do not belong to the respective variable sets: {invalid_entries}.",
      subset_invalid = "Some entries from {.field {cls_entry}} do not belong to the respective variables sets indicating an invalid subset.",
      invalid_full = "A full swap on the variable {.val {var_name}} is not possible as the variable is not fully exogenous.",
      no_var_cls = "There is no closure entry for the selected swap-out variable {.val {var_name}}.",
      missing_specification = "The closure provided must contain both an \"Exogenous\" entry and a \"Rest Endogenous\" entry. Note that the inverse approach is not currently supported."
    )
  }),
  tar_target(shk_err, {
    list(
      cst_scen_val_file = "The last column in the loaded file {.file {input}} must be a {.field Value} column.",
      cst_scen_val_df = "{.obj_type_friendly {input}} supplied as a shock must have {.field Value} as the last column.",
      scen_year_file = "No {.field Year} column was found in the loaded file {.file {input}}.",
      scen_year_df = "{.obj_type_friendly {input}} supplied as a scenario shock must have a column designating {.field Year} consistent with selected time steps.",
      uni_named_lst = "{.arg ...} must consist of named pairs in the format {.code SETi = set_element}",
      unneeded_dots = "{.arg ...} are only utilized for shock type {.val uniform}.",
      not_a_shk = "The value provided to {.arg shock} is not an object created with {.fun teems::emsshock}.",
      not_a_var = "The variable designated for a shock: {.val {var_name}}  was not found within the model Tablo file.",
      extra_col = "If {.field Year} is provided in lieu of the intertemporal set, the intertemporal set {.field {supplied_int_set}} is not necessary.",
      invalid_set = c(
        "{l_errant_set} set{?s} designated for an element-specific uniform shock: {.field {errant_set}} not applicable to the variable {.val {var_name}}.",
        "Set designations within {.pkg teems} comprise the variable-specific uppercase set name and lowercase index.",
        "For {.val {var_name}} these include: {.field {ls_mixed}}.",
        "In intertemporal models, {.field Year} may be provided in lieu of an intertemporal set."
      ),
      x_full_exo = "The variable {.field {raw_shock$var}} was assigned a shock over the entire variable yet is not fully exogenous.",
      x_full_exo_part = "The variable {.field {raw_shock$var}} was assigned a shock over part of the variable yet no components are exogenous.",
      uni_invalid_year = "The Year provided for a shock {.val {Year}} is not among years consistent with provided time steps {.field {CYRS}}.",
      uni_invalid_ele = c(
        "The element: {.val {ele}} is not found within the associated set: {.field {ele_set}}",
        "Valid elements with the current mapping include: {.val {recognized_ele}}."
      ),
      x_part_exo = "The following tuples have been allocated a shock but are not exogenous: {.field {errant_tup}}.",
      cust_invalid_year = "{n_errant_year_tuples} tuple{?s} in the provided custom shock file contain invalid {.field Year} specifications: {.field {errant_year_tuples}}.",
      cust_invalid_tup = "Some tuples provided to a {.arg custom} shock indicate elements used that do not belong to their respective sets: {.field {errant_tuples}}.",
      cust_endo_tup = "Some tuples designated for a shock do not have exogenous status: {.field {x_exo_parts}}.",
      scen_dynamic = "Shock type {.arg scenario} is only valid for temporally dynamic models.",
      scen_missing_tup = c(
        "{n_missing_tuples} tuple{?s} in the provided scenario shock file were missing: {.field {missing_tuples}}.",
        "Note that scenario shocks are subject to aggregation and must contain all unaggregated database- and variable-specific elements for associated sets."
      ),
      shk_file_shocks = "No additional shocks are accepted if a shock file is provided."
    )
  }),
  tar_target(shk_infm, {
    list(uni_named_lst = "Note that set names consist of the concatenation of the set name and variable-specific lowercase index.")
  }),
  tar_target(shk_url, {
    list(
      custom = NULL,
      scenario = NULL,
      uniform = NULL
    )
  }),
  tar_target(swap_err, {
    list(
      invalid_swap = "Invalid list object supplied as swap.",
      no_var = "The variable {.val {var_name}} designated for a swap is not found within the model Tablo file provided.",
      invalid_set = c(
        "The swap set {.val {non_exist_set}} is not associated with the variable {.val {var_name}}.",
        "Note that set designations within {.pkg teems} are comprised of the variable-specific uppercase set name and lowercase index.",
        "For {.val {var_name}} these include: {.field {ls_mixed}}."
      ),
      invalid_comp = c(
        "Elements or subsets designated for a swap are not part of the set {.field {nm}}: {invalid_comp}.",
        "Valid elements include {.val {valid_ele}}.",
        "Valid subsets include {.val {valid_subsets}}."
      ),
      invalid_tup = "{n_invalid_tuples} tuple{?s} designated to be swapped out are not identified as exogenous: {invalid_tuples}.",
      overlap_ele = "{n_overlap} tuple{?s} for {.val {e}} in the supplied pre-swap closure has multiple entries: {overlap}."
    )
  }),
  tar_target(solve_err, {
    list(
      missing_dots = "All input files must be passed as {.arg ...} under the `in-situ solve` method (i.e., when {.arg cmf_file} is not provided and the pipeline is bypassed).",
      insitu_missing_input = "The Tablo file provided indicates that the following files are required: {.val {req_files}} however one or more appear to not have been provided: {.val {missing_files}}.",
      insitu_no_file = "One or more input files provided does not exist: {.val {nonexist_files}}.",
      x_integerish = "{.arg {arg}} must be integer-like.",
      invalid_length = "{.arg {arg}} must be an integer-like numeric of length 1.",
      subint_form = "{.arg n_subintervals} must contain either all even numbers or all odd numbers.",
      step_length = "{.arg steps} must be a numeric vector of length 3.",
      invalid_method = "{.arg matrix_method} {.val {matrix_method}} only applicable to intertemporal model runs.",
      solution_err = "Errors detected during solution. See {.path {paths$diag_out}}.",
      solution_sing = "Singularity detected during solution. See {.path {paths$diag_out}}.",
      NDBBD_time = "The {.arg n_timesteps} argument is required when using the NDBBD matrix method.",
      docker_installed = "Docker is required to call the solver and is not installed on this system.",
      docker_sudo = "Docker is installed but cannot be called without sudo.",
      docker_x_image = "The {.val {image_name}} Docker image is not present."
    )
  }),
  tar_target(solve_wrn, {
    list(
      accuracy = c(
        "Only {.emph {accuracy}} of variables accurate at at least 4 digit precision, less than {a_threshold}.",
        "See {.arg accuracy_threshold} within {.fun teems::ems_option_set} to adjust the warning threshold."
      )
    )
  }),
  tar_target(solve_info, {
    list(
      in_situ = "\"solve-in-situ\" mode activated.",
      terminal_run = "{.arg terminal_run} mode has been selected enabling model runs outside of your R IDE or R script. The following steps are necessary to solve the model and compose outputs.",
      t_run_append = substitute(cli::cli_ol(
        c(
          "Run the following command at your OS terminal: {.val {solve_cmd}}",
          "If errors are present in the terminal output during an ongoing run, it is possible to stop the relevant {.val {hsl}} process early according to your OS-specific system activity monitor.",
          "Error and singularity indicators will be present in the model diagnostic output: {.val {diag_out}}.",
          "If no errors or singularities are detected, run the following command to convert binary outputs: {.val {sol_parse_cmd}}.",
          "The {.arg cmf_path} path to use as a value within {.fun teems::ems_compose} is {.path {cmf_path}}."
        )
      )),
      accuracy = "{.emph {accuracy}} of variables accurate at at least 4 digit precision.",
      elapsed_time = "Elapsed time: {elapsed_time}"
    )
  }),
  tar_target(compose_err, {
    list(
      x_model_dir = "Model directory {.path {model_dir}} not found. This would indicate that the {.arg cmf_path} provided, {.path {cmf_path}}, is not correct.",
      x_var_out = "Model outputs not found. This would indicate that a model run at the {.arg cmf_path} provided, {.path {cmf_path}}, has not taken place.",
      set_mismatch = "Tablo-parsed sets and/or elements are not identical to post-model binary set outputs. If the Tablo file contains set writeout, this is likely an internal error and should be forwarded to the package maintainer. Otherwise, set {.arg post_set_check} to {.val FALSE} within {.fun teems::ems_option_set}.",
      idx_mismatch = "Index mismatch detected on output variables in {.fun teems::ems_compose}.",
      lax_check = "Lax column name check failed: One or more column names in parsed variable data.tables is not present in the tab extract.",
      strict_check = "Strict column name check failed: One or more column names is either not present or in a different order than that of the tab extract.",
      var_check = "Name discrepancy in parsed variables with respect to variable extract names.",
      coeff_check = "One or more coefficients identified from the Tablo extract was not found in the output csvs.",
      invalid_coeff_set = "It appears that a set isn't found which likely means a space around a coefficient set declaration (e.g., (all, r, REG) instead of (all,r,REG).",
      missing_sets = "Set information missing from binary outputs: {.field {x_sets}}."
    )
  }),
  tar_target(check_wrn, {
    list(var_baseline = "Range across all variables is {.val {all_range}}, rather than the expected {.val 0}.")
  }),
  tar_target(check_err, {
    list(missing_coeff = "Input coefficient headers missing from output: {.field {x_coeff}}.")
  }),
  # internal data ====================================================
  tar_target(internal_data, {
    usethis::use_data(
      mappings,
      internal_tab,
      tab_qual,
      # `GTAP-INTv2`,
      param_weights,
      internal_cls,
      set_conversion,
      coeff_conversion,
      gen_info,
      gen_wrn,
      gen_err,
      gen_url,
      data_wrn,
      data_err,
      model_wrn,
      model_err,
      load_err,
      deploy_err,
      shk_err,
      swap_err,
      set_err,
      cls_err,
      shk_infm,
      shk_url,
      solve_err,
      solve_wrn,
      solve_info,
      compose_err,
      check_wrn,
      check_err,
      overwrite = TRUE,
      internal = TRUE
    )
  })
)
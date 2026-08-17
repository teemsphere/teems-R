build_solve_err <- function() {
  list(
    no_insitu_inputs = "No input files loaded; all files must be passed as named arguments via {.arg ...}.",
    missing_insitu_inputs = "Required files {.val {req_inputs}} not all provided; missing: {.val {missing_files}}.",
    insitu_no_file = "Input file{?s} not found: {.val {nonexist_files}}.",
    # test-ems_solve.R: "ems_solve errors when n_tasks is not integerish"
    x_integerish = "{.arg {arg}} must be integer-like.",
    # test-ems_complementarity.R: constructor validation
    comp_arg_type = "{.arg {bad_arg}} must be {requirement}.",
    # test-ems_complementarity.R: "both runs disabled aborts"
    comp_runs_off = c(
      "{.arg do_approx_run} and {.arg do_acc_run} cannot both be {.val FALSE}.",
      "Skipping the approximate run takes the pre-simulation states as
      the accurate run's targets; skipping the accurate run keeps the
      approximate solution as the result (GEMPACK manual 51.6).
      Skipping both leaves nothing to solve."
    ),
    # test-solver_switches.R: mode-switch validation
    switch_mode = "{.arg {bad_arg}} must be one of {.val fatal}, {.val warn} or {.val off}.",
    # test-ems_complementarity.R: "ems_solve rejects a non-spec complementarity"
    comp_spec_class = c(
      "{.arg complementarity} must be built by {.fun ems_complementarity}.",
      "Example: {.code complementarity = ems_complementarity(steps_approx_run = 20L)}."
    ),
    invalid_length = "{.arg {arg}} must be an integer-like numeric of length 1.",
    # test-ems_solve.R: "ems_solve errors when steps are not all even for Gragg"
    step_parity = c(
      "{.arg steps} must be all even when {.arg solution_method} is {.val Gragg}.",
      "Gragg's method guarantees its accuracy properties for even step counts only (Pearson 1991, Theorem 6.1)."
    ),
    # test-ems_solve.R: "ems_solve errors when steps is not length 3"
    step_length = "{.arg steps} must be a numeric vector of length 3.",
    # test-ems_solve.R: "ems_solve errors when steps is not length 1 for RK"
    step_single_rk = c(
      "{.arg steps} must be a single positive integer when {.arg solution_method} is {.val {solution_method}}.",
      "Runge-Kutta methods take one step count (e.g. {.code steps = 8L}); they use no Richardson extrapolation, so no step-count triple is involved."
    ),
    # test-ems_solve.R: "ems_solve errors when adaptive used with non-embedded method"
    adaptive_method = c(
      "{.arg adaptive} {.val {adaptive}} requires an embedded Runge-Kutta {.arg solution_method} ({.val BoSha32} or {.val DoPri54}).",
      "Only the embedded pairs provide the per-step error estimate the adaptive controller acts on."
    ),
    # test-ems_solve.R: "ems_solve errors when subintervals used with RK"
    rk_subintervals = c(
      "{.arg n_subintervals} must be 1 when {.arg solution_method} is {.val {solution_method}}.",
      "Subintervals restart the integrator and only benefit the extrapolating methods; increase {.arg steps} (or use {.arg adaptive}) instead."
    ),
    # test-ems_solve.R: "ems_solve errors when eps_tolerance is invalid"
    epstol_range = "{.arg eps_tolerance} must be a positive numeric of length 1.",
    # test-ems_RK.R / test-ems_solve.R: unknown arguments in the
    # ems_solve dots
    solver_dots = c(
      "Unknown argument{?s} {.arg {unknown_args}} passed to {.arg ...}.",
      "{.arg ...} accepts the Runge-Kutta step controls
      ({.arg adaptive}, {.arg eps_tolerance}, {.arg max_retries},
      {.arg retry_adjust}; see {.fun ems_RK}), the MA48 workspace
      initial guesses ({.arg laA}, {.arg laD}, {.arg laDi}) and the
      expert solver flags ({.arg fastrefac}, {.arg gpzerodivide},
      {.arg cntl_3}, {.arg cntl_6}, {.arg nsbbdblocks},
      {.arg withmc66}, {.arg smllthreads}, {.arg tempdir},
      {.arg nowrites}, {.arg condest})."
    ),
    # test-solve_in_situ.R: solver_args must be a fully named list
    solver_args_list = c(
      "{.arg solver_args} must be a fully named list.",
      "It carries the named solver arguments {.fun ems_solve} accepts
      through {.arg ...}; the in-situ {.arg ...} is reserved for the
      input files."
    ),
    # test-solve_in_situ.R: unknown names in solver_args
    solver_args_unknown = c(
      "Unknown argument{?s} {.arg {unknown_args}} in {.arg solver_args}.",
      "Accepted: the MA48 workspace initial guesses ({.arg laA},
      {.arg laD}, {.arg laDi}) and the expert solver flags
      ({.arg fastrefac}, {.arg gpzerodivide}, {.arg cntl_3},
      {.arg cntl_6}, {.arg nsbbdblocks}, {.arg withmc66},
      {.arg smllthreads}, {.arg tempdir}, {.arg nowrites},
      {.arg condest}). The
      Runge-Kutta step controls are formal arguments of
      {.fun solve_in_situ}."
    ),
    # test-ems_solve.R: "ems_solve errors when steps are not increasing"
    step_increasing = c(
      "{.arg steps} must be strictly increasing for {.arg solution_method} {.val {solution_method}}.",
      "Richardson extrapolation combines three solutions computed with distinct, increasing step counts."
    ),
    # test-ems_solve.R: "ems_solve errors when SBBD used with static model"
    invalid_method = "{.arg matrix_method} {.val {matrix_method}} only applicable to intertemporal model runs.",
    solution_err = "Errors detected during solution. See {.path {paths$diag_out}}.",
    solution_sing = c(
      "Singularity detected during solution. See {.path {paths$diag_out}}.",
      "A square-but-singular system usually indicates a structurally deficient closure partition.",
      "Run {.fun teems::ems_probe} on the deployed model or re-solve with {.code pre_probe = TRUE} for a named structural diagnosis."
    ),
    # test-chk_solver_log.R: condest near-singularity is a warning
    # (the run completed; the verdict is the modeller's to act on)
    condest_nearsing = c(
      "The {.code condest} diagnostic reports the linear system as numerically near-singular (kappa_w2 {kappa_w2}): solutions are unreliable.",
      "The structural probe may pass -- look for near-zero data flows carried by the closure, or re-solve with {.code precision = \"f64\"}. See {.path {paths$diag_out}}."
    ),
    # test-chk_solver_log.R: "TAB errors map to the model-specification abort"
    # lines 2-4 filled by .check_solver_log; line 3 dropped when no
    # manual section applies
    solver_tab = c(
      "The solver rejected the model specification with {n_err} error{?s}:",
      "{err_preview}",
      "See GEMPACK manual section{?s} {.val {manual_secs}}.",
      "Full log: {.path {diag_out}}."
    ),
    # test-chk_solver_log.R: "closure errors map to the closure abort"
    solver_closure = c(
      "The solver rejected the closure or shock inputs with {n_err} error{?s}:",
      "{err_preview}",
      "Check the closure, swap and shock arguments supplied to {.fun teems::ems_model} and {.fun teems::ems_deploy}.",
      "Full log: {.path {diag_out}}."
    ),
    # test-chk_solver_log.R: "data errors map to the data abort"
    solver_data = c(
      "The solver could not read the model data with {n_err} error{?s}:",
      "{err_preview}",
      "Check the data inputs supplied to {.fun teems::ems_data} and the headers named in the TAB file.",
      "Full log: {.path {diag_out}}."
    ),
    # test-chk_solver_log.R: "runtime errors map to the numeric abort"
    solver_numeric = c(
      "The solver stopped on {n_err} runtime error{?s} while evaluating model values:",
      "{err_preview}",
      "See GEMPACK manual section{?s} {.val {manual_secs}}.",
      "Full log: {.path {diag_out}}."
    ),
    # not simulated
    docker_installed = "Docker is required but not installed.",
    docker_sudo = "Docker is installed but cannot be called without sudo.",
    docker_not_running = "Docker is installed but the daemon is not running. Start Docker Desktop and try again.",
    docker_x_image = "The {.val {image_name}} Docker image is not present.",
    no_model_dir = "The {.arg model_dir} provided {.path {model_dir}} does not exist.",
    no_arg_name = "Additional arguments to be passed to the solver must be named: {.code smllthreads = 1}",
    no_input_names = "Input files provided to {.arg ...} must be named as the appear within the {.arg model_file}.",
    # test-ems_solve.R: "ems_solve errors when inmemory is not a logical scalar"
    logical_scalar = "{.arg {arg}} must be logical of length 1.",
    # test-ems_solve.R: "ems_solve errors when verbosity is out of range"
    verbosity_range = "{.arg verbosity} must be 0, 1, or 2.",
    # test-chk_solver_log.R: "a non-zero exit status aborts even with a clean log"
    solver_exit = "The solver exited with status {status} without a recognised error in its log. See {.path {paths$diag_out}}."
  )
}

build_solve_wrn <- function() {
  list(
    accuracy = c(
      "Only {.emph {accuracy}} of variables accurate to at least 4 digits, below the {a_threshold} threshold.",
      "Adjust with {.arg accuracy_threshold} in {.fun teems::ems_option_set}."
    )
  )
}

build_solve_info <- function() {
  list(
    in_situ = "\"solve-in-situ\" mode activated.",
    # test-ems_solve.R: "matrix_method auto resolves by model type"
    auto_method = "{.arg matrix_method} {.val auto}: using {.val {chosen}} for this {model_type} model.",
    auto_dbbd_hint = "This static model's size favors {.val DBBD}: {.code n_tasks = 4} with {.arg matrix_method} {.val DBBD} (or {.val auto}) may solve faster than single-task {.val LU}.",
    # test-ems_solve.R: "condensed deployments advise against bordered methods"
    condense_bordered = c(
      "This deployment is condensed ({n_backsolve} backsolved variable{?s}, {share} of the uncondensed system) and {.val {matrix_method}} is a bordered method.",
      "Substitution densifies the diagonal blocks the bordered methods exploit: condensed deployments solve slower at every elimination share.",
      "Condensation pays under {.val LU}; deploy without {.arg backsolve} for bordered runs ({.arg omit} is unaffected -- omission does not densify)."
    ),
    # test-ems_solve.R: "condensed intertemporal deployments are advised against"
    condense_intertemporal = c(
      "This intertemporal deployment is condensed ({n_backsolve} backsolved variable{?s}, {share} of the uncondensed system).",
      "Condensation is counterproductive on intertemporal models: bordered runs solve slower condensed, and a fully condensed {.val LU} run is slower still than plain {.val SBBD}.",
      "Deploy without {.arg backsolve} and solve with {.val SBBD} ({.arg omit} is unaffected -- omission does not densify)."
    ),
    # test-ems_solve.R: "docker tag auto-selection"
    docker_tag_auto = "Using image {.field teems:{tag}} (matches host CPU capability {.val {level}}). Set {.arg docker_tag} via {.fn ems_option_set} to override.",
    terminal_run = "{.arg terminal_run} activated. To solve and compose outputs:",
    terminal_run_steps = c("Run the above command in your OS terminal.",
                           "If errors are present in the terminal output during an ongoing run, it is possible to stop the relevant {.field {hsl}} process early according to your OS-specific system activity monitor.",
                           "Any error and/or singularity indicators will be present in the model diagnostic output: {.path {diag_out}}.",
                           "If no errors or singularities are detected, use the following expression to structure solver binary outputs: {.run ems_compose({cmf_path})}"),
    accuracy = "{.emph {accuracy}} of variables accurate to at least 4 digits.",
    elapsed_time = "Elapsed time: {elapsed_time}"
  )
}

build_solve_err <- function() {
  list(
    no_insitu_inputs = "No input files loaded; all files must be passed as named arguments via {.arg ...}.",
    missing_insitu_inputs = "Required files {.val {req_inputs}} not all provided; missing: {.val {missing_files}}.",
    insitu_no_file = "Input file{?s} not found: {.val {nonexist_files}}.",
    # test-ems_solve.R: "ems_solve errors when n_tasks is not integerish"
    x_integerish = "{.arg {arg}} must be integer-like.",
    invalid_length = "{.arg {arg}} must be an integer-like numeric of length 1.",
    # test-ems_solve.R: "ems_solve errors when steps are mixed odd/even"
    subint_form = "{.arg n_subintervals} must be all even or all odd.",
    # test-ems_solve.R: "ems_solve errors when steps is not length 3"
    step_length = "{.arg steps} must be a numeric vector of length 3.",
    # test-ems_solve.R: "ems_solve errors when SBBD used with static model"
    invalid_method = "{.arg matrix_method} {.val {matrix_method}} only applicable to intertemporal model runs.",
    solution_err = "Errors detected during solution. See {.path {paths$diag_out}}.",
    solution_sing = "Singularity detected during solution. See {.path {paths$diag_out}}.",
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
    verbosity_range = "{.arg verbosity} must be 0, 1, or 2."
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
    terminal_run = "{.arg terminal_run} activated. To solve and compose outputs:",
    terminal_run_steps = c("Run the above command in your OS terminal.",
                           "If errors are present in the terminal output during an ongoing run, it is possible to stop the relevant {.field {hsl}} process early according to your OS-specific system activity monitor.",
                           "Any error and/or singularity indicators will be present in the model diagnostic output: {.path {diag_out}}.",
                           "If no errors or singularities are detected, use the following expression to structure solver binary outputs: {.run ems_compose({cmf_path})}"),
    accuracy = "{.emph {accuracy}} of variables accurate to at least 4 digits.",
    elapsed_time = "Elapsed time: {elapsed_time}"
  )
}

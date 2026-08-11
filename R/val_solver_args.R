#' @importFrom rlang arg_match is_integerish
#'
#' @keywords internal
#' @noRd
.validate_solver_args <- function(a,
                                  paths,
                                  call) {
  
  solution_method <- a$solution_method
  a$solution_method <- rlang::arg_match(
    arg = solution_method,
    values = c("Johansen", "Gragg", "Euler", "RK2", "RK4", "BoSha32", "DoPri54"),
    error_call = call
  )
  is_rk <- a$solution_method %in% c("RK2", "RK4", "BoSha32", "DoPri54")
  is_rk_embedded <- a$solution_method %in% c("BoSha32", "DoPri54")

  adaptive <- a$adaptive
  a$adaptive <- rlang::arg_match(
    arg = adaptive,
    values = c("no", "yes", "accuracy-only"),
    error_call = call
  )
  
  matrix_method <- a$matrix_method
  a$matrix_method <- rlang::arg_match(
    arg = matrix_method,
    values = c("LU", "DBBD", "SBBD", "NDBBD", "auto"),
    error_call = call
  )
  
  checklist <- list(
    cmf_path = "character",
    solution_method = "character",
    matrix_method = "character",
    n_subintervals = c("numeric", "integer"),
    steps = c("numeric", "integer"),
    adaptive = "character",
    eps_tolerance = c("numeric", "integer"),
    max_retries = c("NULL", "numeric", "integer"),
    retry_adjust = c("NULL", "numeric"),
    n_tasks = c("numeric", "integer"),
    n_threads = c("numeric", "integer"),
    laA = c("numeric", "integer"),
    laD = c("numeric", "integer"),
    laDi = c("numeric", "integer"),
    inmemory = c("NULL", "logical"),
    verbosity = c("NULL", "numeric", "integer"),
    suppress_outputs = "logical",
    terminal_run = "logical",
    assertions = c("NULL", "character"),
    range_test_initial = c("NULL", "character"),
    range_test_updated = c("NULL", "character"),
    postsim = c("NULL", "logical"),
    complementarity = c("NULL", "teems_complementarity"),
    append_args = c("NULL", "character"),
    pre_probe = "logical"
  )
  if (!is.null(a$complementarity) &&
    !inherits(a$complementarity, "teems_complementarity")) {
    .cli_action(solve_err$comp_spec_class,
      action = c("abort", "inform"),
      call = call
    )
  }
  if (!rlang::is_integerish(a$n_threads) || length(a$n_threads) != 1L ||
    a$n_threads < 1) {
    bad_arg <- "n_threads"
    requirement <- "a positive integer-like numeric of length 1"
    .cli_action(solve_err$comp_arg_type,
      action = "abort",
      call = call
    )
  }
  if (!is.null(a$max_retries) &&
    (!rlang::is_integerish(a$max_retries) || length(a$max_retries) != 1L ||
      a$max_retries < 1)) {
    bad_arg <- "max_retries"
    requirement <- "a positive integer-like numeric of length 1"
    .cli_action(solve_err$comp_arg_type,
      action = "abort",
      call = call
    )
  }
  if (!is.null(a$retry_adjust) &&
    (!is.numeric(a$retry_adjust) || length(a$retry_adjust) != 1L ||
      is.na(a$retry_adjust) ||
      a$retry_adjust <= 0 || a$retry_adjust >= 1)) {
    bad_arg <- "retry_adjust"
    requirement <- "a numeric of length 1 in (0, 1)"
    .cli_action(solve_err$comp_arg_type,
      action = "abort",
      call = call
    )
  }
  for (nme in c("assertions", "range_test_initial", "range_test_updated")) {
    x <- a[[nme]]
    if (!is.null(x) &&
      (!is.character(x) || length(x) != 1L || !x %in% c("fatal", "warn", "off"))) {
      bad_arg <- nme
      .cli_action(solve_err$switch_mode,
        action = "abort",
        call = call
      )
    }
  }
  if (!is.null(a$postsim) &&
    (!is.logical(a$postsim) || length(a$postsim) != 1L || is.na(a$postsim))) {
    bad_arg <- "postsim"
    requirement <- "a non-missing logical of length 1"
    .cli_action(solve_err$comp_arg_type,
      action = "abort",
      call = call
    )
  }

  .check_arg_class(
    args_list = a,
    checklist = checklist,
    call = call
  )

  if (!rlang::is_integerish(a$n_tasks)) {
    arg <- "n_tasks"
    .cli_action(solve_err$x_integerish,
      action = "abort",
      call = call
    )
  }

  if (as.integer(length(a$n_tasks)) %!=% 1L) {
    arg <- "n_tasks"
    .cli_action(solve_err$invalid_length,
      action = "abort",
      call = call
    )
  }

  if (!rlang::is_integerish(a$n_subintervals)) {
    arg <- "n_subintervals"
    .cli_action(solve_err$x_integerish,
      action = "abort",
      call = call
    )
  }

  if (as.integer(length(a$n_subintervals)) %!=% 1L) {
    arg <- "n_subintervals"
    .cli_action(solve_err$invalid_length,
      action = "abort",
      call = call
    )
  }

  if (!is.null(a$inmemory) && (as.integer(length(a$inmemory)) %!=% 1L || is.na(a$inmemory))) {
    arg <- "inmemory"
    .cli_action(solve_err$logical_scalar,
      action = "abort",
      call = call
    )
  }

  if (as.integer(length(a$pre_probe)) %!=% 1L || is.na(a$pre_probe)) {
    arg <- "pre_probe"
    .cli_action(probe_err$x_logical,
      action = "abort",
      call = call
    )
  }

  if (!is.null(a$verbosity)) {
    if (!rlang::is_integerish(a$verbosity)) {
      arg <- "verbosity"
      .cli_action(solve_err$x_integerish,
        action = "abort",
        call = call
      )
    }
    if (as.integer(length(a$verbosity)) %!=% 1L) {
      arg <- "verbosity"
      .cli_action(solve_err$invalid_length,
        action = "abort",
        call = call
      )
    }
    if (!a$verbosity %in% c(0, 1, 2)) {
      .cli_action(solve_err$verbosity_range,
        action = "abort",
        call = call
      )
    }
  }

  if (is_rk) {
    if (!all(
      is.numeric(a$steps), length(a$steps) == 1,
      rlang::is_integerish(a$steps), a$steps >= 1
    )) {
      solution_method <- a$solution_method
      .cli_action(solve_err$step_single_rk,
        action = c("abort", "inform"),
        call = call
      )
    }
    if (a$adaptive %!=% "no" && !is_rk_embedded) {
      adaptive <- a$adaptive
      .cli_action(solve_err$adaptive_method,
        action = c("abort", "inform"),
        call = call
      )
    }
    if (a$n_subintervals != 1) {
      solution_method <- a$solution_method
      .cli_action(solve_err$rk_subintervals,
        action = c("abort", "inform"),
        call = call
      )
    }
    if (!all(is.numeric(a$eps_tolerance), length(a$eps_tolerance) == 1, a$eps_tolerance > 0)) {
      .cli_action(solve_err$epstol_range,
        action = "abort",
        call = call
      )
    }
  } else {
    if (!all(is.numeric(a$steps), length(a$steps) == 3)) {
      .cli_action(solve_err$step_length,
        action = "abort",
        call = call
      )
    }
    if (a$solution_method %=% "Gragg" && !all(a$steps %% 2 == 0)) {
      .cli_action(solve_err$step_parity,
        action = c("abort", "inform"),
        call = call
      )
    }
    if (a$solution_method %in% c("Gragg", "Euler") && !all(diff(a$steps) > 0)) {
      solution_method <- a$solution_method
      .cli_action(solve_err$step_increasing,
        action = c("abort", "inform"),
        call = call
      )
    }
  }

  if ("tab_path" %in% names(attributes(paths$cmf))) {
    tab <- readLines(attr(paths$cmf, "tab_path"))
  } else {
    tab <- .retrieve_cmf(
      file = "tabfile",
      cmf_path = paths$cmf
    )
    tab <- readLines(tab)
  }

  if (any(grepl(pattern = "(intertemporal)", tab))) {
    a$enable_time <- TRUE
  } else {
    a$enable_time <- FALSE
  }

  if (a$matrix_method %=% "auto") {
    a$matrix_method <- .resolve_auto_method(
      enable_time = a$enable_time,
      n_tasks = a$n_tasks,
      cmf_path = paths$cmf
    )
  }

  if (a$matrix_method %in% c("SBBD", "NDBBD") && !a$enable_time) {
    matrix_method <- a$matrix_method
    .cli_action(solve_err$invalid_method,
      action = "abort",
      call = call
    )
  }

  a$matsol <- switch(
    EXPR = a$matrix_method,
    "LU" = 0,
    "SBBD" = 1,
    "DBBD" = 2,
    "NDBBD" = 3
  )

  if (a$solution_method %in% c("Gragg", "Euler") || is_rk) {
    a$solmed <- a$solution_method
  } else {
    a$solmed <- "Johansen"
    a$n_subintervals <- 1
  }

  return(a)
}
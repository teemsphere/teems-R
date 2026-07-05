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
    values = c("Johansen", "mod_midpoint"),
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
    n_tasks = c("numeric", "integer"),
    laA = c("numeric", "integer"),
    laD = c("numeric", "integer"),
    laDi = c("numeric", "integer"),
    suppress_outputs = "logical",
    terminal_run = "logical",
    append_args = c("NULL", "character")
  )

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

  if (!(all(a$steps %% 2 == 0) || all(a$steps %% 2 == 1))) {
    .cli_action(solve_err$subint_form,
      action = "abort",
      call = call
    )
  }
  if (!all(is.numeric(a$steps), length(a$steps) == 3)) {
    .cli_action(solve_err$step_length,
      action = "abort",
      call = call
    )
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

  if (a$matrix_method %=% "NDBBD") {
    a$nesteddbbd <- 1
  } else {
    a$nesteddbbd <- 0
  }

  if (a$solution_method %=% "mod_midpoint") {
    a$solmed <- "Mmid"
  } else {
    a$solmed <- "Johansen"
    a$n_subintervals <- 1
  }

  return(a)
}
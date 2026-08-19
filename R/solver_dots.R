#' Named solver arguments accepted beyond the formals: the MA48
#' workspace initial guesses and the expert solver flags formerly
#' passed as raw `append_args` strings. `ems_solve()` and
#' `ems_probe()` take them through `...`; `solve_in_situ()` through
#' `solver_args` (its dots carry the input files). All are validated
#' against this table -- a typo is an R-side error, never a silently
#' ignored solver flag.
#'
#' @keywords internal
#' @noRd
.solver_extra_args <- function() {
  list(
    laA = NULL,
    laD = NULL,
    laDi = NULL,
    fastrefac = NULL,
    gpzerodivide = NULL,
    cntl_3 = NULL,
    cntl_6 = NULL,
    nsbbdblocks = NULL,
    withmc66 = NULL,
    smllthreads = NULL,
    tempdir = NULL,
    nowrites = NULL,
    condest = NULL,
    ma48u = NULL
  )
}

#' Positional checklist entries for the extras, in
#' `.solver_extra_args()` order (`.check_arg_class` pairs by
#' position).
#'
#' @keywords internal
#' @noRd
.solver_extra_checklist <- function() {
  list(
    laA = c("NULL", "numeric", "integer"),
    laD = c("NULL", "numeric", "integer"),
    laDi = c("NULL", "numeric", "integer"),
    fastrefac = c("NULL", "logical"),
    gpzerodivide = c("NULL", "logical"),
    cntl_3 = c("NULL", "numeric"),
    cntl_6 = c("NULL", "numeric"),
    nsbbdblocks = c("NULL", "numeric", "integer"),
    withmc66 = c("NULL", "logical"),
    smllthreads = c("NULL", "numeric", "integer"),
    tempdir = c("NULL", "character"),
    nowrites = c("NULL", "logical"),
    condest = c("NULL", "logical"),
    ma48u = c("NULL", "numeric")
  )
}

#' Scalar/value checks for the extras (classes are checked
#' positionally by `.check_arg_class`).
#'
#' @importFrom rlang is_integerish
#'
#' @keywords internal
#' @noRd
.validate_solver_extras <- function(a, call) {
  for (nme in c("laA", "laD", "laDi", "nsbbdblocks", "smllthreads")) {
    x <- a[[nme]]
    if (!is.null(x) &&
      (!rlang::is_integerish(x) || length(x) != 1L || is.na(x) || x < 1)) {
      bad_arg <- nme
      requirement <- "a positive integer-like numeric of length 1"
      .cli_action(solve_err$comp_arg_type,
        action = "abort",
        call = call
      )
    }
  }
  for (nme in c("fastrefac", "gpzerodivide", "withmc66", "nowrites", "condest")) {
    x <- a[[nme]]
    if (!is.null(x) && (!is.logical(x) || length(x) != 1L || is.na(x))) {
      bad_arg <- nme
      requirement <- "a non-missing logical of length 1"
      .cli_action(solve_err$comp_arg_type,
        action = "abort",
        call = call
      )
    }
  }
  for (nme in c("cntl_3", "cntl_6")) {
    x <- a[[nme]]
    if (!is.null(x) && (!is.numeric(x) || length(x) != 1L || is.na(x))) {
      bad_arg <- nme
      requirement <- "a numeric of length 1"
      .cli_action(solve_err$comp_arg_type,
        action = "abort",
        call = call
      )
    }
  }
  # MA48/MP48 pivot threshold CNTL(2): absent = each library's default
  # (MA48 0.1, HSL_MP48 0.01); the solver validates the same range
  if (!is.null(a$ma48u) &&
    (!is.numeric(a$ma48u) || length(a$ma48u) != 1L || is.na(a$ma48u) ||
      a$ma48u <= 0 || a$ma48u > 1)) {
    bad_arg <- "ma48u"
    requirement <- "a numeric of length 1 in (0, 1]"
    .cli_action(solve_err$comp_arg_type,
      action = "abort",
      call = call
    )
  }
  if (!is.null(a$tempdir) &&
    (!is.character(a$tempdir) || length(a$tempdir) != 1L || is.na(a$tempdir))) {
    bad_arg <- "tempdir"
    requirement <- "a character of length 1"
    .cli_action(solve_err$comp_arg_type,
      action = "abort",
      call = call
    )
  }
  return(invisible(NULL))
}

#' Render the expert flags onto the solver command line. Absent
#' (NULL) values emit nothing: the solver applies its own defaults
#' and records the effective values in sol.stats.json.
#'
#' @keywords internal
#' @noRd
.extra_cli_flags <- function(a) {
  as01 <- function(x) as.integer(isTRUE(x))
  flags <- c(
    if (!is.null(a$fastrefac)) paste("-fastrefac", as01(a$fastrefac)),
    if (!is.null(a$gpzerodivide)) paste("-gpzerodivide", as01(a$gpzerodivide)),
    if (!is.null(a$cntl_3)) paste("-cntl_3", a$cntl_3),
    if (!is.null(a$cntl_6)) paste("-cntl_6", a$cntl_6),
    if (!is.null(a$nsbbdblocks)) paste("-nsbbdblocks", as.integer(a$nsbbdblocks)),
    if (!is.null(a$withmc66)) paste("-withmc66", as01(a$withmc66)),
    if (!is.null(a$smllthreads)) paste("-smllthreads", as.integer(a$smllthreads)),
    if (!is.null(a$tempdir)) paste("-tempdir", a$tempdir),
    if (!is.null(a$nowrites)) paste("-nowrites", as01(a$nowrites)),
    if (!is.null(a$condest)) paste("-condest", as01(a$condest)),
    if (!is.null(a$ma48u)) paste("-ma48u", format(a$ma48u, digits = 15))
  )
  if (is.null(flags)) {
    return(NULL)
  }
  paste(flags, collapse = " ")
}

#' MA48 workspace (la*) initial guesses: a user-passed value wins;
#' otherwise warm-start from the previous run's recorded `la_used`
#' (sol.stats.json under the same fixed convention as ems_compose);
#' otherwise the package cold defaults. Solver images with the
#' grow-and-retry paths treat these as starting sizes only, so an
#' undershot guess costs a redone analyse, not a failed run.
#'
#' @importFrom jsonlite read_json
#'
#' @keywords internal
#' @noRd
.resolve_la_args <- function(laA, laD, laDi, cmf_path) {
  cold <- list(laA = 300L, laD = 200L, laDi = 500L)
  used <- NULL
  stats_path <- file.path(
    dirname(cmf_path), "out", "variables", "bin", "sol.stats.json"
  )
  if (file.exists(stats_path)) {
    stats <- tryCatch(
      jsonlite::read_json(stats_path, simplifyVector = TRUE),
      error = function(e) NULL
    )
    # a structural probe run (ems_probe(), pre_probe, the auto method's
    # probe) writes the same stats.json but never factorizes: its
    # la_used is the launch default, not a measurement -- ignore it
    if (!identical(stats$solution_method, "probe")) {
      used <- stats$la_used
    }
  }
  pick <- function(user, nm) {
    if (!is.null(user)) {
      return(as.integer(user))
    }
    w <- used[[nm]]
    if (!is.null(w) && is.numeric(w) && length(w) == 1L && !is.na(w) && w > 0) {
      return(as.integer(w))
    }
    cold[[nm]]
  }
  list(
    laA = pick(laA, "laA"),
    laD = pick(laD, "laD"),
    laDi = pick(laDi, "laDi")
  )
}

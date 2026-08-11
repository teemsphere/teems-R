#' @title Solve a model with a Runge-Kutta method
#' @export
#' @description A Runge-Kutta front end for [`ems_solve()`]: the same
#'   solver run restricted to the Runge-Kutta `solution_method`
#'   flavors, with the step-control arguments that only exist for
#'   them (`adaptive`, `eps_tolerance`, `max_retries`,
#'   `retry_adjust`) and RK-tuned defaults — `"DoPri54"` with a
#'   single step and adaptive control enabled. Everything else
#'   (`matrix_method`, `n_tasks`, `precision`, ...) is forwarded to
#'   [`ems_solve()`] unchanged, which performs all validation and
#'   remains fully capable of Runge-Kutta runs itself.
#' @param solution_method The Runge-Kutta flavor, one of
#'   `"DoPri54"` (default), `"BoSha32"`, `"RK4"` or `"RK2"`. The
#'   embedded pairs (`"DoPri54"`, `"BoSha32"`) carry the per-step
#'   error estimate that `adaptive` control acts on.
#' @param steps Integer length 1 (default `4L`), the initial number
#'   of steps. Runge-Kutta methods use no Richardson extrapolation,
#'   so no step-count triple is involved; under `adaptive` control
#'   this is the initial step count only.
#' @param adaptive Character length 1, adaptive step-size control
#'   for the embedded Runge-Kutta methods (`"BoSha32"`,
#'   `"DoPri54"`). Default `NULL` resolves to `"yes"` for the
#'   embedded pairs and `"no"` for `"RK2"`/`"RK4"` (which provide no
#'   error estimate). Choices:
#'   * `"no"`: Fixed steps.
#'   * `"yes"`: After each step the worst per-component error
#'   metric is compared against `eps_tolerance`; failing steps are
#'   redone with a smaller step size and passing steps adjust the
#'   next step size (at most halving or doubling it). A step on
#'   which a percentage-change variable crosses `-100%` is also
#'   retried at a reduced step size.
#'   * `"accuracy-only"`: As `"yes"`, but only the error metric is
#'   acted on; check failures are ignored.
#' @param eps_tolerance Numeric length 1 (default is `0.1`), the
#'   per-step error-metric bound targeted by `adaptive` control.
#'   `0.1` suffices for most simulations; use `0.01` for more
#'   accurate solutions. Values below `0.005` are hard to achieve
#'   numerically. Ignored when `adaptive = "no"`.
#' @param max_retries Integer length 1 (default `NULL`, solver
#'   default `3L`), `adaptive = "yes"` only: how many times a step
#'   failing the -100% crossing check is retried at reduced length
#'   before the run aborts.
#' @param retry_adjust Numeric length 1 in (0, 1) (default `NULL`,
#'   solver default `0.5`), adaptive control only: the step-length
#'   multiplier applied on each retry.
#' @param ... All other [`ems_solve()`] arguments
#'   (`matrix_method`, `n_tasks`, `precision`, `terminal_run`, ...),
#'   forwarded unchanged.
#' @inheritParams ems_solve
#' @return As [`ems_solve()`].
#' @seealso [`ems_solve()`] for the general interface and the full
#'   argument documentation.
#' @references Schiffmann, F. (2022), "Runge Kutta integrators for
#'   fast and accurate solutions in GEMPACK".
#' @examples
#' \dontrun{
#' # The following examples require the teems solver to be built.
#' # See https://teemsphere.github.io/ to get started.
#'
#' # Adaptive DoPri54 (the defaults):
#' ems_RK(cmf_path)
#'
#' # Fixed-step RK4:
#' ems_RK(cmf_path, solution_method = "RK4", steps = 8L)
#' }
ems_RK <- function(cmf_path,
                   solution_method = c("DoPri54", "BoSha32", "RK4", "RK2"),
                   steps = 4L,
                   adaptive = NULL,
                   eps_tolerance = 0.1,
                   max_retries = NULL,
                   retry_adjust = NULL,
                   ...) {
  if (missing(cmf_path)) {
    .cli_missing(cmf_path)
  }
  solution_method <- rlang::arg_match(solution_method)
  if (is.null(adaptive)) {
    adaptive <- if (solution_method %in% c("BoSha32", "DoPri54")) {
      "yes"
    } else {
      "no"
    }
  }
  return(ems_solve(
    cmf_path = cmf_path,
    solution_method = solution_method,
    steps = steps,
    adaptive = adaptive,
    eps_tolerance = eps_tolerance,
    max_retries = max_retries,
    retry_adjust = retry_adjust,
    ...
  ))
}

# Condensation advice (teems-solver ROADMAP 6.2, benchmark round
# 2026-07-17). Condensation is an LU-era lever: it pays on static models
# solved by plain LU (-41% wall at 1.35M equations) and is
# counterproductive under the bordered methods at every elimination share
# measured, because substitution destroys the intra-block sparsity
# SBBD/DBBD/NDBBD exploit while the eliminated rows are re-evaluated per
# step by backsolve recovery anyway. Omission does not densify anything,
# so only backsolves trigger the advice.

#' @keywords internal
#' @noRd
.deploy_metadata <- function(cmf_path) {
  metadata_path <- file.path(dirname(cmf_path), "metadata.rds")
  if (!file.exists(metadata_path)) {
    return(NULL)
  }
  readRDS(metadata_path)
}

#' @keywords internal
#' @noRd
.condense_share <- function(condense) {
  paste0(
    format(round(100 * condense$elimination_share, 1), trim = TRUE),
    "%"
  )
}

# Probe-informed condensation advice (ROADMAP 6.2 follow-up, via the
# 6.10 probe pathway). The solve-time advisory above knows only whether a
# deployment is condensed; the probe additionally measures the block
# structure a bordered method would exploit, which is what actually
# decides the question. A partition of more than one diagonal block means
# a bordered method applies and substitution works against it; no usable
# partition means the run is LU-bound, where condensation is the measured
# lever -- and worth suggesting once the system is large enough for the
# gain to show (-41% wall at 1.35M equations; nothing at 202k).
.condense_lu_size <- 1e6

#' @keywords internal
#' @noRd
.probe_condense <- function(stats,
                            cmf_path) {
  if (is.null(stats)) {
    return(NULL)
  }
  metadata <- if (is.null(cmf_path)) NULL else .deploy_metadata(cmf_path)
  nominated <- metadata$condense

  vecsize <- stats$vecsize %|||% NA_integer_
  n_backsolve_ele <- stats$nbselems %|||% 0
  n_blocks <- stats$ndblock %|||% 0
  border <- stats$netcut
  uncondensed <- vecsize + n_backsolve_ele

  condensed <- n_backsolve_ele > 0
  # the probe runs under -matsol 0, so "bordered" here is the detected
  # structure, not the method the run happened to use
  partitioned <- isTRUE(stats$bordered) && n_blocks > 1

  verdict <- "none"
  if (condensed && partitioned) {
    verdict <- "hurts"
  } else if (condensed && !partitioned) {
    verdict <- "helps"
  } else if (!condensed && !partitioned && !is.na(vecsize) &&
    vecsize >= .condense_lu_size) {
    verdict <- "candidate"
  }

  list(
    condensed = condensed,
    n_backsolve = stats$nbacksolve %|||% (nominated$n_backsolve %|||% 0L),
    n_backsolve_ele = n_backsolve_ele,
    n_omit = nominated$n_omit %|||% NA_integer_,
    elimination_share = if (isTRUE(uncondensed > 0)) {
      n_backsolve_ele / uncondensed
    } else {
      0
    },
    partitioned = partitioned,
    n_blocks = n_blocks,
    partition_set = stats$partition_set,
    chain_set = stats$chain_set,
    border = border,
    border_share = if (is.null(border) || is.na(vecsize) || vecsize <= 0) {
      NA_real_
    } else {
      border / vecsize
    },
    verdict = verdict
  )
}

#' @description Render the probe's condensation verdict. Kept beside the
#'   solve-time advisory so both sides of the 6.2 guidance read together.
#' @importFrom cli cli_text
#' @keywords internal
#' @noRd
.probe_print_condense <- function(condense) {
  if (is.null(condense) || condense$verdict %=% "none") {
    return(invisible(NULL))
  }
  share <- .condense_share(condense)
  blocks <- condense$n_blocks
  set <- condense$partition_set %|||% (condense$chain_set %|||% "-")
  border <- condense$border
  n_backsolve <- condense$n_backsolve
  switch(condense$verdict,
    "hurts" = {
      cli::cli_text(
        "condensation: {n_backsolve} backsolved variable{?s} ({share} of
        the uncondensed system), but the probe finds a {blocks}-block
        partition on {.val {set}} (border {border})"
      )
      cli::cli_text(
        "  substitution densifies those blocks -- redeploy without
        {.arg backsolve} and solve with a bordered method"
      )
    },
    "helps" = {
      cli::cli_text(
        "condensation: {n_backsolve} backsolved variable{?s} ({share} of
        the uncondensed system); no usable block partition, so this
        system is {.val LU}-bound -- the case condensation pays for"
      )
    },
    "candidate" = {
      cli::cli_text(
        "condensation: none, and no usable block partition -- this
        {.val LU}-bound system is a candidate for
        {.fn ems_model} {.arg backsolve}"
      )
    }
  )
  invisible(NULL)
}

#' @keywords internal
#' @noRd
.advise_condense <- function(metadata,
                             matrix_method,
                             enable_time,
                             call) {
  condense <- metadata$condense
  if (is.null(condense) || (condense$n_backsolve %|||% 0L) < 1L) {
    return(invisible(NULL))
  }
  n_backsolve <- condense$n_backsolve
  share <- .condense_share(condense)

  if (enable_time) {
    .cli_action(solve_info$condense_intertemporal,
      action = rep("inform", 3),
      call = call
    )
  } else if (matrix_method %in% c("SBBD", "DBBD", "NDBBD")) {
    .cli_action(solve_info$condense_bordered,
      action = rep("inform", 3),
      call = call
    )
  }
  invisible(NULL)
}

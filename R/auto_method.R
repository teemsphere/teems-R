#' Structure-informed `matrix_method = "auto"` (ROADMAP 6.10)
#'
#' The decision reads the deployed system's MEASURED structure -- the
#' solver's structural probe (`-solmed probe`) -- rather than deploy
#' metadata alone: the chain dimension the equations actually couple
#' through lead/lag offsets, the diagonal-block partition candidates
#' (`partition_auto`: blocks, border, block balance per set), and the
#' border sizes of the chosen partition. Deploy metadata supplies the
#' system size (identical to the probe's `vecsize`: both count the
#' condensed endogenous system) and gates whether the probe runs at
#' all, so small solves never pay for it.
#'
#' Every threshold is a named placeholder in `.auto_thresholds()`,
#' reported in the auto message and written to model_diagnostics.txt,
#' so recalibration on the HPC matrix (teems-solver
#' docs/hpc_auto_plan.md section 3(c)) is a constants change, not a
#' plumbing change. The values are today's laptop fits (teems-solver
#' docs/solver-reference.md section 9, 2026-07 sweeps, <= 4 tasks):
#' - intertemporal: SBBD was fastest on every shape tested (T/R up to
#'   13.7, 24k-4.36M equations), including single-task runs; no
#'   NDBBD-over-SBBD region exists at <= 4 tasks, so the escalation
#'   is plumbed but dormant (`ndbbd_n_tasks = Inf`) until the matrix
#'   measures one.
#' - static: LU wins small systems; DBBD (4 tasks) overtakes LU at
#'   ~2.9M equations with 33 blocks and already at ~1.6M with 163.
#'   Thresholds sit on the LU side of the measured crossovers; the
#'   1.5M-2M x <100-block band falls to LU. Border shares on the
#'   winning rigs were tiny (0.1-2% of the system); the ceiling
#'   below is a guard, not a fitted crossover.
#'
#' @keywords internal
#' @noRd
.auto_thresholds <- function() {
  list(
    # static: below this size LU is a foregone conclusion and the
    # probe is skipped (probe cost curve = matrix deliverable)
    probe_min_size = 1.5e6,
    # static DBBD needs n_tasks >= 2 and either of these size gates
    dbbd_size = 2e6,
    dbbd_size_many_blocks = 1.5e6,
    dbbd_n_blocks = 100L,
    # ceiling on max(border variables, border equations) / system size
    # for a bordered method to be chosen from probe evidence
    border_share_max = 0.10,
    # intertemporal SBBD -> NDBBD escalation: minimum n_tasks (Inf =
    # dormant: no measured NDBBD-over-SBBD region yet)
    ndbbd_n_tasks = Inf
  )
}

#' @description Resolve `matrix_method = "auto"`. Returns a list:
#'   `method` (the chosen method), `decision` (the evidence record
#'   rendered in the message and model_diagnostics.txt) and `probe`
#'   (the `teems_probe` object when a probe ran, else `NULL`; the
#'   caller reuses it for the `pre_probe` verdict so one probe run
#'   serves both). `pre_probe = TRUE` forces the probe, so its
#'   structure feeds the decision at any size.
#' @keywords internal
#' @noRd
.resolve_auto_method <- function(enable_time,
                                 n_tasks,
                                 cmf_path,
                                 pre_probe = FALSE,
                                 timeID = NULL,
                                 call = NULL) {
  th <- .auto_thresholds()
  metadata <- .deploy_metadata(cmf_path = cmf_path)
  system_size <- metadata$system_size
  n_tasks <- as.integer(n_tasks)

  size_known <- !is.null(system_size)
  large_enough <- !size_known || system_size >= th$probe_min_size
  probe_reason <- if (isTRUE(pre_probe)) {
    "pre_probe"
  } else if (!enable_time && n_tasks >= 2L && large_enough) {
    "static candidate"
  } else if (enable_time && n_tasks >= th$ndbbd_n_tasks && large_enough) {
    "intertemporal candidate"
  } else {
    NULL
  }

  probe <- NULL
  structure <- NULL
  if (!is.null(probe_reason)) {
    .cli_action(solve_info$auto_probe,
      action = "inform",
      call = call
    )
    probe <- .run_probe(
      cmf_path = cmf_path,
      timeID = timeID %|||% format(x = Sys.time(), "%H%M"),
      call = call
    )
    structure <- probe$structure
    if (!size_known && !is.null(probe$vecsize)) {
      system_size <- probe$vecsize
    }
  }

  d <- .auto_decide(
    enable_time = enable_time,
    n_tasks = n_tasks,
    system_size = system_size,
    n_reg = metadata$n_reg,
    structure = structure,
    th = th
  )
  d$probe_reason <- probe_reason

  chosen <- d$method
  model_type <- d$model_type
  .cli_action(solve_info$auto_method,
    action = "inform",
    call = call
  )
  if (!is.null(probe)) {
    evidence <- .auto_evidence(d)
    .cli_action(solve_info$auto_evidence,
      action = "inform",
      call = call
    )
  }
  if (isTRUE(d$dbbd_hint)) {
    .cli_action(solve_info$auto_dbbd_hint,
      action = "inform",
      call = call
    )
  }
  if (isTRUE(d$no_chain)) {
    .cli_action(solve_info$auto_no_chain,
      action = "inform",
      call = call
    )
  }
  list(method = chosen, decision = d, probe = probe)
}

#' @description Pure decision rule over the evidence (no I/O), unit
#'   tested on synthetic `structure` lists. `structure` is
#'   `probe$structure` (`.probe_stats()` output) or `NULL` when no
#'   probe ran; then the metadata-only rule applies.
#' @keywords internal
#' @noRd
.auto_decide <- function(enable_time,
                         n_tasks,
                         system_size,
                         n_reg = NULL,
                         structure = NULL,
                         th = .auto_thresholds()) {
  probed <- !is.null(structure)
  part <- .auto_partition(structure, n_tasks = n_tasks)
  chain <- if (probed) {
    identical(structure$chain_source, "structural")
  } else {
    NA
  }
  size <- system_size %|||% NA_real_

  d <- list(
    model_type = if (enable_time) "intertemporal" else "static",
    method = NA_character_,
    source = if (probed) "probe" else if (!is.na(size)) "metadata" else "none",
    n_tasks = n_tasks,
    system_size = size,
    chain = chain,
    chain_set = structure$chain_set,
    n_time = structure$ntime,
    chain_border = if (isTRUE(chain)) structure$netcut else NULL,
    partition = part,
    thresholds = th,
    probed = probed,
    dbbd_hint = FALSE,
    no_chain = FALSE
  )

  if (enable_time && !isFALSE(chain)) {
    d$method <- "SBBD"
    if (isTRUE(chain) && !is.null(part) && n_tasks >= th$ndbbd_n_tasks &&
      part$n_blocks >= n_tasks && part$border_share <= th$border_share_max) {
      d$method <- "NDBBD"
    }
    return(d)
  }
  if (enable_time && isFALSE(chain)) {
    # declared intertemporal, but no equation couples set elements
    # through lead/lag offsets: the chain methods would abort in the
    # solver; the static family applies
    d$no_chain <- TRUE
  }

  d$method <- "LU"
  if (probed) {
    if (is.null(part) || is.na(size)) {
      return(d)
    }
    n_blocks <- part$n_blocks
    dbbd_favorable <- size >= th$dbbd_size ||
      (size >= th$dbbd_size_many_blocks && n_blocks >= th$dbbd_n_blocks)
    if (dbbd_favorable && part$border_share <= th$border_share_max) {
      if (n_tasks >= 2L && n_blocks >= n_tasks) {
        d$method <- "DBBD"
      } else if (n_tasks < 2L) {
        d$dbbd_hint <- TRUE
      }
    }
    return(d)
  }
  # metadata-only rule (no probe ran)
  if (!is.na(size)) {
    n_reg <- n_reg %|||% 0L
    dbbd_favorable <- size >= th$dbbd_size ||
      (size >= th$dbbd_size_many_blocks && n_reg >= th$dbbd_n_blocks)
    if (dbbd_favorable && n_tasks < 2L) {
      d$dbbd_hint <- TRUE
    }
  }
  d
}

#' @description The partition the solver would apply at `n_tasks`,
#'   replayed from the probe's candidate table with the solver's own
#'   rule (viable + at least `n_tasks` blocks; smallest border; near
#'   ties within 2% broken by block balance). `border_share` is the
#'   larger of border variables (netcut) and border equations
#'   (`border_neq`, known for the probe's chosen set only) over the
#'   system size. `NULL` when no candidate qualifies.
#' @keywords internal
#' @noRd
.auto_partition <- function(structure,
                            n_tasks) {
  cand <- structure$partition_auto
  if (is.null(cand) || !NROW(cand)) {
    return(NULL)
  }
  cand <- as.data.frame(cand)
  ok <- cand[cand$viable %in% TRUE & cand$nblocks >= n_tasks, , drop = FALSE]
  if (!NROW(ok)) {
    return(NULL)
  }
  best_cut <- min(ok$netcut)
  ok <- ok[50 * ok$netcut <= 51 * best_cut, , drop = FALSE]
  balance <- ok$block_min / pmax(ok$block_max, 1)
  pick <- ok[which.max(balance), , drop = FALSE]
  if (NROW(pick) > 1L) pick <- pick[1L, , drop = FALSE]
  vecsize <- structure$vecsize %|||% NA_real_
  border_neq <- if (identical(pick$set, structure$partition_set)) {
    structure$border_neq %|||% NA_real_
  } else {
    NA_real_
  }
  border <- max(pick$netcut, border_neq, na.rm = TRUE)
  list(
    set = pick$set,
    n_blocks = as.integer(pick$nblocks),
    netcut = as.integer(pick$netcut),
    border_neq = if (is.na(border_neq)) NA_integer_ else as.integer(border_neq),
    block_min = as.integer(pick$block_min),
    block_max = as.integer(pick$block_max),
    border_share = if (is.na(vecsize) || vecsize <= 0) NA_real_ else border / vecsize
  )
}

#' @description One-line evidence string for the auto message.
#' @keywords internal
#' @noRd
.auto_evidence <- function(d) {
  fmt <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
  pct <- function(x) paste0(format(round(100 * x, 1), nsmall = 1, trim = TRUE), "%")
  size <- if (is.na(d$system_size)) "unknown size" else paste(fmt(d$system_size), "equations")
  if (!isTRUE(d$probed)) {
    return(paste0(
      size, ", n_tasks ", d$n_tasks,
      "; structural probe skipped (",
      if (!identical(d$model_type, "intertemporal")) {
        paste0("below ", fmt(d$thresholds$probe_min_size), " equations or single task")
      } else {
        "SBBD escalation dormant"
      },
      ")"
    ))
  }
  chain <- if (isTRUE(d$chain)) {
    paste0("chain ", d$chain_set, " (", d$n_time, " blocks)")
  } else {
    "no chain"
  }
  part <- if (is.null(d$partition)) {
    paste0("no partition viable for ", d$n_tasks, " task(s)")
  } else {
    p <- d$partition
    paste0(
      "partition ", p$set, " (", p$n_blocks, " blocks, border ",
      if (is.na(p$border_share)) "n/a" else pct(p$border_share), ")"
    )
  }
  paste0(size, ", ", chain, ", ", part, ", n_tasks ", d$n_tasks)
}

#' @description Lines for the model_diagnostics.txt solve record.
#' @keywords internal
#' @noRd
.auto_record_lines <- function(d) {
  if (is.null(d)) {
    return(NULL)
  }
  th <- d$thresholds
  c(
    sprintf(
      "Matrix method auto: %s (%s: %s)", d$method,
      if (isTRUE(d$probed)) "structural probe" else "deploy metadata",
      .auto_evidence(d)
    ),
    sprintf(
      "  thresholds: probe_min_size %s, dbbd_size %s, dbbd_size_many_blocks %s, dbbd_n_blocks %s, border_share_max %s, ndbbd_n_tasks %s",
      format(th$probe_min_size, scientific = FALSE),
      format(th$dbbd_size, scientific = FALSE),
      format(th$dbbd_size_many_blocks, scientific = FALSE),
      th$dbbd_n_blocks, th$border_share_max, th$ndbbd_n_tasks
    )
  )
}

#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble as_tibble
#' @importFrom data.table rbindlist
#'
#' @keywords internal
#' @noRd
.probe_object <- function(probe_path,
                          stats_path = NULL,
                          diag_out = NULL,
                          cmf_path = NULL,
                          call = NULL) {
  if (!file.exists(probe_path)) {
    probe_path <- normalizePath(probe_path, "/", mustWork = FALSE)
    .cli_action(probe_err$no_report,
      action = "abort",
      call = call
    )
  }
  # simplifyMatrix = FALSE keeps per-statement dims as character
  # vectors even when every statement happens to share a dimension count
  probe <- jsonlite::fromJSON(probe_path, simplifyMatrix = FALSE)

  stats <- NULL
  if (!is.null(stats_path) && file.exists(stats_path)) {
    stats <- jsonlite::fromJSON(stats_path)
  }

  statements <- .probe_statements(probe$statements)
  incidence <- .probe_incidence(probe$statements)

  out <- structure(
    list(
      valid = !isTRUE(probe$defective),
      version = probe$version,
      vecsize = probe$vecsize,
      structural = .probe_pattern(probe$structural, probe$vecsize),
      realized = .probe_pattern(probe$realized, probe$vecsize),
      defects = .probe_defects(probe),
      statements = statements,
      incidence = incidence,
      cores = .probe_cores(probe$fine),
      structure = .probe_stats(stats),
      condense = .probe_condense(
        stats = .probe_stats(stats),
        cmf_path = cmf_path
      ),
      paths = list(
        report = normalizePath(probe_path, "/", mustWork = FALSE),
        stats = if (is.null(stats_path)) NULL else normalizePath(stats_path, "/", mustWork = FALSE),
        log = diag_out,
        cmf = cmf_path
      )
    ),
    class = "teems_probe"
  )
  return(out)
}

#' @keywords internal
#' @noRd
.probe_pattern <- function(p,
                           n) {
  if (is.null(p)) {
    return(NULL)
  }
  list(
    flag = p$flag,
    entries = p$entries %|||% NA_integer_,
    n = n,
    rank = p$rank %|||% NA_integer_,
    unmatched_rows = p$unmatched_rows %|||% NA_integer_,
    unmatched_cols = p$unmatched_cols %|||% NA_integer_,
    defective = isTRUE(p$defective),
    under_determined = .probe_element_tbl(p$under_determined_vars),
    over_constrained = .probe_element_tbl(p$over_constrained_eqs),
    under_by_var = .probe_agg_tbl(p$under_determined_by_var),
    over_by_eq = .probe_agg_tbl(p$over_constrained_by_eq),
    dm = p$dm,
    dm_under_by_var = .probe_agg_tbl(p$dm_under_by_var),
    dm_over_by_eq = .probe_agg_tbl(p$dm_over_by_eq)
  )
}

#' @description Parse solver-side element labels ("qo(svcs,reu)") into
#'   name plus element tuple. String split only: the names originate
#'   solver-side, so no TAB interpretation happens here.
#' @keywords internal
#' @noRd
.probe_element_tbl <- function(x) {
  if (is.null(x) || !length(x)) {
    return(tibble::tibble(
      element = character(),
      name = character(),
      elements = list()
    ))
  }
  name <- sub("\\(.*$", "", x)
  inner <- ifelse(grepl("(", x, fixed = TRUE),
    sub("^[^(]*\\(", "", sub("\\)$", "", x)),
    NA_character_
  )
  elements <- lapply(inner, function(i) {
    if (is.na(i)) character() else strsplit(i, ",", fixed = TRUE)[[1]]
  })
  tibble::tibble(
    element = x,
    name = name,
    elements = elements
  )
}

#' @keywords internal
#' @noRd
.probe_agg_tbl <- function(x) {
  if (is.null(x) || !NROW(x)) {
    return(tibble::tibble(name = character(), count = integer()))
  }
  tibble::tibble(name = x$n, count = as.integer(x$c))
}

#' @keywords internal
#' @noRd
.probe_statements <- function(s) {
  if (is.null(s) || !NROW(s)) {
    return(tibble::tibble(
      eq = character(),
      rows = integer(),
      dims = list(),
      vars = list()
    ))
  }
  tibble::tibble(
    eq = s$eq,
    rows = as.integer(s$rows),
    dims = s$dims,
    vars = lapply(s$vars, function(v) {
      if (is.null(v) || !NROW(v)) {
        tibble::tibble(var = character(), weight = integer())
      } else {
        tibble::tibble(var = v$v, weight = as.integer(v$w))
      }
    })
  )
}

#' @description Statement-level equation-system incidence in long form:
#'   one row per (equation statement, referenced variable) with the
#'   element-level incidence weight.
#' @keywords internal
#' @noRd
.probe_incidence <- function(s) {
  if (is.null(s) || !NROW(s)) {
    return(tibble::tibble(
      eq = character(),
      var = character(),
      weight = integer(),
      rows = integer()
    ))
  }
  pieces <- lapply(seq_len(NROW(s)), function(i) {
    v <- s$vars[[i]]
    if (is.null(v) || !NROW(v)) {
      return(NULL)
    }
    data.table::data.table(
      eq = s$eq[[i]],
      var = v$v,
      weight = as.integer(v$w),
      rows = as.integer(s$rows[[i]])
    )
  })
  tibble::as_tibble(data.table::rbindlist(pieces))
}

#' @keywords internal
#' @noRd
.probe_cores <- function(fine) {
  if (is.null(fine)) {
    return(NULL)
  }
  sizes <- if (is.null(fine$core_sizes) || !NROW(fine$core_sizes)) {
    tibble::tibble(size = integer(), count = integer())
  } else {
    tibble::tibble(
      size = as.integer(fine$core_sizes$size),
      count = as.integer(fine$core_sizes$count)
    )
  }
  top <- fine$top_cores
  top_tbl <- if (is.null(top) || !NROW(top)) {
    tibble::tibble(core = integer(), size = integer(), eqs = list(), vars = list())
  } else {
    tibble::tibble(
      core = seq_len(NROW(top)),
      size = as.integer(top$size),
      eqs = lapply(top$eqs, .probe_agg_tbl),
      vars = lapply(top$vars, .probe_agg_tbl)
    )
  }
  list(
    sq_comps = fine$sq_comps %|||% NA_integer_,
    cores_gt1 = fine$cores_gt1 %|||% NA_integer_,
    largest = fine$largest_core %|||% NA_integer_,
    sizes = sizes,
    top = top_tbl
  )
}

#' @keywords internal
#' @noRd
.probe_defects <- function(probe) {
  pieces <- list()
  for (pattern in c("structural", "realized")) {
    p <- probe[[pattern]]
    if (is.null(p)) next
    under <- .probe_element_tbl(p$under_determined_vars)
    over <- .probe_element_tbl(p$over_constrained_eqs)
    if (NROW(under)) {
      under$pattern <- pattern
      under$side <- "under_determined_variable"
      pieces <- c(pieces, list(under))
    }
    if (NROW(over)) {
      over$pattern <- pattern
      over$side <- "over_constrained_equation"
      pieces <- c(pieces, list(over))
    }
  }
  if (!length(pieces)) {
    return(tibble::tibble(
      pattern = character(),
      side = character(),
      element = character(),
      name = character(),
      elements = list()
    ))
  }
  out <- tibble::as_tibble(data.table::rbindlist(pieces))
  out[, c("pattern", "side", "element", "name", "elements")]
}

#' @keywords internal
#' @noRd
.probe_stats <- function(stats) {
  if (is.null(stats)) {
    return(NULL)
  }
  keep <- c(
    "version", "vecsize", "nvarele", "nexo", "nbacksolve", "nbselems",
    "matrix_method", "solution_method", "mpi_size", "bordered",
    "chain_source", "partition_source", "chain_set", "partition_set",
    "ntime", "nreg", "ndblock", "netcut", "nintraeq", "border_neq"
  )
  out <- stats[intersect(keep, names(stats))]
  if (!is.null(stats$partition_auto) && NROW(stats$partition_auto)) {
    out$partition_auto <- tibble::as_tibble(stats$partition_auto)
  }
  return(out)
}

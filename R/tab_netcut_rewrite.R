#' Netcut enforcement, stage E2 (roadmap 6.5). The solver places every
#' element of a variable referenced with a lead or lag into the dense
#' border of the bordered matrix methods, so an inter-period link on an
#' element slice of a large variable (e.g. qo("capital",r,t+1)) borders
#' the whole variable. When a lead/lag reference fixes one or more
#' dimensions to quoted elements, the border contribution is reducible
#' mechanically: synthesize a proxy variable over the remaining
#' dimensions (NCV*), tie it to the slice with a linking equation
#' (E_NCV*), and move the lead/lag onto the proxy. References that run
#' over full sets are not reducible and are left to the .check_netcut
#' warning.
#'
#' @importFrom purrr map_chr map_lgl
#'
#' @keywords internal
#' @noRd
.rewrite_tab_netcut <- function(tab,
                                call) {
  int_sets <- regmatches(tab, regexec(
    "^[Ss][Ee][Tt]\\s*\\(\\s*[Ii][Nn][Tt][Ee][Rr][Tt][Ee][Mm][Pp][Oo][Rr][Aa][Ll]\\s*\\)\\s*([A-Za-z_][A-Za-z0-9_]*)",
    tab
  ))
  int_sets <- toupper(purrr::map_chr(
    int_sets[lengths(int_sets) > 0L],
    2
  ))

  if (length(int_sets) %=% 0L) {
    return(tab)
  }

  vars <- .netcut_var_table(tab)

  if (nrow(vars) %=% 0L) {
    return(tab)
  }

  synth <- new.env(parent = emptyenv())
  synth$n <- 0L
  synth$tab <- tab
  synth$rewrites <- character(0)
  additions <- list()

  is_eq <- grepl("^[Ee][Qq][Uu][Aa][Tt][Ii][Oo][Nn][^A-Za-z0-9_]", tab)

  for (s in which(is_eq)) {
    refs <- .netcut_lagged_refs(tab[s], vars)
    if (nrow(refs) %=% 0L) {
      next
    }
    stmt <- tab[s]
    for (k in rev(seq_len(nrow(refs)))) {
      proxy <- .netcut_proxy(
        ref = refs[k, ],
        vars = vars,
        synth = synth
      )
      if (is.null(proxy)) {
        next
      }
      if (length(proxy$pre) > 0L) {
        at <- as.character(proxy$after)
        additions[[at]] <- c(additions[[at]], proxy$pre)
        synth$tab <- c(synth$tab, proxy$pre)
      }
      stmt <- paste0(
        substr(stmt, 1L, refs$start[k] - 1L),
        proxy$ref,
        substring(stmt, refs$end[k] + 1L)
      )
    }
    tab[s] <- stmt
  }

  if (length(additions) %=% 0L) {
    return(tab)
  }

  out <- vector("list", length(tab))
  for (s in seq_along(tab)) {
    out[[s]] <- c(tab[s], additions[[as.character(s)]])
  }

  proxy_summary <- synth$rewrites
  .cli_action(model_info$netcut_rewrite,
    action = c("inform", "inform"),
    call = call
  )

  unlist(out, use.names = FALSE)
}

#' Variable declarations as a table: name, statement index, per-dim
#' index letters and set names (in argument order), and the qualifier
#' groups to inherit on a synthesized proxy.
#'
#' @keywords internal
#' @noRd
.netcut_var_table <- function(tab) {
  is_var <- grepl("^[Vv][Aa][Rr][Ii][Aa][Bb][Ll][Ee][^A-Za-z0-9_]", tab)
  rows <- lapply(which(is_var), function(s) {
    rest <- trimws(sub("^[Vv][Aa][Rr][Ii][Aa][Bb][Ll][Ee]\\s*", "", tab[s]))
    quals <- character(0)
    quants <- list()
    repeat {
      rest <- sub("^\\s+", "", rest)
      if (!startsWith(rest, "(")) break
      close <- .match_bracket(rest, 1L)
      if (is.na(close)) break
      grp <- substr(rest, 1L, close)
      m <- regmatches(grp, regexec(
        "^\\(\\s*[Aa][Ll][Ll]\\s*,\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*,\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*\\)$",
        grp
      ))[[1]]
      if (length(m) > 0L) {
        quants[[length(quants) + 1L]] <- c(idx = m[2], set = m[3])
      } else {
        quals <- c(quals, grp)
      }
      rest <- substring(rest, close + 1L)
    }
    m <- regmatches(rest, regexec(
      "^([A-Za-z_][A-Za-z0-9_]*)\\s*(\\(([^()]*)\\))?",
      rest
    ))[[1]]
    if (length(m) %=% 0L || m[2] %=% "") {
      return(NULL)
    }
    args <- if (m[4] %=% "") character(0) else trimws(strsplit(m[4], ",")[[1]])
    q_idx <- purrr::map_chr(quants, "idx")
    q_set <- purrr::map_chr(quants, "set")
    at <- match(tolower(args), tolower(q_idx))
    if (anyNA(at)) {
      return(NULL)
    }
    list(
      name = tolower(m[2]),
      stmt = s,
      idx = args,
      sets = q_set[at],
      quals = paste(quals[!grepl("orig_level", quals, ignore.case = TRUE)],
        collapse = ""
      )
    )
  })
  rows <- rows[!purrr::map_lgl(rows, is.null)]
  tibble::tibble(
    name = purrr::map_chr(rows, "name"),
    stmt = vapply(rows, function(r) r$stmt, integer(1)),
    idx = lapply(rows, function(r) r$idx),
    sets = lapply(rows, function(r) r$sets),
    quals = purrr::map_chr(rows, "quals")
  )
}

#' Lead/lag variable references within one equation statement that fix
#' at least one dimension to a quoted element. Returns a table with the
#' character span of each reference and its parsed arguments.
#'
#' @keywords internal
#' @noRd
.netcut_lagged_refs <- function(stmt, vars) {
  hits <- gregexpr("([A-Za-z_][A-Za-z0-9_]*)\\s*\\(([^()]*)\\)", stmt)[[1]]
  out <- list(name = character(0), start = integer(0), end = integer(0), args = list())
  if (hits[1] %=% -1L) {
    return(tibble::as_tibble(out))
  }
  for (i in seq_along(hits)) {
    start <- hits[i]
    end <- start + attr(hits, "match.length")[i] - 1L
    ref <- substr(stmt, start, end)
    nm <- tolower(sub("\\s*\\(.*$", "", ref))
    if (!nm %in% vars$name) {
      next
    }
    args <- trimws(strsplit(sub("^[^(]*\\(", "", sub("\\)$", "", ref)), ",")[[1]])
    has_offset <- any(grepl("^[A-Za-z_][A-Za-z0-9_]*\\s*[+-]\\s*[0-9]+$", args))
    has_elem <- any(grepl("^\"[^\"]*\"$", args))
    if (!has_offset || !has_elem) {
      next
    }
    if (length(args) %!=% length(vars$idx[[match(nm, vars$name)]])) {
      next
    }
    out$name <- c(out$name, nm)
    out$start <- c(out$start, start)
    out$end <- c(out$end, end)
    out$args <- c(out$args, list(args))
  }
  tibble::as_tibble(out)
}

#' The proxy (cached by variable and fixed-element pattern) for one
#' lead/lag reference: list(pre, after, ref) — the statements to insert
#' (possibly none when cached), the statement index to insert after,
#' and the replacement reference text.
#'
#' @keywords internal
#' @noRd
.netcut_proxy <- function(ref,
                          vars,
                          synth) {
  v <- match(ref$name, vars$name)
  args <- ref$args[[1]]
  fixed <- grepl("^\"", args)
  key <- paste0(
    "NC|", toupper(ref$name), "|",
    paste0(which(fixed), "=", tolower(args[fixed]), collapse = "|")
  )
  cached <- synth[[key]]
  keep_args <- args[!fixed]
  if (!is.null(cached)) {
    return(list(
      pre = character(0),
      after = vars$stmt[v],
      ref = paste0(cached, "(", paste(keep_args, collapse = ","), ")")
    ))
  }
  nm <- .synth_proxy_name(synth)
  synth[[key]] <- nm
  keep_idx <- vars$idx[[v]][!fixed]
  keep_sets <- vars$sets[[v]][!fixed]
  quants <- paste0("(all,", keep_idx, ",", keep_sets, ")", collapse = "")
  src_args <- args
  src_args[!fixed] <- keep_idx
  src_args <- sub("\\s*[+-]\\s*[0-9]+$", "", src_args)
  src <- paste0(ref$name, "(", paste(src_args, collapse = ","), ")")
  synth$rewrites <- c(synth$rewrites, paste0(nm, " = ", src))
  pre <- c(
    sprintf(
      "Variable %s%s %s(%s) # netcut proxy for %s #",
      vars$quals[v], quants, nm, paste(keep_idx, collapse = ","), src
    ),
    sprintf(
      "Equation E_%s # netcut proxy link # %s %s(%s) = %s",
      nm, quants, nm, paste(keep_idx, collapse = ","), src
    )
  )
  list(
    pre = pre,
    after = vars$stmt[v],
    ref = paste0(nm, "(", paste(keep_args, collapse = ","), ")")
  )
}

#' A fresh proxy variable name unused anywhere in the model.
#'
#' @keywords internal
#' @noRd
.synth_proxy_name <- function(synth) {
  repeat {
    synth$n <- synth$n + 1L
    nm <- paste0("NCV", synth$n)
    hit <- paste0("(^|[^A-Za-z0-9_])(E_)?", nm, "([^A-Za-z0-9_]|$)")
    if (!any(grepl(hit, synth$tab, ignore.case = TRUE))) {
      return(nm)
    }
  }
}

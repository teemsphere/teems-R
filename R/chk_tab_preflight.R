#' Pre-flight TAB validation against the solver's fatal invariants
#'
#' Statement-table checks mirroring the solver-side fatals inventoried
#' in dev/validation_table.md (names 11.2.1, qualifiers 10.3/10.4,
#' bounds 10.19.1, Default statements 10.19, Reads 10.6/11.11.8,
#' PostSim scope 12.2.1-12.2.3). Aborting here fails the model before
#' the Docker deploy round-trip; the solver remains authoritative for
#' everything expression- or data-dependent.
#'
#' @keywords internal
#' @noRd
.check_tab_preflight <- function(model,
                                 call) {
  .chk_tab_names(model, call = call)
  .chk_tab_reads(model, call = call)
  .chk_tab_postsim(model, call = call)
  return(invisible(NULL))
}

#' Raw-statement checks that must run before any object parsing
#'
#' Formula & Equation, Default statements, malformed qualifier lists,
#' and headerless/terminal Reads crash or corrupt the downstream
#' extract parsers, so they are diagnosed on the cleaned statement
#' vector straight after .check_statements().
#'
#' @keywords internal
#' @noRd
.chk_raw_statements <- function(statements,
                                call) {
  fe <- grepl("^\\s*formula\\s*&\\s*equation\\b", tolower(statements))
  if (any(fe)) {
    .cli_action(model_err$formula_equation,
      action = c("abort", "inform"),
      call = call
    )
  }
  ps_begin <- sum(grepl("^\\s*postsim\\s*\\(\\s*begin", statements, ignore.case = TRUE))
  ps_end <- sum(grepl("^\\s*postsim\\s*\\(\\s*end", statements, ignore.case = TRUE))
  if (ps_begin != ps_end) {
    .cli_action(model_err$postsim_unbalanced,
      action = "abort",
      call = call
    )
  }
  # Formula/Equation/Update need an "=": a statement whose leading
  # token is no recognized keyword is folded into the preceding
  # statement as an implicit continuation (.check_statements), so an
  # unknown keyword surfaces here as a math statement without "=" --
  # the raw form crashes the downstream extract parsers
  kw_stmt <- tolower(sub("^\\s*([A-Za-z_]+).*$", "\\1", statements))
  math_stmt <- kw_stmt %in% c("formula", "equation", "update")
  no_eq <- math_stmt & !grepl("=", gsub("#[^#]*#", "", statements), fixed = TRUE)
  if (any(no_eq)) {
    bad_stmt <- trimws(statements[no_eq][1])
    stmt_kw <- tools::toTitleCase(kw_stmt[no_eq][1])
    .cli_action(model_err$stmt_missing_equals,
      action = c("abort", "inform"),
      call = call
    )
  }
  .chk_tab_defaults(statements, call = call)
  .chk_tab_qualifiers(statements, call = call)
  .chk_raw_reads(statements, call = call)
  return(invisible(NULL))
}

#' @keywords internal
#' @noRd
.chk_raw_reads <- function(statements,
                           call) {
  reads <- grep("^\\s*read\\b", statements, ignore.case = TRUE, value = TRUE)
  if (length(reads) == 0L) {
    return(invisible(NULL))
  }
  # specific defects first: terminal source, partial (indexed) reads,
  # missing file clause; only then the generic header requirement
  term <- grepl("from\\s+terminal", reads, ignore.case = TRUE)
  if (any(term)) {
    bad_stmt <- trimws(reads[term][1])
    .cli_action(model_err$read_terminal,
      action = "abort",
      call = call
    )
  }
  if (any(grepl("\\(", reads))) {
    .cli_action(model_err$invalid_read,
      action = "abort",
      call = call
    )
  }
  if (!all(grepl("from\\s+file", reads, ignore.case = TRUE))) {
    .cli_action(model_err$missing_file,
      action = "abort",
      call = call
    )
  }
  nohdr <- !grepl("\\bheader\\b", reads, ignore.case = TRUE)
  if (any(nohdr)) {
    bad_reads <- trimws(reads[nohdr])
    .cli_action(model_err$read_no_header,
      action = "abort",
      call = call
    )
  }
  return(invisible(NULL))
}

#' @keywords internal
#' @noRd
.chk_tab_names <- function(model,
                           call) {
  typ <- tolower(model$type)
  decl_name <- function(kind) {
    n <- model$name[typ == kind]
    n <- n[!is.na(n) & grepl("^[A-Za-z]", n)]
    n
  }
  coef <- decl_name("coefficient")
  var <- decl_name("variable")
  set <- decl_name("set")

  coef_l <- tolower(coef)
  var_l <- tolower(var)
  set_l <- tolower(set)

  clash <- intersect(coef_l, var_l)
  if (length(clash) > 0L) {
    .cli_action(model_err$name_coef_var,
      action = c("abort", "inform"),
      call = call
    )
  }
  clash <- intersect(coef_l, set_l)
  if (length(clash) > 0L) {
    .cli_action(model_err$name_coef_set,
      action = c("abort", "inform"),
      call = call
    )
  }
  clash <- intersect(var_l, set_l)
  if (length(clash) > 0L) {
    .cli_action(model_err$name_var_set,
      action = c("abort", "inform"),
      call = call
    )
  }

  for (kind in c("coefficient", "variable")) {
    n <- if (kind == "coefficient") coef_l else var_l
    if (anyDuplicated(n) > 0L) {
      dup_names <- unique(n[duplicated(n)])
      dup_type <- kind
      .cli_action(model_err$name_dup,
        action = "abort",
        call = call
      )
    }
  }

  res_names <- unique(c(coef_l, var_l, set_l)[c(coef_l, var_l, set_l) %in% tab_reserved_words])
  if (length(res_names) > 0L) {
    .cli_action(model_err$name_reserved,
      action = "abort",
      call = call
    )
  }

  bad_names <- coef[grepl("^c_", coef, ignore.case = TRUE)]
  if (length(bad_names) > 0L) {
    .cli_action(model_err$name_c_prefix,
      action = "abort",
      call = call
    )
  }

  # variables whose second character is "_" collide with a coefficient
  # named after their suffix (the solver generates p_/c_ linear names)
  pre <- var_l[substr(var_l, 2, 2) == "_"]
  base <- substring(pre, 3)
  hit <- base %in% coef_l
  if (any(hit)) {
    clash <- paste0(base[hit], "/", pre[hit])
    .cli_action(model_err$name_prefix_clash,
      action = c("abort", "inform"),
      call = call
    )
  }

  max_len <- 255L
  all_names <- c(coef, var, set)
  long_names <- unique(all_names[nchar(all_names) > max_len])
  if (length(long_names) > 0L) {
    long_names <- paste0(substr(long_names, 1, 20), "...")
    .cli_action(model_err$name_too_long,
      action = "abort",
      call = call
    )
  }
  return(invisible(NULL))
}

#' Leading parenthetical qualifier groups of a declaration statement
#'
#' Mirrors the solver's tab_qualifiers_parse region rule: qualifier
#' groups are the leading (...) groups; the first (all,...) quantifier
#' ends the region.
#'
#' @keywords internal
#' @noRd
.tab_qualifier_groups <- function(text) {
  text <- gsub("#[^#]*#", "", text)
  rest <- sub("^\\s*[A-Za-z_]+", "", text)
  groups <- character(0)
  unbalanced <- FALSE
  repeat {
    if (!grepl("^\\s*\\(", rest)) break
    if (grepl("^\\s*\\(\\s*all[ ,]", rest, ignore.case = TRUE)) break
    m <- regmatches(rest, regexec("^\\s*\\(([^)]*)\\)", rest))[[1]]
    if (length(m) == 0L) {
      unbalanced <- TRUE
      break
    }
    groups <- c(groups, m[2])
    rest <- sub("^\\s*\\([^)]*\\)", "", rest)
  }
  list(groups = groups, unbalanced = unbalanced)
}

#' @keywords internal
#' @noRd
.chk_tab_qualifiers <- function(statements,
                                call) {
  kw <- tolower(sub("^\\s*([A-Za-z_]+).*$", "\\1", statements))
  rows <- which(kw %in% c("variable", "coefficient"))
  bad_quals <- character(0)
  for (i in rows) {
    is_variable <- kw[i] == "variable"
    vocab <- if (is_variable) tab_var_qualifiers else tab_coef_qualifiers
    prefixes <- if (is_variable) tab_var_qualifier_prefixes else character(0)
    parsed <- .tab_qualifier_groups(statements[i])
    bad_stmt <- trimws(statements[i])
    if (parsed$unbalanced) {
      .cli_action(model_err$qual_unbalanced,
        action = "abort",
        call = call
      )
    }
    n_lower <- 0L
    n_upper <- 0L
    for (g in parsed$groups) {
      toks <- tolower(gsub("[[:space:]]", "", strsplit(g, ",", fixed = TRUE)[[1]]))
      if (length(toks) == 0L) toks <- ""
      if (any(grepl("^default", toks))) next
      for (tok in toks) {
        if (!nzchar(tok)) {
          .cli_action(model_err$qual_empty,
            action = "abort",
            call = call
          )
        }
        if (is_variable && tok == "no_split") {
          .cli_action(model_err$qual_no_split,
            action = "abort",
            call = call
          )
        }
        if (is_variable && grepl("^linear_(name|var)=", tok)) {
          .cli_action(model_err$qual_linear_name,
            action = "abort",
            call = call
          )
        }
        if (grepl("^(ge|gt|le|lt)[-+0-9.]", tok)) {
          if (grepl("^g", tok)) {
            n_lower <- n_lower + 1L
          } else {
            n_upper <- n_upper + 1L
          }
          next
        }
        if (tok %in% vocab) next
        if (length(prefixes) > 0L &&
          any(startsWith(tok, prefixes))) {
          next
        }
        bad_quals <- c(bad_quals, tok)
      }
    }
    if (n_lower > 1L || n_upper > 1L) {
      bound_dir <- if (n_lower > 1L) "lower" else "upper"
      .cli_action(model_err$bound_dup,
        action = c("abort", "inform"),
        call = call
      )
    }
  }
  if (length(bad_quals) > 0L) {
    bad_quals <- unique(bad_quals)
    .cli_action(model_err$qual_unknown,
      action = c("abort", "inform"),
      call = call
    )
  }
  return(invisible(NULL))
}

#' @keywords internal
#' @noRd
.chk_tab_defaults <- function(statements,
                              call) {
  idx <- grep("\\(\\s*default", statements, ignore.case = TRUE)
  for (i in idx) {
    stmt <- statements[i]
    bad_stmt <- trimws(stmt)
    kw <- tolower(sub("^\\s*([A-Za-z_]+).*$", "\\1", stmt))
    bad_val <- sub(".*?\\(\\s*default\\s*=?\\s*([^);]*).*$", "\\1", stmt, ignore.case = TRUE)
    bad_val <- tolower(gsub("[[:space:]]", "", bad_val))
    if (!kw %in% names(tab_default_values)) {
      .cli_action(model_err$default_keyword,
        action = "abort",
        call = call
      )
    }
    if (bad_val %in% tab_default_values[[kw]]) {
      # valid for the solver, but the teems pipeline cannot carry the
      # positional Default semantics through deploy yet
      .cli_action(model_err$default_unsupported,
        action = c("abort", "inform"),
        call = call
      )
    }
    if (kw == "coefficient" && grepl("^(lower|upper)_bound", bad_val)) {
      .cli_action(model_err$default_bound,
        action = "abort",
        call = call
      )
    }
    if (kw == "equation" && bad_val == "levels") {
      .cli_action(model_err$default_levels,
        action = "abort",
        call = call
      )
    }
    if (kw == "equation" && grepl("^add_homotopy", bad_val)) {
      .cli_action(model_err$default_homotopy,
        action = "abort",
        call = call
      )
    }
    default_kw <- tools::toTitleCase(kw)
    .cli_action(model_err$default_unknown,
      action = "abort",
      call = call
    )
  }
  return(invisible(NULL))
}

#' @keywords internal
#' @noRd
.chk_tab_reads <- function(model,
                           call) {
  typ <- tolower(model$type)
  reads <- which(typ == "read")
  if (length(reads) == 0L) {
    return(invisible(NULL))
  }
  declared <- tolower(model$name[typ %in% c("coefficient", "variable")])
  # PostSim reads get their own target diagnosis (.chk_tab_postsim)
  ord_reads <- reads[!model$postsim[reads]]
  tgt <- tolower(model$name[ord_reads])
  undecl <- !is.na(tgt) & nzchar(tgt) & !tgt %in% declared
  if (any(undecl)) {
    bad_targets <- unique(tgt[undecl])
    .cli_action(model_err$read_undeclared,
      action = "abort",
      call = call
    )
  }
  return(invisible(NULL))
}

#' PostSim Formula LHS identifier (leading quantifier groups stripped)
#'
#' @keywords internal
#' @noRd
.formula_lhs_name <- function(comp1) {
  x <- tolower(comp1)
  x <- gsub("^\\s*(\\([^)]*\\)\\s*)*", "", x)
  sub("^([a-z][a-z0-9_]*).*$", "\\1", x)
}

#' @keywords internal
#' @noRd
.chk_tab_postsim <- function(model,
                             call) {
  if (!any(model$postsim)) {
    return(invisible(NULL))
  }
  ps <- model$postsim
  typ <- tolower(model$type)
  ps_names <- tolower(model$name[ps & typ %in% c("coefficient", "set", "subset", "file")])
  ps_names <- unique(ps_names[!is.na(ps_names) & nzchar(ps_names)])
  coef_ord <- tolower(model$name[!ps & typ == "coefficient"])
  coef_ps <- tolower(model$name[ps & typ == "coefficient"])
  var_names <- tolower(model$name[typ == "variable"])

  # scope isolation (12.2.1): ordinary executables must not reference
  # PostSim-declared names
  if (length(ps_names) > 0L) {
    exec_ord <- which(!ps & typ %in% c("formula", "equation", "update", "assertion", "read"))
    scan <- tolower(gsub("#[^#]*#", "", model$tab[exec_ord]))
    hits <- vapply(
      ps_names,
      function(n) any(grepl(paste0("\\b", n, "\\b"), scan)),
      logical(1)
    )
    if (any(hits)) {
      bad_refs <- ps_names[hits]
      .cli_action(model_err$postsim_scope,
        action = c("abort", "inform"),
        call = call
      )
    }
  }

  # same-file rule (12.2.3)
  rd <- typ == "read"
  files_ord <- unique(tolower(model$file[rd & !ps]))
  files_ps <- unique(tolower(model$file[rd & ps]))
  bad_files <- setdiff(intersect(files_ord, files_ps), NA)
  if (length(bad_files) > 0L) {
    .cli_action(model_err$postsim_same_file,
      action = c("abort", "inform"),
      call = call
    )
  }

  # PostSim Read targets (12.2.3)
  tgt <- tolower(model$name[rd & ps])
  tgt <- tgt[!is.na(tgt) & nzchar(tgt)]
  bad_targets <- unique(tgt[tgt %in% var_names])
  if (length(bad_targets) > 0L) {
    .cli_action(model_err$postsim_read_var,
      action = "abort",
      call = call
    )
  }
  bad_targets <- unique(tgt[tgt %in% coef_ord & !tgt %in% coef_ps])
  if (length(bad_targets) > 0L) {
    .cli_action(model_err$postsim_read_ord,
      action = "abort",
      call = call
    )
  }
  bad_targets <- unique(tgt[!tgt %in% c(coef_ord, coef_ps)])
  if (length(bad_targets) > 0L) {
    .cli_action(model_err$postsim_read_undecl,
      action = "abort",
      call = call
    )
  }

  # PostSim Formula LHS (12.2.2)
  psf <- which(ps & typ == "formula" & !is.na(model$comp1))
  if (length(psf) > 0L) {
    lhs <- .formula_lhs_name(model$comp1[psf])
    bad_lhs <- unique(lhs[lhs %in% var_names])
    if (length(bad_lhs) > 0L) {
      .cli_action(model_err$postsim_lhs_var,
        action = "abort",
        call = call
      )
    }
    bad_lhs <- unique(lhs[lhs %in% coef_ord & !lhs %in% coef_ps])
    if (length(bad_lhs) > 0L) {
      .cli_action(model_err$postsim_lhs_ord,
        action = "abort",
        call = call
      )
    }
  }
  return(invisible(NULL))
}

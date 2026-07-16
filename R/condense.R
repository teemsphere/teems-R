# Condensation engine (GEMPACK manual 10.16 / 14.1.10):
# variable omission and backsolving at the TAB level. Backsolved
# variables are symbolically substituted out of every other equation;
# the nominated (defining) equation is retained and a Backsolve
# statement is emitted at write-out so the solver can recover values
# post-solve. Substitutions are applied to previously retained defining
# equations too, so every retained equation references surviving
# variables only.

#' @importFrom purrr map_chr map_lgl
#'
#' @keywords internal
#' @noRd
.condense_model <- function(tab,
                            omit,
                            backsolve,
                            ignore_condense,
                            quiet,
                            call) {
  none <- list(
    tab = tab,
    flags = NULL,
    n_omit = 0L,
    n_backsolve = 0L
  )

  intab <- .parse_intab_condense(tab)

  if (length(intab$rows) > 0L) {
    tab <- tab[-intab$rows]
    if (ignore_condense) {
      n_ignored <- length(intab$rows)
      if (!quiet) {
        .cli_action(model_info$condense_ignored,
          action = "inform",
          call = call
        )
      }
      intab$actions <- list()
    } else {
      sub_var <- purrr::map_chr(
        intab$actions[purrr::map_lgl(intab$actions, "substitute")],
        "var"
      )
      if (length(sub_var) > 0L && !quiet) {
        .cli_action(model_info$substitute_as_backsolve,
          action = c("inform", "inform"),
          call = call
        )
      }
    }
  }

  intab_omit <- purrr::map_chr(
    intab$actions[purrr::map_chr(intab$actions, "action") == "omit"],
    "var"
  )
  intab_backsolve <- intab$actions[
    purrr::map_chr(intab$actions, "action") == "backsolve"
  ]

  if (length(intab_omit) == 0L && length(intab_backsolve) == 0L &&
    is.null(omit) && is.null(backsolve)) {
    none$tab <- tab
    return(none)
  }

  extract <- .generate_extracts(tab = tab, call = call)
  var_extract <- .parse_tab_obj(
    extract = extract$model,
    obj_type = "variable",
    call = call
  )
  coeff_extract <- .parse_tab_obj(
    extract = extract$model,
    obj_type = "coefficient",
    call = call
  )
  math_extract <- .parse_tab_maths(
    extract = extract$model,
    call = call
  )
  eq_names <- math_extract$name[math_extract$type %in% "Equation"]

  omit_vars <- unique(.canonical_vars(
    input = c(intab_omit, omit),
    var_extract = var_extract,
    err = model_err$invalid_omit,
    call = call
  ))

  pairs <- .resolve_backsolves(
    intab_backsolve = intab_backsolve,
    backsolve = backsolve,
    var_extract = var_extract,
    eq_names = eq_names,
    call = call
  )
  pairs <- pairs[!duplicated(purrr::map_chr(pairs, function(p) {
    paste(tolower(p$var), tolower(p$eq))
  }))]

  all_vars <- c(omit_vars, purrr::map_chr(pairs, "var"))
  if (anyDuplicated(tolower(all_vars))) {
    conflict_var <- unique(all_vars[duplicated(tolower(all_vars))])
    .cli_action(model_err$condense_conflict,
      action = "abort",
      call = call
    )
  }

  if (length(pairs) > 0L) {
    all_eqs <- purrr::map_chr(pairs, "eq")
    if (anyDuplicated(tolower(all_eqs))) {
      reused_eq <- unique(all_eqs[duplicated(tolower(all_eqs))])
      .cli_action(model_err$condense_eq_reused,
        action = "abort",
        call = call
      )
    }
  }

  statement_type <- tolower(purrr::map_chr(strsplit(tab, " "), 1))

  for (var in omit_vars) {
    has_args <- var_extract$ls_upper_idx[[var]] %!=% NA
    tab <- .zero_var_refs(
      var = var,
      tab = tab,
      statement_type = statement_type,
      has_args = has_args
    )
  }

  flags <- data.frame(
    name = omit_vars,
    type = rep("Variable", length(omit_vars)),
    condense = rep("omit", length(omit_vars)),
    condense_eq = rep(NA_character_, length(omit_vars))
  )

  if (length(pairs) > 0L) {
    tab <- .backsolve_all(
      tab = tab,
      pairs = pairs,
      var_extract = var_extract,
      coeff_extract = coeff_extract,
      eq_names = eq_names,
      call = call
    )

    flags <- rbind(
      flags,
      data.frame(
        name = purrr::map_chr(pairs, "var"),
        type = "Variable",
        condense = "backsolve",
        condense_eq = purrr::map_chr(pairs, "eq")
      ),
      data.frame(
        name = purrr::map_chr(pairs, "eq"),
        type = "Equation",
        condense = "backsolve",
        condense_eq = purrr::map_chr(pairs, "eq")
      )
    )
  }

  list(
    tab = tab,
    flags = flags,
    n_omit = length(omit_vars),
    n_backsolve = length(pairs)
  )
}

# Deploy-time guard: swaps and shocks must not touch condensed variables.
#' @keywords internal
#' @noRd
.abort_condensed <- function(var_name,
                             var_extract,
                             err,
                             call) {
  r_idx <- match(tolower(var_name), tolower(var_extract$name))
  if (is.na(r_idx)) {
    return(invisible(NULL))
  }
  condense_action <- var_extract$condense[[r_idx]]
  if (!is.na(condense_action)) {
    .cli_action(err,
      action = c("abort", "inform"),
      call = call
    )
  }
  invisible(NULL)
}

# In-TAB OMIT / SUBSTITUTE / BACKSOLVE statements (GEMPACK manual 10.16).
#' @keywords internal
#' @noRd
.parse_intab_condense <- function(tab) {
  first <- tolower(purrr::map_chr(strsplit(trimws(tab), "\\s+"), 1))
  rows <- which(first %in% c("omit", "substitute", "backsolve"))
  actions <- list()

  for (r in rows) {
    words <- strsplit(trimws(tab[[r]]), "\\s+")[[1]]
    kind <- tolower(words[[1]])
    if (kind %=% "omit") {
      for (v in words[-1]) {
        actions[[length(actions) + 1L]] <- list(
          action = "omit",
          var = v,
          eq = NA_character_,
          substitute = FALSE
        )
      }
    } else {
      using_at <- which(tolower(words) == "using")
      var <- words[[2]]
      eq <- if (length(using_at) == 1L && using_at == 3L && length(words) >= 4L) {
        words[[4]]
      } else {
        NA_character_
      }
      actions[[length(actions) + 1L]] <- list(
        action = "backsolve",
        var = var,
        eq = eq,
        substitute = kind %=% "substitute"
      )
    }
  }

  list(rows = rows, actions = actions)
}

# Case-insensitive canonicalization of user/TAB variable names.
#' @keywords internal
#' @noRd
.canonical_vars <- function(input,
                            var_extract,
                            err,
                            call) {
  if (length(input) == 0L) {
    return(character())
  }
  r_idx <- match(tolower(input), tolower(var_extract$name))
  if (anyNA(r_idx)) {
    invalid_var <- input[is.na(r_idx)]
    .cli_action(err,
      action = "abort",
      call = call
    )
  }
  var_extract$name[r_idx]
}

# Backsolve nominations: in-TAB pairs first (TAB order), then the
# `backsolve` argument. Unnamed argument entries resolve their defining
# equation by the E_<variable> convention; named entries give it
# explicitly (GEMPACK manual 10.16).
#' @keywords internal
#' @noRd
.resolve_backsolves <- function(intab_backsolve,
                                backsolve,
                                var_extract,
                                eq_names,
                                call) {
  pairs <- list()

  arg_actions <- list()
  if (!is.null(backsolve)) {
    arg_names <- names(backsolve) %|||% rep("", length(backsolve))
    for (b in seq_along(backsolve)) {
      if (nzchar(arg_names[[b]])) {
        arg_actions[[length(arg_actions) + 1L]] <- list(
          var = arg_names[[b]],
          eq = backsolve[[b]]
        )
      } else {
        arg_actions[[length(arg_actions) + 1L]] <- list(
          var = backsolve[[b]],
          eq = NA_character_
        )
      }
    }
  }

  for (action in c(intab_backsolve, arg_actions)) {
    bs_var <- .canonical_vars(
      input = action$var,
      var_extract = var_extract,
      err = model_err$invalid_backsolve_var,
      call = call
    )

    if (is.na(action$eq)) {
      conv_eq <- paste0("E_", bs_var)
      e_idx <- match(tolower(conv_eq), tolower(eq_names))
      if (is.na(e_idx)) {
        .cli_action(model_err$backsolve_unresolvable,
          action = c("abort", "inform", "inform"),
          call = call
        )
      }
    } else {
      e_idx <- match(tolower(action$eq), tolower(eq_names))
      if (is.na(e_idx)) {
        invalid_eq <- action$eq
        .cli_action(model_err$invalid_backsolve_eq,
          action = "abort",
          call = call
        )
      }
    }

    pairs[[length(pairs) + 1L]] <- list(var = bs_var, eq = eq_names[[e_idx]])
  }

  pairs
}

# Omission: references become 0 in Equation and Update statements; the
# declaration statement is retained (write-out filters on the tibble
# condense flag).
#' @keywords internal
#' @noRd
.zero_var_refs <- function(var,
                           tab,
                           statement_type,
                           has_args) {
  target <- statement_type %in% c("equation", "update")
  if (has_args) {
    pattern <- paste0("(?<![[:alnum:]_])", var, "\\([^\\)]*\\)")
  } else {
    pattern <- paste0("(?<![[:alnum:]_])", var, "(?![[:alnum:]_(])")
  }
  tab[target] <- gsub(pattern, "0", tab[target], perl = TRUE, ignore.case = TRUE)
  return(tab)
}

# ---------------------------------------------------------------------
# Backsolve engine
# ---------------------------------------------------------------------

#' @keywords internal
#' @noRd
.backsolve_all <- function(tab,
                           pairs,
                           var_extract,
                           coeff_extract,
                           eq_names,
                           call) {
  # re-extract: omission zeroing above edited equation texts
  extract <- .generate_extracts(tab = tab, call = call)
  math_extract <- .parse_tab_maths(
    extract = extract$model,
    call = call
  )
  math_extract <- math_extract[math_extract$type %in% "Equation", ]

  var_lookup <- as.list(stats::setNames(
    var_extract$name,
    tolower(var_extract$name)
  ))

  taken <- tolower(c(
    var_extract$name, coeff_extract$name, math_extract$name,
    extract$set$name
  ))
  csub <- new.env(parent = emptyenv())
  csub$counter <- 0L
  csub$taken <- taken
  csub$cache <- list()
  csub$statements <- character()

  eqs <- new.env(parent = emptyenv())

  for (pair in pairs) {
    def <- .eq_entry(
      eq_name = pair$eq,
      eqs = eqs,
      math_extract = math_extract,
      tab = tab,
      var_lookup = var_lookup,
      call = call
    )

    occ_check <- .check_backsolve_rules(
      var_name = pair$var,
      entry = def,
      decl_sets = var_extract$ls_upper_idx[[pair$var]],
      call = call
    )

    solution <- .rearrange_defining(
      entry = def,
      var_name = pair$var,
      def_args = occ_check$args,
      csub = csub,
      call = call
    )

    # substitute into every other equation currently referencing the var
    ref_pattern <- paste0("(?<![[:alnum:]_])", pair$var, "(?![[:alnum:]_])")
    for (e in seq_len(nrow(math_extract))) {
      eq_name <- math_extract$name[[e]]
      if (tolower(eq_name) %=% tolower(pair$eq)) {
        next
      }
      loaded <- !is.null(eqs[[tolower(eq_name)]])
      if (!loaded &&
        !grepl(ref_pattern, math_extract$definition[[e]],
          perl = TRUE, ignore.case = TRUE
        )) {
        next
      }
      entry <- .eq_entry(
        eq_name = eq_name,
        eqs = eqs,
        math_extract = math_extract,
        tab = tab,
        var_lookup = var_lookup,
        call = call
      )
      .substitute_into_eq(
        entry_name = tolower(eq_name),
        eqs = eqs,
        var_name = pair$var,
        def_args = occ_check$args,
        solution = solution,
        csub = csub
      )
    }
  }

  for (nm in ls(eqs)) {
    entry <- eqs[[nm]]
    if (!entry$dirty) {
      next
    }
    tab[[entry$row_id]] <- .serialize_eq_statement(entry)
  }

  c(tab, csub$statements)
}

# Load (or fetch) the flattened form of an equation.
#' @keywords internal
#' @noRd
.eq_entry <- function(eq_name,
                      eqs,
                      math_extract,
                      tab,
                      var_lookup,
                      call) {
  key <- tolower(eq_name)
  if (!is.null(eqs[[key]])) {
    return(eqs[[key]])
  }

  r_idx <- match(key, tolower(math_extract$name))
  row <- math_extract[r_idx, ]
  statement <- tab[[row$row_id]]

  quants <- .parse_eq_quants(
    eq_name = eq_name,
    statement = statement,
    call = call
  )

  sides <- lapply(c(row$comp1, row$comp2), function(comp) {
    if (is.na(comp)) {
      return(list())
    }
    tryCatch(
      .prune_zero_terms(.parse_linear_side(comp, var_lookup)),
      error = function(e) {
        parse_reason <- conditionMessage(e)
        .cli_action(model_err$condense_parse,
          action = c("abort", "inform"),
          call = call
        )
      }
    )
  })

  entry <- list(
    name = row$name,
    label = row$label,
    qualifier_list = row$qualifier_list,
    quants = quants,
    lhs = sides[[1]],
    rhs = sides[[2]],
    dirty = FALSE,
    row_id = row$row_id
  )
  eqs[[key]] <- entry
  entry
}

#' @keywords internal
#' @noRd
.prune_zero_terms <- function(terms) {
  terms[!purrr::map_lgl(terms, function(t) {
    any(t$fac == "0" & t$ops == "*")
  })]
}

#' @keywords internal
#' @noRd
.parse_eq_quants <- function(eq_name,
                             statement,
                             call) {
  # quantifiers precede the expression and `(all,` cannot occur inside
  # one; labels are stripped so their free text cannot alias a quantifier
  bare <- gsub("#[^#]*#", "", statement)
  bare <- gsub("\\s", "", bare)
  m <- gregexpr("\\(all,([[:alnum:]_]+),([[:alnum:]_]+)\\)", bare)
  hits <- regmatches(bare, m)[[1]]
  n_all <- length(gregexpr("(all,", bare, fixed = TRUE)[[1]])
  if (identical(gregexpr("(all,", bare, fixed = TRUE)[[1]][1], -1L)) {
    n_all <- 0L
  }
  if (n_all != length(hits)) {
    parse_reason <- "unsupported equation quantifier form"
    .cli_action(model_err$condense_parse,
      action = c("abort", "inform"),
      call = call
    )
  }
  lapply(hits, function(h) {
    parts <- strsplit(gsub("[()]", "", h), ",")[[1]]
    list(idx = parts[[2]], set = parts[[3]])
  })
}

# GEMPACK manual 14.1.10: the eight requirements for a substitution
# (or backsolve) to be possible. Returns the common argument pattern.
#' @keywords internal
#' @noRd
.check_backsolve_rules <- function(var_name,
                                   entry,
                                   decl_sets,
                                   call) {
  eq_name <- entry$name
  all_terms <- c(entry$lhs, entry$rhs)
  occ_at <- which(purrr::map_lgl(all_terms, function(t) {
    !is.null(t$var) && t$var$name %=% var_name
  }))

  if (length(occ_at) == 0L) {
    rule_text <- paste0(
      "The variable does not occur in the equation."
    )
    .cli_action(model_err$condense_rule,
      action = c("abort", "inform", "inform"),
      call = call
    )
  }

  quant_idx <- purrr::map_chr(entry$quants, "idx")
  quant_set <- purrr::map_chr(entry$quants, "set")
  if (length(decl_sets) == 1L && is.na(decl_sets)) {
    decl_sets <- character()
  }

  fail <- function(rule_text) {
    .cli_action(model_err$condense_rule,
      action = c("abort", "inform", "inform"),
      call = call
    )
  }

  for (o in occ_at) {
    t <- all_terms[[o]]
    args <- t$var$args

    if (any(grepl("^\"", args))) {
      fail(paste0(
        "Occurrence ", .serialize_term(t), ": an element occurs as an ",
        "argument; every argument must be an index (requirement 1)."
      ))
    }

    if (any(grepl("[+-]", args))) {
      fail(paste0(
        "Occurrence ", .serialize_term(t), ": an argument carries a ",
        "lead/lag offset; offsets block substitution in intertemporal ",
        "models (requirement 6)."
      ))
    }

    sum_idx <- purrr::map_chr(t$quants, "idx")
    if (any(args %in% sum_idx)) {
      fail(paste0(
        "Occurrence ", .serialize_term(t), ": a SUM index occurs as an ",
        "argument; every index must be an equation ALL index ",
        "(requirement 2)."
      ))
    }

    if (!all(quant_idx %in% args)) {
      missing_idx <- setdiff(quant_idx, args)
      fail(paste0(
        "Equation ALL index (", paste(missing_idx, collapse = ","),
        ") absent from occurrence ", .serialize_term(t),
        "; every equation ALL index must appear in each occurrence ",
        "(requirement 3)."
      ))
    }

    if (anyDuplicated(args)) {
      fail(paste0(
        "Occurrence ", .serialize_term(t), ": a repeated index; all ",
        "indices of one occurrence must be different (requirement 5)."
      ))
    }

    if (!all(args %in% quant_idx)) {
      fail(paste0(
        "Occurrence ", .serialize_term(t), ": an argument is not bound ",
        "by an equation ALL quantifier (requirement 2)."
      ))
    }

    arg_sets <- quant_set[match(args, quant_idx)]
    if (length(arg_sets) != length(decl_sets) ||
      !all(tolower(arg_sets) == tolower(decl_sets))) {
      fail(paste0(
        "Occurrence ", .serialize_term(t), " ranges over {",
        paste(arg_sets, collapse = ","), "} but the variable is declared ",
        "over {", paste(decl_sets, collapse = ","), "}; every index must ",
        "range over the full declared set (requirement 4)."
      ))
    }
  }

  first_args <- all_terms[[occ_at[[1]]]]$var$args
  for (o in occ_at[-1]) {
    if (!identical(all_terms[[o]]$var$args, first_args)) {
      fail(paste0(
        "Occurrences ", .serialize_term(all_terms[[occ_at[[1]]]]), " and ",
        .serialize_term(all_terms[[o]]), " have different index patterns; ",
        "all occurrences must share one pattern (requirement 7)."
      ))
    }
  }

  list(args = first_args)
}

# Rearrange the defining equation into x = <solution terms>
# (GEMPACK manual 14.1.1): combine the x terms, divide through, negate.
#' @keywords internal
#' @noRd
.rearrange_defining <- function(entry,
                                var_name,
                                def_args,
                                csub,
                                call) {
  eq_name <- entry$name
  all_terms <- c(entry$lhs, .negate_terms(entry$rhs))
  is_x <- purrr::map_lgl(all_terms, function(t) {
    !is.null(t$var) && t$var$name %=% var_name
  })
  x_terms <- all_terms[is_x]
  others <- .negate_terms(all_terms[!is_x])

  pieces <- purrr::map_chr(x_terms, function(t) {
    body <- "1"
    if (length(t$fac) > 0L) {
      body <- t$fac[[1]]
      for (f in seq_along(t$fac)[-1]) {
        body <- paste0(body, t$ops[[f]], t$fac[[f]])
      }
    }
    for (q in rev(t$quants)) {
      body <- paste0("sum{", q$idx, ",", q$set, ", ", body, "}")
    }
    body
  })
  signs <- purrr::map_int(x_terms, "sign")

  if (all(pieces == "1")) {
    pivot_num <- sum(signs)
    if (pivot_num == 0L) {
      rule_text <- paste0(
        "The occurrences of the variable cancel; no expression for it ",
        "can be obtained from this equation."
      )
      .cli_action(model_err$condense_rule,
        action = c("abort", "inform", "inform"),
        call = call
      )
    }
    if (pivot_num < 0L) {
      others <- .negate_terms(others)
    }
    if (abs(pivot_num) != 1L) {
      others <- lapply(others, function(t) {
        t$fac <- c(t$fac, as.character(abs(pivot_num)))
        t$ops <- c(t$ops, "/")
        t
      })
    }
    solution <- others
  } else {
    pivot_expr <- ""
    for (p in seq_along(pieces)) {
      joint <- if (p == 1L) {
        ifelse(signs[[p]] == 1L, "", "-")
      } else {
        ifelse(signs[[p]] == 1L, " + ", " - ")
      }
      pivot_expr <- paste0(pivot_expr, joint, pieces[[p]])
    }

    binding <- .quant_binding(entry$quants)
    binding <- binding[intersect(names(binding), .expr_idents(pivot_expr))]
    pivot_ref <- .csub_new(
      expr = pivot_expr,
      binding = binding,
      label = paste0("backsolve pivot (", entry$name, ")"),
      csub = csub
    )

    .cli_action(model_wrn$condense_pivot_zero,
      action = c("warn", "inform"),
      call = call
    )

    solution <- lapply(others, function(t) {
      t$fac <- c(t$fac, pivot_ref)
      t$ops <- c(t$ops, "/")
      t
    })
  }

  # expression-swell control: hoist multi-factor coefficient products
  # into synthesized coefficients (GEMPACK manual 14.1.12)
  lapply(solution, function(t) {
    .hoist_term(
      term = t,
      binding = .quant_binding(c(entry$quants, t$quants)),
      label = paste0("backsolve product (", entry$name, ")"),
      csub = csub,
      min_fac = 2L
    )
  })
}

#' @keywords internal
#' @noRd
.quant_binding <- function(quants) {
  binding <- purrr::map_chr(quants, "set")
  names(binding) <- purrr::map_chr(quants, "idx")
  binding
}

# Replace a term's coefficient factor product with one synthesized
# coefficient when it has at least `min_fac` factors.
#' @keywords internal
#' @noRd
.hoist_term <- function(term,
                        binding,
                        label,
                        csub,
                        min_fac = 2L) {
  if (length(term$fac) < min_fac) {
    return(term)
  }
  if (all(grepl("^-?(?:[0-9.]|\")", term$fac))) {
    return(term)
  }
  expr <- term$fac[[1]]
  for (f in seq_along(term$fac)[-1]) {
    expr <- paste0(expr, term$ops[[f]], term$fac[[f]])
  }
  used <- intersect(names(binding), .expr_idents(expr))
  ref <- .csub_new(
    expr = expr,
    binding = binding[used],
    label = label,
    csub = csub
  )
  term$fac <- ref
  term$ops <- "*"
  term
}

# Synthesize (or reuse) a coefficient CSUB<n> with a defining formula.
# Statements are appended at the end of the TAB: only equations
# reference them, and equation evaluation is order-free.
#' @keywords internal
#' @noRd
.csub_new <- function(expr,
                      binding,
                      label,
                      csub) {
  dims <- names(binding)
  canon_map <- stats::setNames(paste0("d", seq_along(dims)), dims)
  key <- paste0(
    .rename_expr_tokens(expr, canon_map),
    "|",
    paste(unname(binding), collapse = ",")
  )

  cached <- csub$cache[[key]]
  if (!is.null(cached)) {
    return(.csub_ref(cached, dims))
  }

  repeat {
    csub$counter <- csub$counter + 1L
    name <- paste0("CSUB", csub$counter)
    if (!tolower(name) %in% csub$taken) {
      break
    }
  }

  quant_text <- paste0(
    "(all,", dims, ",", unname(binding), ")",
    collapse = ""
  )
  if (length(dims) == 0L) {
    quant_text <- ""
  }

  csub$statements <- c(
    csub$statements,
    paste0(
      "Coefficient ", quant_text, " ", .csub_ref(name, dims),
      " # ", label, " #"
    ),
    paste0(
      "Formula ", quant_text, " ", .csub_ref(name, dims),
      " = ", expr
    )
  )
  csub$cache[[key]] <- name
  .csub_ref(name, dims)
}

#' @keywords internal
#' @noRd
.csub_ref <- function(name, dims) {
  if (length(dims) == 0L) {
    return(name)
  }
  paste0(name, "(", paste(dims, collapse = ","), ")")
}

# Substitute the solution for `var_name` into one equation.
#' @keywords internal
#' @noRd
.substitute_into_eq <- function(entry_name,
                                eqs,
                                var_name,
                                def_args,
                                solution,
                                csub) {
  entry <- eqs[[entry_name]]

  touched <- FALSE
  used <- .eq_idents(entry)

  for (side in c("lhs", "rhs")) {
    new_side <- list()
    for (t in entry[[side]]) {
      if (is.null(t$var) || t$var$name %!=% var_name) {
        new_side <- c(new_side, list(t))
        next
      }
      touched <- TRUE
      expanded <- .expand_occurrence(
        t = t,
        def_args = def_args,
        solution = solution,
        entry = entry,
        used = used,
        csub = csub
      )
      new_side <- c(new_side, expanded$terms)
      used <- expanded$used
    }
    entry[[side]] <- new_side
  }

  if (touched) {
    entry$dirty <- TRUE
    eqs[[entry_name]] <- entry
  }
  invisible(entry)
}

#' @keywords internal
#' @noRd
.eq_idents <- function(entry) {
  idents <- purrr::map_chr(entry$quants, "idx")
  for (t in c(entry$lhs, entry$rhs)) {
    idents <- c(
      idents,
      purrr::map_chr(t$quants, "idx"),
      unlist(lapply(t$fac, .expr_idents)),
      if (!is.null(t$var)) {
        c(t$var$name, unlist(lapply(t$var$args, .expr_idents)))
      }
    )
  }
  unique(idents)
}

# One occurrence c*x(args) expands into the mapped solution terms.
# Sum indices of the solution are freshened against the target
# equation; sums whose index the surviving variable does not carry are
# inverted onto the coefficient product and hoisted (GEMPACK 14.1.12).
#' @keywords internal
#' @noRd
.expand_occurrence <- function(t,
                               def_args,
                               solution,
                               entry,
                               used,
                               csub) {
  base_map <- stats::setNames(t$var$args, def_args)
  base_map <- base_map[names(base_map) != base_map]
  out <- list()

  for (s in solution) {
    fresh_map <- character()
    for (q in s$quants) {
      if (q$idx %in% used || q$idx %in% t$var$args) {
        candidate_pool <- paste0(q$idx, seq_len(99L))
        fresh <- candidate_pool[!candidate_pool %in% used][[1]]
        fresh_map[[q$idx]] <- fresh
        used <- c(used, fresh)
      }
    }

    s2 <- .rename_term(s, c(base_map, fresh_map))

    new <- .new_term(
      sign = t$sign * s$sign,
      quants = c(t$quants, s2$quants),
      fac = c(t$fac, s2$fac),
      ops = c(t$ops, s2$ops),
      var = s2$var
    )

    new <- .invert_idle_sums(
      term = new,
      binding = .quant_binding(entry$quants),
      label = paste0("backsolve product (", entry$name, ")"),
      csub = csub
    )

    out <- c(out, list(new))
  }

  list(terms = out, used = used)
}

# Rewrite sum{u,S, c(u)*y(i)} as [sum{u,S, c(u)}]*y(i) when y does not
# carry u, then hoist the resulting coefficient sum.
#' @keywords internal
#' @noRd
.invert_idle_sums <- function(term,
                              binding,
                              label,
                              csub) {
  if (length(term$quants) == 0L || length(term$fac) == 0L) {
    return(.hoist_term(
      term = term,
      binding = c(binding, .quant_binding(term$quants)),
      label = label,
      csub = csub,
      min_fac = 3L
    ))
  }

  var_idents <- character()
  if (!is.null(term$var)) {
    var_idents <- unlist(lapply(term$var$args, .expr_idents))
  }

  idle <- purrr::map_lgl(term$quants, function(q) {
    !q$idx %in% var_idents
  })

  if (!any(idle)) {
    return(.hoist_term(
      term = term,
      binding = c(binding, .quant_binding(term$quants)),
      label = label,
      csub = csub,
      min_fac = 3L
    ))
  }

  expr <- term$fac[[1]]
  if (length(term$fac) > 1L) {
    for (f in seq_along(term$fac)[-1]) {
      expr <- paste0(expr, term$ops[[f]], term$fac[[f]])
    }
  }
  for (q in rev(term$quants[idle])) {
    expr <- paste0("sum{", q$idx, ",", q$set, ", ", expr, "}")
  }

  keep <- term$quants[!idle]
  full_binding <- c(binding, .quant_binding(keep))
  used <- intersect(names(full_binding), .expr_idents(expr))

  ref <- .csub_new(
    expr = expr,
    binding = full_binding[used],
    label = label,
    csub = csub
  )

  term$fac <- ref
  term$ops <- "*"
  term$quants <- keep
  term
}

#' @keywords internal
#' @noRd
.serialize_eq_statement <- function(entry) {
  quant_text <- paste0(
    purrr::map_chr(entry$quants, function(q) {
      paste0("(all,", q$idx, ",", q$set, ")")
    }),
    collapse = ""
  )

  parts <- c(
    "Equation",
    entry$name,
    if (!is.na(entry$label)) paste0("# ", entry$label, " #"),
    if (!is.na(entry$qualifier_list)) entry$qualifier_list,
    quant_text,
    paste(
      .serialize_linear(entry$lhs),
      "=",
      .serialize_linear(entry$rhs)
    )
  )
  paste(parts[nzchar(parts)], collapse = " ")
}

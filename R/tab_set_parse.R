#' @importFrom purrr map
#' @importFrom utils tail
#'
#' @keywords internal
#' @noRd
.parse_tab_sets <- function(extract,
                            call) {

  sets <- extract[tolower(extract$type) %in% "set",]
  sets$type <- "Set"
  sets$qualifier_list <- ifelse(
    substr(sets$remainder, 1, 1) == "(",
    .get_element(
      input = sets$remainder,
      split = " ",
      index = 1
    ),
    NA
  )

  sets$qualifier_list <- ifelse(is.na(sets$qualifier_list),
    "(non_intertemporal)",
    sets$qualifier_list
  )

  valid_qual <- c("(intertemporal)", "(non_intertemporal)")
  if (any(!tolower(sets$qualifier_list) %in% valid_qual)) {
    invalid_qual <- setdiff(tolower(sets$qualifier_list), valid_qual)
    .cli_action(model_err$invalid_set_qual,
      action = "abort",
      call = call
    )
  }

  sets$remainder <- .advance_remainder(
    remainder = sets$remainder,
    pattern = sets$qualifier_list
  )

  sets$name <- trimws(ifelse(grepl("#", sets$remainder),
    .get_element(
      input = sets$remainder,
      split = "#",
      index = 1
    ),
    ifelse(grepl("\\(", sets$remainder),
      .get_element(
        input = sets$remainder,
        split = "\\(",
        index = 1
      ),
      .get_element(
        input = sets$remainder,
        split = "=",
        index = 1
      )
    )
  ))
  
  sets$name <- ifelse(grepl("\\s", sets$name),
                      purrr::map_chr(strsplit(sets$name, "\\s"), 1),
                      sets$name)

  sets$remainder <- .advance_remainder(
    remainder = sets$remainder,
    pattern = sets$name
  )

  sets$label <- ifelse(grepl("#", sets$remainder),
    trimws(purrr::map(strsplit(sets$remainder, "#"), 2)),
    NA
  )

  sets$remainder <- .advance_remainder(
    remainder = sets$remainder,
    pattern = sets$label
  )

  sets$remainder <- trimws(x = gsub(
    pattern = "#",
    replacement = "",
    x = sets$remainder
  ))

  sets$max_size <- unlist(x = ifelse(
    test = grepl(
      pattern = "maximum size",
      x = sets$remainder,
      ignore.case = TRUE
    ),
    yes = paste("maximum size", purrr::map(.x = strsplit(
      x = sets$remainder, split = " "
    ), 3)),
    no = NA
  ))

  sets$remainder <- .advance_remainder(
    remainder = sets$remainder,
    pattern = sets$max_size
  )

  sets$full_read <- ifelse(
    test = grepl(pattern = "read elements from file", x = tolower(x = sets$remainder)),
    yes = sets$remainder,
    no = NA
  )

  sets$file <- ifelse(
    test = !is.na(x = sets$full_read),
    yes = unlist(x = purrr::map(
      .x = strsplit(x = toupper(x = sets$full_read), split = toupper(x = "from file")),
      .f = function(s) {
        trimws(x = purrr::map(strsplit(
          x = s[2], split = toupper("header")
        ), 1))
      }
    )),
    no = NA
  )

  sets$header <- ifelse(test = !is.na(x = sets$full_read),
    yes = unlist(x = purrr::map(
      .x = strsplit(x = toupper(x = sets$full_read), split = toupper(x = "from file")),
      .f = function(s) {
        trimws(x = purrr::map(strsplit(x = s[2], split = toupper("header")), 2))
      }
    )),
    no = NA
  )

  sets$header <- gsub(pattern = "\"", replacement = "", x = sets$header)

  # S10: the solver's header buffer holds HEADERSIZE (4) characters
  hdr_long <- !is.na(sets$header) & nchar(trimws(sets$header)) > 4L
  if (any(hdr_long)) {
    bad_set <- sets$name[hdr_long][1]
    bad_header <- trimws(sets$header[hdr_long][1])
    .cli_action(model_err$set_header_len,
      action = "abort",
      call = call
    )
  }

  sets$remainder <- .advance_remainder(
    remainder = sets$remainder,
    pattern = sets$full_read
  )

  sets$size <- ifelse(test = grepl(pattern = "size", x = tolower(x = sets$remainder)),
    yes = trimws(x = purrr::map(.x = strsplit(x = sets$remainder, split = "\\("), 1)),
    no = NA
  )

  sets$remainder <- .advance_remainder(
    remainder = sets$remainder,
    pattern = sets$size
  )

  sets$definition <- ifelse(test = sets$remainder != "",
    yes = sets$remainder,
    no = NA
  )

  # S5/S9 on raw explicit element lists (before the cleanup below can
  # silently swallow empty elements or fold a range into one "element")
  is_ele_list <- !is.na(sets$definition) &
    !grepl("^\\s*=", sets$definition) &
    tolower(sets$qualifier_list) != "(intertemporal)" &
    is.na(sets$full_read)
  for (i in which(is_ele_list)) {
    bad_set <- sets$name[i]
    inner <- sub("^\\s*\\(", "", sub("\\)\\s*$", "", trimws(sets$definition[i])))
    bad_def <- trimws(sets$definition[i])
    eles <- trimws(strsplit(inner, ",", fixed = TRUE)[[1]])
    ranged <- grepl("-", eles, fixed = TRUE)
    if (any(ranged)) {
      bad_ele <- eles[ranged][1]
      .cli_action(model_err$set_ele_range,
        action = c("abort", "inform"),
        call = call
      )
    }
    collapsed <- gsub("[[:space:]]", "", inner)
    empty <- !nzchar(collapsed) || grepl("^,|,,|,$", collapsed)
    malformed <- any(grepl("[[:space:]]", eles))
    if (empty || malformed) {
      empty_or_malformed <- if (empty) "empty" else "malformed"
      .cli_action(model_err$set_ele_list,
        action = "abort",
        call = call
      )
    }
  }

  sets$remainder <- .advance_remainder(
    remainder = sets$remainder,
    pattern = sets$definition
  )

  if (any(sets$remainder != "")) {
    .cli_action(model_err$set_parse_fail,
      action = "abort",
      call = call,
      .internal = TRUE
    )
  }

  # conditional set builders `Set X = (all,i,SRC: <cond>);` (manual
  # 10.1.2): the solver evaluates the data-dependent condition from
  # the deployed input files ahead of set resolution
  # (tab_setbuilder_transform); R mirrors it at deploy from the same
  # aggregated tables (.eval_set_builder) so closure/shock validation
  # and compose see the elements. Parsed here for shape only.
  is_builder <- !is.na(sets$definition) &
    grepl("^\\s*=\\s*\\(\\s*all\\s*,", sets$definition, ignore.case = TRUE)
  for (i in which(is_builder)) {
    bad_set <- sets$name[i]
    bad_def <- trimws(sets$definition[i])
    b <- .parse_set_builder(sets$definition[i])
    if (is.null(b)) {
      .cli_action(model_err$set_builder_cond,
        action = c("abort", "inform"),
        call = call
      )
    }
    if (tolower(sets$qualifier_list[i]) %=% "(intertemporal)") {
      .cli_action(model_err$set_builder_int,
        action = "abort",
        call = call
      )
    }
    if (tolower(b$src) %=% tolower(sets$name[i])) {
      .cli_action(model_err$set_self_ref,
        action = c("abort", "inform"),
        call = call
      )
    }
    src_idx <- match(tolower(b$src), tolower(sets$name))
    if (is.na(src_idx)) {
      bad_stmt <- paste("Set", sets$name[i], bad_def)
      bad_refs <- b$src
      .cli_action(model_err$set_undeclared,
        action = c("abort", "inform"),
        call = call
      )
    }
    # canonical spelling of the source set for the downstream exact
    # matches; the deployed statement is the author's text
    sets$definition[i] <- sprintf(
      "= (all,%s,%s: %s)", b$idx, sets$name[src_idx], b$cond
    )
  }

  if (any(grepl(":", sets$definition[!is_builder]))) {
    .cli_action(model_err$binary_switch,
                action = c("abort", "inform", "inform"),
                call = call)
  }

  # set equality (GEMPACK manual 10.1.2.1): Set B = A; keeps its "="
  # so downstream code can tell the bare set name from an explicit
  # single-element list. The (Intertemporal)/(Non_Intertemporal)
  # conversion forms (manual 13.3.1) are not supported.
  is_set_eq <- !is.na(sets$definition) &
    grepl("^\\s*=\\s*[A-Za-z_][A-Za-z0-9_]*\\s*$", sets$definition)

  for (i in which(is_set_eq)) {
    rhs_nm <- trimws(sub("^\\s*=\\s*", "", sets$definition[i]))
    if (tolower(rhs_nm) %=% tolower(sets$name[i])) {
      bad_set <- sets$name[i]
      .cli_action(model_err$set_self_eq,
        action = "abort",
        call = call
      )
    }
    rhs_idx <- match(rhs_nm, sets$name)
    if (is.na(rhs_idx)) {
      # names are case-insensitive (11.2.1): canonicalize a spelling
      # mismatch to the declared form so downstream exact matches hold
      rhs_idx <- match(tolower(rhs_nm), tolower(sets$name))
      if (is.na(rhs_idx)) {
        bad_stmt <- paste("Set", sets$name[i], sets$definition[i])
        bad_refs <- rhs_nm
        .cli_action(model_err$set_undeclared,
          action = c("abort", "inform"),
          call = call
        )
      }
      sets$definition[i] <- paste("=", sets$name[rhs_idx])
    }
    if (tolower(sets$qualifier_list[i]) %=% "(intertemporal)" ||
      (!is.na(rhs_idx) &&
        tolower(sets$qualifier_list[rhs_idx]) %=% "(intertemporal)")) {
      eq_statement <- paste("Set", sets$name[i], sets$definition[i])
      .cli_action(model_err$int_set_eq_fail,
        action = c("abort", "inform"),
        call = call
      )
    }
  }

  # set product (manual 10.1.1.2): intentional reject
  is_product <- !is.na(sets$definition) &
    grepl("^\\s*=.*\\s[Xx]\\s", sets$definition)
  if (any(is_product)) {
    bad_set <- sets$name[is_product][1]
    bad_def <- trimws(sets$definition[is_product][1])
    .cli_action(model_err$set_product,
      action = c("abort", "inform"),
      call = call
    )
  }

  lapply(sets$definition[!is_set_eq & !is_builder], function(entry) {
    if (!is.na(entry)) {
      if (!any(grepl(
        '\\+|\\-|\\^|&|\\(|\\)|"|union|intersect',
        entry,
        ignore.case = TRUE
      ))) {
        if (grepl(pattern = "=", x = entry)) {
          bad_def <- entry
          .cli_action(model_err$invalid_set_def,
            action = "abort",
            call = call
          )
        }
      }
    }
  })

  is_expr <- .is_set_expr(sets$definition) &
    sets$qualifier_list != "(intertemporal)" & !is_builder
  sets$definition <- ifelse(is_expr & !is_set_eq,
    trimws(sub("^\\s*=\\s*", "", sets$definition)),
    ifelse(is_set_eq | is_builder,
      trimws(sets$definition),
      trimws(gsub("\\(|=|\\)", "", sets$definition))
    )
  )
  sets$definition <- ifelse(!is_expr & !is_builder & grepl(",", sets$definition),
    strsplit(sets$definition, ","),
    sets$definition
  )
  sets$definition <- lapply(sets$definition, trimws)
  names(sets$definition) <- sets$name

  # S1/S2: expression operands must be declared sets (quoted single
  # elements aside) and never the set being defined; a spelling that
  # differs only by case is canonicalized to the declared form so the
  # downstream exact matches (implied subsets, .eval_set_expr) hold
  for (i in which(is_expr & !is_set_eq)) {
    toks <- .set_expr_tokens(sets$definition[[i]])
    named <- toks[!toks %in% c("+", "-", "^", "&", "(", ")") &
      !grepl('^"', toks)]
    bad_refs <- character(0)
    for (tk in unique(named)) {
      if (tolower(tk) %=% tolower(sets$name[i])) {
        bad_set <- sets$name[i]
        bad_def <- sets$definition[[i]]
        .cli_action(model_err$set_self_ref,
          action = c("abort", "inform"),
          call = call
        )
      }
      if (tk %in% sets$name) next
      ci <- match(tolower(tk), tolower(sets$name))
      if (is.na(ci)) {
        bad_refs <- c(bad_refs, tk)
      } else {
        sets$definition[[i]] <- gsub(
          paste0("\\b", tk, "\\b"),
          sets$name[ci],
          sets$definition[[i]]
        )
      }
    }
    if (length(bad_refs) > 0L) {
      bad_stmt <- paste("Set", sets$name[i], "=", sets$definition[[i]])
      .cli_action(model_err$set_undeclared,
        action = c("abort", "inform"),
        call = call
      )
    }
  }

  expr_info <- purrr::map2(sets$definition, is_expr, function(d, e) {
    if (isTRUE(e)) .set_expr_info(d) else NA
  })

  sets$operator <- purrr::map_chr(expr_info, function(fo) {
    if (!is.list(fo) || length(fo$ops) %=% 0L) {
      return(NA_character_)
    }
    switch(fo$ops[1], "^" = "union", "&" = "intersect", fo$ops[1])
  })

  sets$comp1 <- purrr::map_chr(expr_info, function(fo) {
    if (is.list(fo) && length(fo$named) >= 1L) fo$named[1] else NA_character_
  })
  # a builder's source set is its (implied) superset
  for (i in which(is_builder)) {
    sets$comp1[i] <- .parse_set_builder(sets$definition[[i]])$src
  }
  sets$comp2 <- purrr::map_chr(expr_info, function(fo) {
    if (is.list(fo) && length(fo$named) >= 2L) fo$named[2] else NA_character_
  })

  subsets <- extract[tolower(extract$type) %in% "subset",]
  if (any(grepl(pattern = "\\(by numbers\\)", subsets$remainder))) {
    .cli_action(
      msg = "Subset '(by numbers)' argument not supported.",
      action = "abort",
      call = call
    )
  }

  subsets$subset <- purrr::map_chr(subsets$remainder, function(s) {
    strsplit(s, " ")[[1]][1]
  })

  subsets$set <- purrr::map_chr(subsets$remainder, function(s) {
    utils::tail(strsplit(s, " ")[[1]], 1)
  })

  # S2 for Subset statements: both sides must be declared sets (an
  # unknown superset used to crash the fold below with a raw indexing
  # error); case mismatches are canonicalized to the declared form
  for (col in c("subset", "set")) {
    known <- subsets[[col]] %in% sets$name
    ci <- match(tolower(subsets[[col]]), tolower(sets$name))
    undecl <- !known & is.na(ci)
    if (any(undecl)) {
      j <- which(undecl)[1]
      bad_stmt <- paste("Subset", subsets$remainder[j])
      bad_refs <- subsets[[col]][j]
      .cli_action(model_err$set_undeclared,
        action = c("abort", "inform"),
        call = call
      )
    }
    subsets[[col]] <- ifelse(known, subsets[[col]], sets$name[ci])
  }

  sets$subsets <- vector("list", nrow(sets))
  r_idx <- match(subsets$set, sets$name)
  
  for (pos in seq_along(r_idx)) {
    id <- r_idx[pos]
    sets$subsets[[id]] <- c(sets$subsets[[id]], subsets$subset[[pos]])
  }

  # implied SUBSET statements (GEMPACK manual): all UNION/'+' makes
  # every named operand a subset of the result; all INTERSECT makes the
  # result a subset of every operand; a trailing top-level UNION
  # (INTERSECT) term is a subset (superset) of the result; the simple
  # two-set complement keeps the legacy rule (result and subtrahend are
  # subsets of the minuend). Anything else needs an explicit Subset.
  add_subs <- function(sets, set_nm, new) {
    r <- which(sets$name == set_nm)[1]
    if (is.na(r)) {
      return(sets)
    }
    sets$subsets[r] <- purrr::list_flatten(list(unique(c(sets$subsets[[r]], new))))
    sets
  }
  for (i in seq_len(nrow(sets))) {
    fo <- expr_info[[i]]
    nm <- sets$name[i]
    if (is_builder[i]) {
      # the solver emits "subset NAME is subset of SRC" with the
      # rewritten element list
      sets <- add_subs(sets, sets$comp1[i], nm)
      next
    }
    if (!is.list(fo)) next
    if (is_set_eq[i]) {
      # set equality generates SUBSET statements both ways (manual 10.1.2.1)
      sets <- add_subs(sets, nm, fo$named[1])
      sets <- add_subs(sets, fo$named[1], nm)
      next
    }
    if (fo$simple_complement) {
      sets <- add_subs(sets, fo$named[1], c(nm, fo$named[2]))
    } else if (fo$all_plus_union) {
      sets <- add_subs(sets, nm, fo$named)
    } else if (fo$all_intersect) {
      for (tnm in fo$named) sets <- add_subs(sets, tnm, nm)
    } else {
      if (isTRUE(fo$last_top_op %=% "^") && !is.na(fo$last_term)) {
        sets <- add_subs(sets, nm, fo$last_term)
      }
      if (isTRUE(fo$last_top_op %=% "&") && !is.na(fo$last_term)) {
        sets <- add_subs(sets, fo$last_term, nm)
      }
    }
  }
  
  sets$subsets <- purrr::map(sets$subsets, \(s) {
    if (is.null(s)) {
      NA
    } else {
      s
    }
  })

  names(sets$subsets) <- sets$name

  # transitive closure over direct subset relations; visited-set based
  # because set equality creates mutual (cyclic) subset pairs
  direct_subs <- sets$subsets
  for (i in seq_len(nrow(sets))) {
    nm <- sets$name[i]
    closure <- character(0)
    frontier <- direct_subs[[nm]]
    frontier <- frontier[!is.na(frontier)]
    while (length(frontier) > 0) {
      closure <- c(closure, frontier)
      nxt <- unlist(
        direct_subs[intersect(frontier, names(direct_subs))],
        use.names = FALSE
      )
      frontier <- setdiff(nxt[!is.na(nxt)], c(closure, nm))
    }
    if (length(closure) > 0) {
      sets$subsets[i] <- list(unique(closure))
    }
  }

  sets$ls_upper_idx <- NA
  sets$ls_mixed_idx <- NA
  sets <- sets[, c("type",
                   "name",
                   "label",
                   "qualifier_list",
                   "ls_upper_idx",
                   "ls_mixed_idx",
                   "header",
                   "file",
                   "definition",
                   "subsets",
                   "comp1",
                   "comp2",
                   "row_id")]
  
  # other checks should include
  # le/ge/lt/gt in the RHS of formula
  # summation in formula headers
  return(sets)
}
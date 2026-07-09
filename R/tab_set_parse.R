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

  if (any(grepl(":", sets$definition))) {
    .cli_action(model_err$binary_switch,
                action = c("abort", "inform", "inform"),
                call = call)
  }

  lapply(sets$definition, function(entry) {
    if (!is.na(entry)) {
      if (!any(grepl(
        '\\+|\\-|\\^|&|\\(|\\)|"|union|intersect',
        entry,
        ignore.case = TRUE
      ))) {
        if (grepl(pattern = "=", x = entry)) {
          .cli_action(model_err$identical_set_fail,
            action = c("abort", "inform", "inform"),
            call = call
          )
        }
      }
    }
  })

  is_expr <- .is_set_expr(sets$definition) &
    sets$qualifier_list != "(intertemporal)"
  sets$definition <- ifelse(is_expr,
    trimws(sub("^\\s*=\\s*", "", sets$definition)),
    trimws(gsub("\\(|=|\\)", "", sets$definition))
  )
  sets$definition <- ifelse(!is_expr & grepl(",", sets$definition),
    strsplit(sets$definition, ","),
    sets$definition
  )
  sets$definition <- lapply(sets$definition, trimws)
  names(sets$definition) <- sets$name

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
    if (!is.list(fo)) next
    nm <- sets$name[i]
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

  for (i in seq_len(nrow(sets))) {
    nm <- sets$name[i]
    e_ss <- sets$subsets[[nm]]
    if (!all(is.na(e_ss))) {
      ss <- with(sets$subsets, mget(e_ss))
      while (!all(is.na(ss))) {
        ss <- unlist(ss[!is.na(ss)], use.names = FALSE)
        sets$subsets[i] <- list(unique(c(sets$subsets[[i]], ss)))
        ss <- with(sets$subsets, mget(ss, ifnotfound = NA))
      }
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
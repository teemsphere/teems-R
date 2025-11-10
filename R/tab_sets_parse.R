#' @importFrom purrr map
#'
#' @keywords internal
#' @noRd
.parse_tab_sets <- function(tab_extract,
                            call) {
  sets <- subset(tab_extract, tolower(type) %in% "set")
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
  if (!any(tolower(sets$qualifier_list) %in% valid_qual)) {
    invalid_qual <- setdiff(tolower(sets$qualifier_list), valid_qual)
    .cli_action(load_err$invalid_set_qual,
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

  if (any(!"" %in% sets$remainder)) {
    .cli_action(load_err$set_parse_fail,
      action = "abort",
      call = call,
      .internal = TRUE
    )
  }

  if (any(sapply(
    sets$definition,
    function(entry) {
      sum(grepl(
        pattern = "\\+",
        x = unlist(strsplit(entry, ""))
      )) >= 2
    }
  ))) {
    .cli_action(load_err$set_op_fail,
      action = c("abort", "inform"),
      call = call
    )
  }

  lapply(sets$definition, function(entry) {
    if (!is.na(entry)) {
      if (!any(grepl(
        "\\+|\\-|union|intersect",
        entry,
        ignore.case = TRUE
      ))) {
        if (grepl(pattern = "=", x = entry)) {
          .cli_action(load_err$identical_set_fail,
            action = c("abort", "inform"),
            call = call
          )
        }
      }
    }
  })

  sets$definition <- trimws(gsub("\\(|=|\\)", "", sets$definition))
  sets$definition <- ifelse(grepl(",", sets$definition),
    strsplit(sets$definition, ","),
    sets$definition
  )
  sets$definition <- lapply(sets$definition, trimws)
  names(sets$definition) <- sets$name

  sets$operator <- ifelse(grepl("UNION", sets$definition, ignore.case = TRUE), "union",
                          ifelse(grepl("\\+", sets$definition), "+",
                                 ifelse(grepl("\\-", sets$definition), "-",
                                        ifelse(grepl("INTERSECT", sets$definition, ignore.case = TRUE), "intersect",
                                               NA))))

  sets$comp <- purrr::list_flatten(purrr::pmap(
    list(
      sets$operator,
      sets$definition,
      sets$qualifier_list
    ),
    function(o, d, q) {
      if (!is.na(x = o) && q != "(intertemporal)") {
        purrr::map(strsplit(d, "(?i)UNION|\\+|\\-|INTERSECT"), trimws)
      } else {
        NA
      }
    }
  ))

  sets$comp1 <- purrr::map_chr(sets$comp, 1)
  sets$comp2 <- purrr::map_chr(sets$comp, purrr::pluck, 2, .default = NA)

  subsets <- subset(tab_extract, tolower(type) %in% "subset")
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
    tail(strsplit(s, " ")[[1]], 1)
  })

  sets$subsets <- vector("list", nrow(sets))
  r_idx <- match(sets$name, subsets$set)
  sets$subsets <- subsets$subset[r_idx]

  for (i in 1:nrow(sets)) {
    nm <- sets$name[i]
    o <- sets$operator[i]
    c1 <- sets$comp1[i]
    c2 <- sets$comp2[i]

    if (o %=% "-") {
      c1_row <- which(sets$name == c1)
      existing_subsets <- sets$subsets[[c1_row[1]]]
      new_subsets <- c(existing_subsets, nm)
      if (!is.na(c2)) {
        new_subsets <- c(new_subsets, c2)
      }
      sets$subsets[c1_row[1]] <- purrr::list_flatten(list(unique(new_subsets)))
    } else if (o %in% c("union", "+")) {
      existing_subsets <- sets$subsets[[i]]
      if (is.na(existing_subsets)) {
        existing_subsets <- character(0)
      }
      new_subsets <- c(existing_subsets, c1, c2)
      sets$subsets[i] <- purrr::list_flatten(list(unique(new_subsets)))
    }
  }

  names(sets$subsets) <- sets$name
  existing_subsets <- subset(sets, select = subsets, drop = 1)

  for (i in 1:nrow(sets)) {
    nm <- sets$name[i]
    e_ss <- existing_subsets[[nm]]
    if (!all(is.na(e_ss))) {
      ss <- with(existing_subsets, mget(e_ss))
      while (!all(is.na(ss))) {
        ss <- unlist(ss[!is.na(ss)], use.names = FALSE)
        sets$subsets[i] <- list(unique(c(sets$subsets[[i]], ss)))
        ss <- with(sets$subsets, mget(ss, ifnotfound = NA))
      }
    }
  }

  if (any(grepl(":", sets$definition))) {
    .cli_action(model_err$binary_switch,
                action = "abort",
                call = call)
  }
  
  sets$ls_upper_idx <- NA
  sets$ls_mixed_idx <- NA
  sets <- subset(sets, select = c(type, name, label, qualifier_list, ls_upper_idx, ls_mixed_idx, header, file, definition, subsets, comp1, comp2, row_id))
  # other checks should include
  # le/ge/lt/gt in the RHS of formula
  # summation in formula headers
  return(sets)
}
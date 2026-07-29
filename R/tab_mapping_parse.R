#' Parse Mapping declarations (GEMPACK manual 11.9.1)
#'
#' `Mapping [(onto)] NAME from S1 to S2;` rows are carried through the
#' model tibble so the statement reaches the solver verbatim while the
#' name joins the 11.2.1 namespace checks and the domain/codomain sets
#' are validated against the declared sets. Set references differing
#' from a declaration only by case are canonicalized to the declared
#' spelling (GEMPACK names are case-insensitive; downstream matching
#' is exact).
#'
#' @keywords internal
#' @noRd
.parse_tab_mapping <- function(extract,
                               set_names,
                               call) {
  maps <- extract[tolower(extract$type) %in% "mapping", ]
  maps$type <- "Mapping"

  parsed <- regmatches(
    maps$remainder,
    regexec(
      "^\\s*(\\(\\s*onto\\s*\\)\\s*)?([A-Za-z][A-Za-z0-9_]*)\\s+from\\s+([A-Za-z][A-Za-z0-9_]*)\\s+to\\s+([A-Za-z][A-Za-z0-9_]*)\\s*$",
      maps$remainder,
      ignore.case = TRUE
    )
  )

  bad <- lengths(parsed) == 0L
  if (any(bad)) {
    bad_stmt <- trimws(paste("Mapping", maps$remainder[bad][1]))
    .cli_action(model_err$map_malformed,
      action = c("abort", "inform"),
      call = call
    )
  }

  maps$qualifier_list <- purrr::map_chr(parsed, function(p) {
    if (nzchar(trimws(p[2]))) "(onto)" else NA_character_
  })
  maps$name <- purrr::map_chr(parsed, 3)
  maps$comp1 <- purrr::map_chr(parsed, 4)
  maps$comp2 <- purrr::map_chr(parsed, 5)

  canonical <- function(s) {
    r_idx <- match(tolower(s), tolower(set_names))
    ifelse(is.na(r_idx), s, set_names[r_idx])
  }
  maps$comp1 <- canonical(maps$comp1)
  maps$comp2 <- canonical(maps$comp2)

  undecl <- !tolower(maps$comp1) %in% tolower(set_names) |
    !tolower(maps$comp2) %in% tolower(set_names)
  if (any(undecl)) {
    map_name <- maps$name[undecl][1]
    bad_sets <- setdiff(
      unique(c(maps$comp1[undecl], maps$comp2[undecl])),
      set_names
    )
    .cli_action(model_err$map_undeclared_set,
      action = "abort",
      call = call
    )
  }

  maps$definition <- paste("from", maps$comp1, "to", maps$comp2)
  maps$remainder <- NULL
  maps$label <- NA
  maps$ls_upper_idx <- NA
  maps$ls_mixed_idx <- NA
  maps$header <- NA
  maps$file <- NA
  maps$subsets <- NA

  maps <- maps[, c(
    "type",
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
    "row_id"
  )]
  return(maps)
}

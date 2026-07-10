#' @importFrom purrr map map_chr map_lgl
#'
#' @keywords internal
#' @note Roadmap 6.5 E1: the solver places every element of a variable
#'   referenced with a lead or lag into the dense border of the bordered
#'   matrix methods (SBBD/DBBD/NDBBD), so non-time dimensions on such
#'   variables multiply the interface problem (netcut).
#' @noRd
.check_netcut <- function(var_extract,
                          math_extract,
                          set_extract,
                          call) {

  int_sets <- set_extract$name[grepl("\\(intertemporal\\)",
    set_extract$qualifier_list,
    ignore.case = TRUE
  )]
  int_sets <- toupper(unique(int_sets[!is.na(int_sets)]))

  if (length(int_sets) == 0) {
    return(invisible(NULL))
  }

  eqs <- math_extract[math_extract$type %in% "Equation", ]

  if (nrow(eqs) == 0) {
    return(invisible(NULL))
  }

  lagged_refs <- purrr::map(eqs$definition, function(def) {
    refs <- regmatches(
      def,
      gregexpr("[a-zA-Z][a-zA-Z0-9_]*\\s*\\([^()]*\\)", def)
    )[[1]]

    keep <- purrr::map_lgl(refs, function(ref) {
      args <- sub("^[^(]*\\(", "", sub("\\)$", "", ref))
      args <- trimws(strsplit(args, ",")[[1]])
      any(grepl("^[a-zA-Z][a-zA-Z0-9_]*\\s*[+-]\\s*[0-9]+$", args))
    })

    tolower(unique(sub("\\s*\\(.*$", "", refs[keep])))
  })

  names(lagged_refs) <- eqs$name

  lagged_vars <- intersect(
    unique(unlist(lagged_refs)),
    tolower(var_extract$name)
  )

  offending <- purrr::map_lgl(lagged_vars, function(v) {
    var_sets <- var_extract$ls_upper_idx[[match(v, tolower(var_extract$name))]]
    sum(!toupper(var_sets) %in% int_sets) >= 2
  })

  if (!any(offending)) {
    return(invisible(NULL))
  }

  offenders <- purrr::map_chr(lagged_vars[offending], function(v) {
    var_sets <- var_extract$ls_upper_idx[[match(v, tolower(var_extract$name))]]
    paste0(v, "(", paste(var_sets, collapse = ","), ")")
  })

  lag_eqs <- names(lagged_refs)[purrr::map_lgl(
    lagged_refs,
    function(refs) any(lagged_vars[offending] %in% refs)
  )]

  .cli_action(model_wrn$netcut_inflation,
    action = c("warn", "inform", "inform"),
    call = call
  )

  return(invisible(NULL))
}

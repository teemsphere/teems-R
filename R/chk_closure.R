#' @importFrom purrr map2 map_chr
#'
#' @noRd
#' @keywords internal
.check_closure <- function(closure,
                           var_extract,
                           call) {
  closure <- closure[!grepl("!", closure)]
  temp <- gsub("\\([^)]*\\)", "", closure)

  closure <- unlist(purrr::map2(
    closure,
    temp,
    function(cls, t) {
      if (grepl("\\s", t)) {
        strsplit(t, " ")
      } else {
        cls
      }
    }
  ))

  cls_var <- purrr::map_chr(strsplit(closure, "\\("), 1)

  omit_vars <- var_extract$name[var_extract$condense %in% "omit"]
  backsolve_vars <- var_extract$name[var_extract$condense %in% "backsolve"]

  # substituted-out variables must be endogenous (GEMPACK manual 14.1.1)
  if (any(tolower(backsolve_vars) %in% tolower(cls_var))) {
    bs_exo <- backsolve_vars[tolower(backsolve_vars) %in% tolower(cls_var)]
    .cli_action(model_err$condense_endo,
      action = c("abort", "inform"),
      call = call
    )
  }

  # omitted variables must be exogenous and unshocked (GEMPACK manual 14.1)
  if (!all(tolower(omit_vars) %in% tolower(cls_var))) {
    omit_endo <- omit_vars[!tolower(omit_vars) %in% tolower(cls_var)]
    .cli_action(model_err$condense_exo,
      action = c("abort", "inform"),
      call = call
    )
  }

  keep <- !tolower(cls_var) %in% tolower(omit_vars)
  closure <- closure[keep]
  cls_var <- cls_var[keep]

  if (!all(cls_var %in% var_extract$name)) {
    var_discrepancy <- setdiff(tolower(cls_var), tolower(var_extract$name))
    candidates <- .nearest_names(var_discrepancy, var_extract$name)
    msg <- cls_err$unknown_var
    action <- c("abort", "inform")
    if (length(candidates) == 0L) {
      msg <- msg[1]
      action <- action[1]
    }
    .cli_action(msg,
      action = action,
      call = call
    )
  }
  return(closure)
}

#' Nearest declared names for typo suggestions (generalized edit
#' distance, case-insensitive), closest first, capped
#'
#' @importFrom utils adist
#'
#' @keywords internal
#' @noRd
.nearest_names <- function(unknown,
                           declared,
                           max_dist = 3L,
                           cap = 5L) {
  declared <- unique(declared)
  d <- utils::adist(tolower(unknown), tolower(declared))
  hits <- lapply(seq_along(unknown), function(i) {
    j <- which(d[i, ] <= max_dist)
    j[order(d[i, j])]
  })
  candidates <- declared[unique(unlist(hits))]
  utils::head(candidates, cap)
}

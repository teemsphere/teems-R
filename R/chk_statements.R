#' @importFrom purrr map_chr
#' @importFrom tools toTitleCase
#' @importFrom utils packageVersion
#' 
#' @keywords internal
#' @note Comprehensive statement-specific checks to be implemented here
#' @noRd
.check_statements <- function(tab,
                              call) {

  n_comments <- paste(unlist(strsplit(tab, "![^!]*!", perl = TRUE)), collapse = "")
  statements <- unlist(strsplit(n_comments, ";", perl = TRUE))

  statements <- gsub("\r|\n", " ", statements, perl = TRUE)
  statements <- gsub("\\s{2,}", " ", statements)
  statements <- trimws(statements[statements != " "])

  # Formula&Equation is the 10.9.1 double statement: expand it into its
  # two halves here so classification, equation counting and the
  # deployed TAB all see the split forms (the solver linearizes the
  # Equation (levels) half); covers the no-space spelling, which would
  # otherwise classify as an unknown keyword
  fe <- grep("^\\s*formula\\s*(\\(\\s*initial\\s*\\))?\\s*&\\s*equation\\b",
    statements,
    ignore.case = TRUE
  )
  if (length(fe) > 0L) {
    statements <- as.list(statements)
    for (s in fe) {
      statements[[s]] <- .expand_formula_equation(statements[[s]], call = call)
    }
    statements <- unlist(statements)
  }

  state_decl <- tolower(unique(purrr::map_chr(strsplit(statements, " ", perl = TRUE), 1)))

  if (any(state_decl %in% tolower(invalid_state))) {
    inv_state <- state_decl[state_decl %in% tolower(invalid_state)]
    inv_state <- tools::toTitleCase(inv_state)
    version <- utils::packageVersion("teems")
    .cli_action(model_err$invalid_state,
                action = c("abort", "inform"),
                call = call)
  }
  
  if (any(state_decl %in% tolower(ignored_state))) {
    ign_state <- state_decl[state_decl %in% tolower(ignored_state)]
    ign_state <- tools::toTitleCase(ign_state)
    .cli_action(model_wrn$ignored_state,
                action = "warn",
                call = call)
  }

  if (any(!state_decl %in% tolower(supported_state))) {
    # make implicit decl explicit
    for (s in seq_len(length(statements))) {
      statement <- strsplit(statements[s], split = " ")[[1]][1]
      state_check <- paste0("\\b", supported_state, "\\b")
      if (!grepl(paste(state_check, collapse = "|"),
        statement,
        ignore.case = TRUE
      )) {
        implicit_stat <- strsplit(statements[s - 1], split = " ")[[1]][1]
        statements[s] <- paste(implicit_stat, statements[s])
      }
    }
  }

  # second check
  state_decl <- tolower(unique(purrr::map_chr(strsplit(statements, " ", perl = TRUE), 1)))
  if (any(!state_decl %in% tolower(supported_state))) {
    state_decl <- unique(purrr::map_chr(strsplit(statements, " ", perl = TRUE), 1))
    unsupported <- state_decl[!tolower(state_decl) %in% tolower(supported_state)]
    unsupported <- tools::toTitleCase(unsupported)
    .cli_action(model_err$unsupported_tab,
      action = "abort",
      call = call
    )
  }

  return(statements)
}

#' Expand one Formula&Equation statement into its two 10.9.1 halves:
#' Formula (initial) keeps the body; Equation (levels) keeps the name,
#' optional label and body. Only (initial)/(levels) qualifiers are
#' legal on the double statement.
#'
#' @keywords internal
#' @noRd
.expand_formula_equation <- function(statement,
                                     call) {
  rem <- sub(
    "^\\s*formula\\s*(\\(\\s*initial\\s*\\))?\\s*&\\s*equation\\s*(\\(\\s*levels\\s*\\))?\\s*",
    "",
    statement,
    ignore.case = TRUE
  )
  m <- regmatches(
    rem,
    regexec("^([A-Za-z0-9_@]+)\\s*(#[^#]*#)?\\s*(.+)$", rem)
  )[[1]]
  if (length(m) == 0L || !grepl("=", m[4])) {
    bad_stmt <- statement
    .cli_action(model_err$formula_equation,
      action = "abort",
      call = call
    )
  }
  out <- c(
    paste("Formula (initial)", m[4]),
    trimws(paste("Equation (levels)", m[2], m[3], m[4]))
  )
  gsub("\\s{2,}", " ", out)
}

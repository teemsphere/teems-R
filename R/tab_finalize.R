#' @keywords internal
#' @noRd
.finalize_tab <- function(model) {

  if (is.null(model$postsim)) {
    model$postsim <- FALSE
  }

  # omitted variables: declaration rows are retained in the model tibble
  # (closure/shock validation reads the condense flag) but filtered from
  # the deployed TAB; backsolved declarations stay (the solver recovers
  # their values from the retained defining equations)
  omitted <- model$type == "Variable" & model$condense %in% "omit"
  backsolved <- model$type == "Variable" & model$condense %in% "backsolve"
  backsolve_writeout <- paste(
    "Backsolve",
    model$name[backsolved],
    "using",
    model$condense_eq[backsolved],
    ";"
  )
  if (!any(backsolved)) {
    backsolve_writeout <- NULL
  }
  model <- model[!omitted, ]

  set_extract <- model[model$type == "Set",]
  coeff_extract <- model[model$type == "Coefficient",]

  set_extract$name <- toupper(set_extract$name)
  set_writeout <- paste(
    "File",
    "(new)",
    set_extract$name,
    "#",
    set_extract$name,
    "output file #;\nWrite",
    "(set)",
    set_extract$name,
    "to file",
    set_extract$name,
    "header",
    paste0('"', set_extract$name, '"'),
    "longname",
    paste0('"', trimws(gsub("#", "", set_extract$label)), '"', ";")
  )

  coeff_writeout <- paste(
    "File",
    "(new)",
    coeff_extract$name,
    "#",
    coeff_extract$name,
    "output file #;\nWrite",
    coeff_extract$name,
    "to file",
    coeff_extract$name,
    "header",
    paste0('"', coeff_extract$name, '"'),
    "longname",
    paste0('"', trimws(gsub("#", "", coeff_extract$label)), '"', ";")
  )

  # PostSim executables re-wrap in a single trailing section (sections
  # are conceptually concatenated at end of file, manual 12.2); their
  # declarations stay inline, and the write-all File/Write pairs below
  # remain in the ordinary part so the solver dumps PostSim
  # coefficients after the post-solve pass
  ps_exec <- !is.na(model$postsim) & model$postsim &
    tolower(model$type) %in% c("formula", "assertion", "zerodivide")
  postsim_block <- NULL
  if (any(ps_exec)) {
    postsim_block <- c(
      "PostSim (Begin);",
      model$tab[ps_exec],
      "PostSim (End);"
    )
  }

  tab <- paste(
    c(
      model$tab[!ps_exec],
      backsolve_writeout,
      set_writeout,
      coeff_writeout,
      postsim_block
    ),
    collapse = "\n"
  )
  
  attr(tab, "file") <- attr(model, "tab_file")
  class(tab) <- c("tab", class(tab))
  return(tab)
}
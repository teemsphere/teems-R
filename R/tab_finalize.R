#' @keywords internal
#' @noRd
.finalize_tab <- function(model,
                          write_coefficients = FALSE) {

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

  is_ps <- !is.na(model$postsim) & model$postsim
  set_extract <- model[model$type == "Set",]
  # PostSim coefficients are PostSim-only names (manual 12.2.1): they are
  # declared inside the trailing section below and reach R through the
  # solver's coefficient dump, never through ordinary Write pairs
  coeff_extract <- model[model$type == "Coefficient" & !is_ps,]

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

  # coefficient values reach teems-R through the solver's binary dump
  # (<sol>.cof/.cbin); the per-coefficient CSV Write pairs are opt-in
  coeff_writeout <- if (write_coefficients) paste(
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
  ) else NULL

  # PostSim statements re-wrap in a single trailing section (sections
  # are conceptually concatenated at end of file, manual 12.2):
  # coefficient declarations and executables alike, in model order --
  # the solver classifies PostSim coefficients by where they are declared
  ps_exec <- is_ps &
    tolower(model$type) %in% c("coefficient", "formula", "assertion", "zerodivide")
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
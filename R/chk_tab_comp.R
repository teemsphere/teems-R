#' Parse one Complementarity statement into its components
#'
#' Mirrors the solver's cp_parse_stmt (teems-solver
#' tab_complementarity_transform, design doc section 7): qualifier
#' group entries (variable/lower_bound/upper_bound), the statement
#' name, the quantifier count and the expression remainder. Structural
#' defects abort; reference-level validation happens in
#' .chk_tab_comp().
#'
#' @keywords internal
#' @noRd
.parse_comp_stmt <- function(statement,
                             call) {
  txt <- gsub("#[^#]*#", " ", statement)
  m <- regmatches(
    txt,
    regexec("^\\s*complementarity\\s*\\(([^)]*)\\)\\s*(.*)$",
      txt,
      ignore.case = TRUE
    )
  )[[1]]
  if (length(m) == 0L) {
    bad_stmt <- trimws(statement)
    .cli_action(model_err$comp_malformed,
      action = c("abort", "inform"),
      call = call
    )
  }
  entries <- strsplit(m[2], ",")[[1]]
  keys <- character(0)
  vals <- character(0)
  for (e in entries) {
    kv <- strsplit(e, "=")[[1]]
    if (length(kv) != 2L) {
      bad_stmt <- trimws(statement)
      .cli_action(model_err$comp_malformed,
        action = c("abort", "inform"),
        call = call
      )
    }
    keys <- c(keys, tolower(trimws(kv[1])))
    vals <- c(vals, trimws(kv[2]))
  }
  if (any(!keys %in% c("variable", "lower_bound", "upper_bound")) ||
    anyDuplicated(keys) > 0L) {
    bad_stmt <- trimws(statement)
    .cli_action(model_err$comp_malformed,
      action = c("abort", "inform"),
      call = call
    )
  }
  if (!"variable" %in% keys) {
    bad_stmt <- trimws(statement)
    .cli_action(model_err$comp_missing_variable,
      action = "abort",
      call = call
    )
  }
  rem <- trimws(m[3])
  name <- regmatches(rem, regexec("^([A-Za-z0-9_]+)", rem))[[1]]
  if (length(name) == 0L) {
    bad_stmt <- trimws(statement)
    .cli_action(model_err$comp_malformed,
      action = c("abort", "inform"),
      call = call
    )
  }
  name <- name[2]
  rem <- trimws(substring(rem, nchar(name) + 1L))
  n_quant <- length(gregexpr("\\(\\s*all\\s*,", rem, ignore.case = TRUE)[[1]])
  if (gregexpr("\\(\\s*all\\s*,", rem, ignore.case = TRUE)[[1]][1] == -1L) {
    n_quant <- 0L
  }
  list(
    name = name,
    comp_var = vals[keys == "variable"],
    lower_bound = if ("lower_bound" %in% keys) vals[keys == "lower_bound"] else NULL,
    upper_bound = if ("upper_bound" %in% keys) vals[keys == "upper_bound"] else NULL,
    n_quant = n_quant
  )
}

#' Complementarity statement validation (GEMPACK manual 10.17/11.14)
#'
#' Mirrors the solver-side fatals of tab_complementarity_transform:
#' the VARIABLE qualifier names a declared levels variable; at least
#' one bound, each a levels variable, a Coefficient (parameter) or a
#' real constant; the name is limited to 10 characters; the quantifier
#' count equals the argument count of the variable and of each
#' non-constant bound; and the 11.14.1 condensation guards (the
#' complementarity variable must not be omitted or backsolved, bound
#' variables must not be omitted). Set matching (equal or same-ordered
#' subset) stays solver-side: it needs resolved set elements.
#'
#' @keywords internal
#' @noRd
.chk_tab_comp <- function(model,
                          call) {
  comp_stmts <- model$tab[tolower(model$type) == "complementarity"]
  if (length(comp_stmts) == 0L) {
    return(invisible(NULL))
  }
  typ <- tolower(model$type)
  is_lev <- typ == "variable" & !is.na(model$qualifier_list) &
    grepl("\\blevels\\b", model$qualifier_list, ignore.case = TRUE)
  lev_names <- tolower(model$name[is_lev])
  par_names <- tolower(model$name[
    typ == "coefficient" & !is.na(model$qualifier_list) &
      grepl("\\bparameter\\b", model$qualifier_list, ignore.case = TRUE) &
      !grepl("\\bnon_parameter\\b", model$qualifier_list, ignore.case = TRUE)
  ])
  is_num <- function(x) {
    grepl("^[-+]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][-+]?[0-9]+)?$", x)
  }
  n_args_of <- function(nme) {
    r <- which(tolower(model$name) == tolower(nme) &
      typ %in% c("variable", "coefficient"))[1]
    idx <- model$ls_upper_idx[[r]]
    if (idx %=% NA || is.null(idx)) {
      return(0L)
    }
    length(idx)
  }
  for (statement in comp_stmts) {
    cp <- .parse_comp_stmt(statement, call = call)
    comp_name <- cp$name
    if (nchar(comp_name) > 10L) {
      .cli_action(model_err$comp_name_length,
        action = "abort",
        call = call
      )
    }
    comp_var <- cp$comp_var
    if (!tolower(comp_var) %in% lev_names) {
      .cli_action(model_err$comp_not_levels,
        action = "abort",
        call = call
      )
    }
    bounds <- c(cp$lower_bound, cp$upper_bound)
    if (length(bounds) == 0L) {
      .cli_action(model_err$comp_no_bound,
        action = "abort",
        call = call
      )
    }
    for (b in bounds) {
      if (is_num(b)) next
      if (tolower(b) %in% c(lev_names, par_names)) next
      bad_bound <- b
      .cli_action(model_err$comp_bad_bound,
        action = c("abort", "inform"),
        call = call
      )
    }
    # quantifier count vs the variable's and non-constant bounds'
    # argument counts (11.14 points 2-3; set matching is solver-side)
    for (ref_name in c(comp_var, bounds[!is_num(bounds)])) {
      n_args <- n_args_of(ref_name)
      n_quant <- cp$n_quant
      if (n_quant != n_args) {
        .cli_action(model_err$comp_quant_count,
          action = "abort",
          call = call
        )
      }
    }
    # 11.14.1 condensation guards
    comp_refs <- rbind(
      data.frame(nme = comp_var, role = "variable", no_backsolve = TRUE),
      if (!is.null(cp$lower_bound) && !is_num(cp$lower_bound)) {
        data.frame(nme = cp$lower_bound, role = "lower bound", no_backsolve = FALSE)
      },
      if (!is.null(cp$upper_bound) && !is_num(cp$upper_bound)) {
        data.frame(nme = cp$upper_bound, role = "upper bound", no_backsolve = FALSE)
      }
    )
    for (r in seq_len(nrow(comp_refs))) {
      v_row <- which(tolower(model$name) == tolower(comp_refs$nme[r]) &
        typ == "variable")[1]
      if (is.na(v_row)) next
      cond <- model$condense[v_row]
      if (cond %in% "omit" ||
        (comp_refs$no_backsolve[r] && cond %in% "backsolve")) {
        bad_var <- model$name[v_row]
        bad_action <- ifelse(cond %in% "omit", "omitted", "backsolved")
        comp_role <- comp_refs$role[r]
        .cli_action(model_err$comp_condense,
          action = c("abort", "inform"),
          call = call
        )
      }
    }
  }
  return(invisible(NULL))
}

#' Active complementarity components in the final closure (C2)
#'
#' Mirrors the solver's comp_closure_check rebalance (teems-solver C2,
#' design doc section 8) on the FINAL (post-swap) closure: each
#' complementarity contributes one E_$comp equation of quantifier size;
#' components whose variable stays ENDOGENOUS are ACTIVE (the solver
#' exogenizes their dummy and runs the approximate-run state
#' machinery), components exogenized by the closure are inert (the
#' endogenous dummy absorbs the row, net zero). For count-squaring the
#' system therefore gains one equation element per ACTIVE component --
#' this function returns that total; .check_system_square adds it.
#'
#' @importFrom purrr map_chr map_dbl
#'
#' @keywords internal
#' @noRd
.comp_active_count <- function(model,
                               closure,
                               var_extract,
                               sets,
                               call) {
  comp_stmts <- model$tab[tolower(model$type) == "complementarity"]
  if (length(comp_stmts) == 0L) {
    return(0)
  }
  cls_vars <- tolower(purrr::map_chr(closure, attr, "var_name"))
  n_active <- 0
  for (statement in comp_stmts) {
    cp <- .parse_comp_stmt(statement, call = call)
    v_row <- which(tolower(var_extract$name) == tolower(cp$comp_var))[1]
    if (is.na(v_row)) next
    comp_var <- var_extract$name[v_row]
    var_sets <- var_extract$ls_upper_idx[[v_row]]
    if (var_sets %=% NA || is.null(var_sets)) {
      n_ele <- 1L
    } else {
      n_ele <- prod(lengths(with(sets$ele, mget(var_sets, ifnotfound = ""))))
    }
    entries <- closure[cls_vars == tolower(comp_var)]
    n_exo <- sum(purrr::map_dbl(
      entries,
      function(entry) {
        ele <- attr(entry, "ele")
        if (ele %=% NA || is.null(ele)) {
          return(1)
        }
        nrow(ele)
      }
    ))
    n_active <- n_active + max(n_ele - n_exo, 0)
  }
  n_active
}

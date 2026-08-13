# Deploy-time auto-omission (teems-solver ROADMAP 6.2 follow-up). A
# variable that is exogenous across every one of its elements and carries
# no shock has a change of zero: its columns contribute nothing to the
# shock vector, so omitting it shrinks the exogenous side without
# altering the solved system. Unlike substitution (backsolving), omission
# densifies nothing, which is why it keeps its own rationale after the
# 2026-07 benchmark round.
#
# The nomination cannot be made at ems_model() time -- only the post-swap
# closure and the finalized shocks say which variables qualify -- so
# ems_model(auto_omit = TRUE) records the intent and the work happens
# here, inside ems_deploy().

#' @importFrom purrr map_chr map_dbl
#' @importFrom cli cli_vec
#'
#' @keywords internal
#' @noRd
.auto_omit <- function(model,
                       closure,
                       sets,
                       shock,
                       call) {
  live <- model$type == "Variable" & is.na(model$condense)
  vars <- model[live, ]
  if (nrow(vars) == 0L) {
    return(NULL)
  }

  # a user-supplied shock file is opaque -- nothing says which variables
  # it shocks, so no variable can be shown to be unshocked
  if (is.character(shock)) {
    .cli_action(deploy_info$auto_omit_shock_file,
      action = c("inform", "inform"),
      call = call
    )
    return(NULL)
  }
  shocked <- character(0)
  if (!is.null(shock)) {
    shocked <- tolower(purrr::map_chr(shock, "var"))
  }

  # exogenous element count per variable, post-swap
  cls_var <- tolower(purrr::map_chr(closure, attr, "var_name"))
  cls_n <- purrr::map_dbl(closure, function(entry) {
    ele <- attr(entry, "ele")
    if (ele %=% NA) {
      return(1)
    }
    nrow(ele)
  })
  exo_n <- tapply(cls_n, cls_var, sum)

  # references outside Equation/Update statements are not rewritten by
  # the omission machinery, so a variable named anywhere else (PostSim
  # formulas, assertions, complementarity components, writes) is left
  # alone rather than deployed as a dangling reference
  elsewhere <- !model$type %in% c("Variable", "Equation", "Update")
  other_tab <- model$tab[elsewhere]

  nominate <- rep(FALSE, nrow(vars))
  for (i in seq_len(nrow(vars))) {
    var <- vars$name[[i]]
    if (tolower(var) %in% shocked) {
      next
    }
    exo <- unname(exo_n[tolower(var)])
    n_ele <- .count_var_elements(
      var_extract = vars[i, ],
      sets = sets
    )
    if (is.na(exo) || exo < n_ele) {
      next
    }
    pattern <- paste0("(?<![[:alnum:]_])", var, "(?![[:alnum:]_])")
    nominate[[i]] <- !any(grepl(pattern, other_tab,
      perl = TRUE,
      ignore.case = TRUE
    ))
  }

  omit_vars <- vars$name[nominate]
  if (length(omit_vars) == 0L) {
    .cli_action(deploy_info$auto_omit_none,
      action = "inform",
      call = call
    )
    return(NULL)
  }

  statement_type <- tolower(model$type)
  for (var in omit_vars) {
    model$tab <- .zero_var_refs(
      var = var,
      tab = model$tab,
      statement_type = statement_type,
      has_args = vars$ls_upper_idx[[match(var, vars$name)]] %!=% NA
    )
  }
  model$condense[model$type == "Variable" &
    tolower(model$name) %in% tolower(omit_vars)] <- "omit"

  # omitted variables leave the system entirely: their closure entries go
  # with them (the equivalent filter for user omissions runs against the
  # raw closure in .check_closure)
  cls_file <- attr(closure, "file")
  cls_class <- class(closure)
  closure <- closure[!cls_var %in% tolower(omit_vars)]
  attr(closure, "file") <- cls_file
  class(closure) <- cls_class

  n_auto_omit <- length(omit_vars)
  omitted_shown <- cli::cli_vec(
    omit_vars,
    style = list("vec-trunc" = 5)
  )
  .cli_action(deploy_info$auto_omit,
    action = c("inform", "inform"),
    call = call
  )

  list(
    model = model,
    closure = closure,
    omitted = omit_vars
  )
}

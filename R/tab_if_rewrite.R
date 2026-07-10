#' GEMPACK IF in formulas (manual 11.4.5-11.4.7). An IF term entering a
#' Formula's right-hand side additively at the top level is rewritten
#' into sequential formulas the solver executes natively: a base
#' formula with the IF terms dropped, then one accumulate formula per
#' IF term whose domain is narrowed to where the condition holds, so
#' the value expression is only ever evaluated there (matching GEMPACK,
#' which does not evaluate the expression when the condition fails).
#' Conditions follow the manual's simple shapes:
#'   <index> in <set>     the index's quantifier is narrowed to
#'                        <set> & <range> (rule 4: <set> need not be a
#'                        subset of the current range)
#'   <index> = "element"  the quantifier is narrowed to a synthesized
#'                        singleton set "element" & <range>
#'   <coefref> <op> <c>   a conditional quantifier ':' is appended to
#'                        the statement's last quantifier
#' Compound conditions (AND/OR/NOT), non-additive IF placement, and IF
#' nested below the top level abort.
#'
#' @keywords internal
#' @noRd
.rewrite_tab_if <- function(tab,
                            call) {
  if_pattern <- "(^|[^A-Za-z0-9_])[Ii][Ff]\\s*[][({]"
  has_if <- grepl(if_pattern, tab)
  if (!any(has_if)) {
    return(tab)
  }

  synth <- new.env(parent = emptyenv())
  synth$n <- 0L
  synth$tab <- tab

  out <- vector("list", length(tab))
  for (s in seq_along(tab)) {
    if (!has_if[s]) {
      out[[s]] <- tab[s]
      next
    }
    type <- tolower(sub("\\s.*$", "", tab[s]))
    if (type %=% "formula") {
      out[[s]] <- .rewrite_formula_if(
        stmt = tab[s],
        synth = synth,
        call = call
      )
    } else if (type %=% "equation") {
      .cli_action(model_err$invalid_if_eq,
        action = "abort",
        call = call
      )
    } else {
      out[[s]] <- tab[s]
    }
  }
  unlist(out, use.names = FALSE)
}

#' Character-level bracket depth before each character; '(' '[' '{' are
#' interchangeable (manual 11.4.6). Quoted element literals are opaque.
#'
#' @keywords internal
#' @noRd
.tab_scan <- function(s) {
  chs <- strsplit(s, "")[[1]]
  qcum <- cumsum(chs == '"')
  in_quote <- qcum %% 2L == 1L | chs == '"'
  opens <- chs %in% c("(", "[", "{") & !in_quote
  closes <- chs %in% c(")", "]", "}") & !in_quote
  depth <- cumsum(opens) - cumsum(closes)
  list(
    chs = chs,
    in_quote = in_quote,
    depth_before = c(0L, depth[-length(chs)])
  )
}

#' Position of the bracket matching the opener at position i.
#'
#' @keywords internal
#' @noRd
.match_bracket <- function(s, i) {
  scan <- .tab_scan(s)
  target <- scan$depth_before[i] + 1L
  n <- length(scan$chs)
  j <- i + 1L
  while (j <= n) {
    if (scan$depth_before[j] == target &&
      scan$chs[j] %in% c(")", "]", "}") && !scan$in_quote[j]) {
      return(j)
    }
    j <- j + 1L
  }
  NA_integer_
}

#' Split an expression into its top-level additive terms.
#'
#' @keywords internal
#' @noRd
.split_tab_terms <- function(s) {
  scan <- .tab_scan(s)
  chs <- scan$chs
  n <- length(chs)
  is_pm <- chs %in% c("+", "-") & scan$depth_before == 0L & !scan$in_quote
  idx <- which(is_pm)
  # a +/- directly after an exponent marker continues a numeric literal
  expo <- idx > 2L & chs[pmax(idx - 1L, 1L)] %in% c("e", "E") &
    grepl("[0-9.]", chs[pmax(idx - 2L, 1L)])
  idx <- idx[!expo]
  first_char <- which(chs != " ")[1]
  lead_sign <- !is.na(first_char) && chs[first_char] %in% c("+", "-")
  idx <- idx[!idx %in% first_char]
  starts <- c(1L, idx)
  ends <- c(idx - 1L, n)
  body <- substring(s, starts, ends)
  sign <- c(
    if (lead_sign && chs[first_char] %=% "-") "-" else "+",
    chs[idx]
  )
  if (lead_sign) {
    body[1] <- sub("^\\s*[+-]", "", body[1])
  }
  body[-1] <- sub("^[+-]", "", body[-1])
  list(sign = sign, body = trimws(body))
}

#' Is this term exactly one IF call? Returns its condition and value
#' (split at the first top-level comma) or NULL.
#'
#' @keywords internal
#' @noRd
.parse_if_term <- function(body) {
  if (!grepl("^[Ii][Ff]\\s*[][({]", body)) {
    return(NULL)
  }
  open <- regexpr("[][({]", body)
  close <- .match_bracket(body, open)
  if (is.na(close) || nchar(trimws(substring(body, close + 1L))) > 0L) {
    return(NULL)
  }
  inner <- substr(body, open + 1L, close - 1L)
  scan <- .tab_scan(inner)
  comma <- which(scan$chs == "," & scan$depth_before == 0L & !scan$in_quote)
  if (length(comma) %=% 0L) {
    return(NULL)
  }
  list(
    cond = trimws(substr(inner, 1L, comma[1] - 1L)),
    value = trimws(substring(inner, comma[1] + 1L))
  )
}

#' Classify an IF condition into the supported shapes.
#'
#' @keywords internal
#' @noRd
.classify_if_cond <- function(cond) {
  m <- regmatches(cond, regexec(
    "^([A-Za-z_][A-Za-z0-9_]*)\\s+[Ii][Nn]\\s+([A-Za-z_][A-Za-z0-9_]*)$",
    cond
  ))[[1]]
  if (length(m) > 0L) {
    return(list(kind = "in_set", idx = m[2], set = m[3]))
  }
  m <- regmatches(cond, regexec(
    '^([A-Za-z_][A-Za-z0-9_]*)\\s*=\\s*"([^"]+)"$',
    cond
  ))[[1]]
  if (length(m) > 0L) {
    return(list(kind = "elem", idx = m[2], elem = m[3]))
  }
  ops <- c(
    eq = "=", ne = "<>", gt = ">", lt = "<", ge = ">=", le = "<=",
    "=" = "=", "<>" = "<>", ">" = ">", "<" = "<", ">=" = ">=", "<=" = "<="
  )
  m <- regmatches(cond, regexec(
    paste0(
      "^([A-Za-z_][A-Za-z0-9_]*(\\([^()]*\\))?)\\s*",
      "([Ee][Qq]|[Nn][Ee]|[Gg][Tt]|[Ll][Tt]|[Gg][Ee]|[Ll][Ee]|<=|>=|<>|=|<|>)\\s*",
      "([-+]?[0-9]*\\.?[0-9]+([eE][-+]?[0-9]+)?)$"
    ),
    cond
  ))[[1]]
  if (length(m) > 0L) {
    return(list(
      kind = "cmp",
      ref = gsub("\\s", "", m[2]),
      op = ops[[tolower(m[4])]],
      num = m[5]
    ))
  }
  NULL
}

#' A fresh set name unused anywhere in the model.
#'
#' @keywords internal
#' @noRd
.synth_set_name <- function(synth) {
  repeat {
    synth$n <- synth$n + 1L
    nm <- paste0("IFS", synth$n)
    hit <- paste0("(^|[^A-Za-z0-9_])", nm, "([^A-Za-z0-9_]|$)")
    if (!any(grepl(hit, synth$tab, ignore.case = TRUE))) {
      return(nm)
    }
  }
}

#' Rewrite one Formula statement; returns the replacement statements
#' (synthesized Set statements first, then base and accumulate
#' formulas).
#'
#' @keywords internal
#' @noRd
.rewrite_formula_if <- function(stmt,
                                synth,
                                call) {
  body <- sub("^\\s*[Ff][Oo][Rr][Mm][Uu][Ll][Aa]\\s*", "", stmt)

  label <- ""
  if (startsWith(trimws(body), "#")) {
    close <- regexpr("#[^#]*#", body)
    label <- paste0(substr(body, close, close + attr(close, "match.length") - 1L), " ")
    body <- substring(body, close + attr(close, "match.length"))
  }

  groups <- character(0)
  repeat {
    body <- sub("^\\s+", "", body)
    if (!startsWith(body, "(")) break
    close <- .match_bracket(body, 1L)
    groups <- c(groups, substr(body, 1L, close))
    body <- substring(body, close + 1L)
  }

  quant <- lapply(groups, function(g) {
    inner <- trimws(substr(g, 2L, nchar(g) - 1L))
    m <- regmatches(inner, regexec(
      "^[Aa][Ll][Ll]\\s*,\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*,\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*(:.*)?$",
      inner
    ))[[1]]
    if (length(m) %=% 0L) {
      return(list(is_quant = FALSE, text = g))
    }
    list(
      is_quant = TRUE, text = g, idx = m[2], set = m[3],
      cond = nchar(m[4]) > 0L
    )
  })

  scan <- .tab_scan(body)
  eq_pos <- which(scan$chs == "=" & scan$depth_before == 0L & !scan$in_quote)
  lhs <- trimws(substr(body, 1L, eq_pos[1] - 1L))
  rhs <- trimws(substring(body, eq_pos[1] + 1L))

  if_pattern <- "(^|[^A-Za-z0-9_])[Ii][Ff]\\s*[][({]"
  terms <- .split_tab_terms(rhs)
  parsed <- lapply(terms$body, .parse_if_term)
  is_if <- !purrr::map_lgl(parsed, is.null)

  if (any(grepl(if_pattern, terms$body[!is_if]))) {
    if_statement <- stmt
    .cli_action(model_err$invalid_if_placement,
      action = c("abort", "inform"),
      call = call
    )
  }

  base_rhs <- paste(
    ifelse(terms$sign[!is_if] == "-", "- ", ""),
    terms$body[!is_if],
    sep = "",
    collapse = " + "
  )
  base_rhs <- gsub("+ - ", "- ", base_rhs, fixed = TRUE)
  if (base_rhs %=% "") {
    base_rhs <- "0"
  }
  header <- paste0(label, paste0(purrr::map_chr(quant, "text"), collapse = ""))
  statements <- paste("Formula", header, lhs, "=", base_rhs)
  pre <- character(0)

  q_idx <- purrr::map_chr(quant, function(q) {
    if (isTRUE(q$is_quant)) q$idx else NA_character_
  })

  narrow <- function(cond_info, if_cond) {
    at <- match(tolower(cond_info$idx), tolower(q_idx))
    if (is.na(at)) {
      .cli_action(model_err$invalid_if_cond,
        action = c("abort", "inform"),
        call = call
      )
    }
    range_set <- quant[[at]]$set
    operand <- if (cond_info$kind %=% "in_set") {
      cond_info$set
    } else {
      paste0('"', cond_info$elem, '"')
    }
    key <- toupper(paste0(operand, "&", range_set))
    nm <- synth[[key]]
    if (is.null(nm)) {
      nm <- .synth_set_name(synth)
      synth[[key]] <- nm
      pre <<- c(pre, sprintf(
        "Set %s # if-rewrite %s intersect %s # = %s & %s",
        nm, operand, range_set, operand, range_set
      ))
    }
    q2 <- quant
    q2[[at]]$text <- sprintf("(all,%s,%s)", cond_info$idx, nm)
    q2
  }

  for (k in which(is_if)) {
    if_cond <- parsed[[k]]$cond
    cond_info <- .classify_if_cond(if_cond)
    if (is.null(cond_info)) {
      .cli_action(model_err$invalid_if_cond,
        action = c("abort", "inform"),
        call = call
      )
    }
    if (cond_info$kind %in% c("in_set", "elem")) {
      if (cond_info$kind %=% "in_set" &&
        toupper(cond_info$set) %in% toupper(q_idx[!is.na(q_idx)])) {
        # an index, not a set, on the right of IN
        .cli_action(model_err$invalid_if_cond,
          action = c("abort", "inform"),
          call = call
        )
      }
      q2 <- narrow(cond_info, if_cond)
      header2 <- paste0(label, paste0(purrr::map_chr(q2, "text"), collapse = ""))
    } else {
      last_q <- max(which(!is.na(q_idx)), -Inf)
      if (is.infinite(last_q) || isTRUE(quant[[last_q]]$cond)) {
        .cli_action(model_err$invalid_if_cond,
          action = c("abort", "inform"),
          call = call
        )
      }
      q2 <- quant
      q2[[last_q]]$text <- sprintf(
        "(all,%s,%s: %s %s %s)",
        quant[[last_q]]$idx, quant[[last_q]]$set,
        cond_info$ref, cond_info$op, cond_info$num
      )
      header2 <- paste0(label, paste0(purrr::map_chr(q2, "text"), collapse = ""))
    }
    statements <- c(statements, paste(
      "Formula", header2, lhs, "=",
      lhs, terms$sign[k], paste0("[", parsed[[k]]$value, "]")
    ))
  }

  c(pre, statements)
}

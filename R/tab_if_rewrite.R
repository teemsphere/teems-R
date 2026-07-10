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
      out[[s]] <- .rewrite_equation_if(
        stmt = tab[s],
        synth = synth,
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

#' A fresh coefficient name unused anywhere in the model.
#'
#' @keywords internal
#' @noRd
.synth_coeff_name <- function(synth) {
  if (is.null(synth$nc)) {
    synth$nc <- 0L
  }
  repeat {
    synth$nc <- synth$nc + 1L
    nm <- paste0("IFC", synth$nc)
    hit <- paste0("(^|[^A-Za-z0-9_])", nm, "([^A-Za-z0-9_]|$)")
    if (!any(grepl(hit, synth$tab, ignore.case = TRUE))) {
      return(nm)
    }
  }
}

#' A synthesized intersection set (cached by operand and range).
#' Returns list(pre, name).
#'
#' @keywords internal
#' @noRd
.synth_intersect_set <- function(operand, range_set, synth) {
  key <- toupper(paste0(operand, "&", range_set))
  nm <- synth[[key]]
  pre <- character(0)
  if (is.null(nm)) {
    nm <- .synth_set_name(synth)
    synth[[key]] <- nm
    pre <- sprintf(
      "Set %s # if-rewrite %s intersect %s # = %s & %s",
      nm, operand, range_set, operand, range_set
    )
  }
  list(pre = pre, name = nm)
}

#' A 0/1 indicator coefficient for a data-comparison IF condition in an
#' Equation (the shipped MAKESUNIT/E_qca adaptation shape). Cached by
#' the condition with index names canonicalized to their sets, so the
#' same condition in another equation reuses one indicator. Returns
#' list(pre, ref) where ref uses the current statement's index names.
#'
#' @keywords internal
#' @noRd
.if_indicator <- function(cond_info,
                          quant,
                          q_idx,
                          synth,
                          if_cond,
                          call) {
  argstr <- sub("^[A-Za-z_][A-Za-z0-9_]*", "", cond_info$ref)
  args <- if (nchar(argstr) > 0L) {
    trimws(strsplit(gsub("[()]", "", argstr), ",")[[1]])
  } else {
    character(0)
  }
  is_idx <- !grepl('^"', args)
  at <- match(tolower(args[is_idx]), tolower(q_idx))
  if (length(args[is_idx]) %=% 0L || anyNA(at)) {
    # scalar conditions have no quantifier to carry the comparison
    .cli_action(model_err$invalid_if_cond,
      action = c("abort", "inform"),
      call = call
    )
  }
  dims <- args[is_idx]
  sets <- purrr::map_chr(quant[at], "set")
  canon <- args
  canon[is_idx] <- toupper(sets)
  key <- paste0(
    "CMP|", toupper(sub("\\(.*$", "", cond_info$ref)),
    "(", paste(canon, collapse = ","), ")|", cond_info$op, "|", cond_info$num
  )
  ind <- synth[[key]]
  pre <- character(0)
  if (is.null(ind)) {
    ind <- .synth_coeff_name(synth)
    synth[[key]] <- ind
    quants <- sprintf("(all,%s,%s)", dims, sets)
    n <- length(quants)
    quants_cond <- c(
      quants[-n],
      sprintf(
        "(all,%s,%s: %s %s %s)",
        dims[n], sets[n], cond_info$ref, cond_info$op, cond_info$num
      )
    )
    dimargs <- paste(dims, collapse = ",")
    pre <- c(
      sprintf(
        "Coefficient %s %s(%s) # if-rewrite indicator %s #",
        paste0(quants, collapse = ""), ind, dimargs, if_cond
      ),
      sprintf(
        "Formula %s %s(%s) = 0",
        paste0(quants, collapse = ""), ind, dimargs
      ),
      sprintf(
        "Formula %s %s(%s) = 1",
        paste0(quants_cond, collapse = ""), ind, dimargs
      )
    )
  }
  list(pre = pre, ref = sprintf("%s(%s)", ind, paste(dims, collapse = ",")))
}

#' Two fresh equation names for a domain split.
#'
#' @keywords internal
#' @noRd
.synth_eq_names <- function(name, synth) {
  purrr::map_chr(c("A", "B"), function(suffix) {
    nm <- paste0(name, suffix)
    hit <- paste0("(^|[^A-Za-z0-9_])", nm, "([^A-Za-z0-9_]|$)")
    while (any(grepl(hit, synth$tab, ignore.case = TRUE))) {
      nm <- paste0(nm, suffix)
      hit <- paste0("(^|[^A-Za-z0-9_])", nm, "([^A-Za-z0-9_]|$)")
    }
    nm
  })
}

#' Rewrite one Equation statement. Data-comparison IF terms become 0/1
#' indicator coefficients distributed over the value's top-level terms
#' (the value must already be valid over the full domain, as in
#' GEMPACK, where only IN conditions relax index checking). A set-
#' membership or element IF term splits the equation into two
#' complementary-domain equations (manual 11.4.7 rule 2: inside the
#' value the index is deemed to range over the condition's set, so the
#' value may reference arrays declared only there and must not be
#' evaluated elsewhere). Returns the replacement statements.
#'
#' @keywords internal
#' @noRd
.rewrite_equation_if <- function(stmt,
                                 synth,
                                 call) {
  rest <- trimws(sub("^\\s*[Ee][Qq][Uu][Aa][Tt][Ii][Oo][Nn]\\s*", "", stmt))

  qual <- ""
  if (startsWith(rest, "(")) {
    close <- .match_bracket(rest, 1L)
    qual <- paste0(substr(rest, 1L, close), " ")
    rest <- trimws(substring(rest, close + 1L))
  }
  name <- sub("^([A-Za-z_][A-Za-z0-9_]*).*$", "\\1", rest)
  rest <- trimws(sub("^[A-Za-z_][A-Za-z0-9_]*", "", rest))

  label <- ""
  if (startsWith(rest, "#")) {
    m <- regexpr("^#[^#]*#", rest)
    label <- paste0(substr(rest, 1L, attr(m, "match.length")), " ")
    rest <- trimws(substring(rest, attr(m, "match.length") + 1L))
  }

  groups <- character(0)
  repeat {
    rest <- sub("^\\s+", "", rest)
    if (!startsWith(rest, "(")) break
    close <- .match_bracket(rest, 1L)
    groups <- c(groups, substr(rest, 1L, close))
    rest <- substring(rest, close + 1L)
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
  q_idx <- purrr::map_chr(quant, function(q) {
    if (isTRUE(q$is_quant)) q$idx else NA_character_
  })

  scan <- .tab_scan(rest)
  eq_pos <- which(scan$chs == "=" & scan$depth_before == 0L & !scan$in_quote)
  sides <- list(
    trimws(substr(rest, 1L, eq_pos[1] - 1L)),
    trimws(substring(rest, eq_pos[1] + 1L))
  )

  if_pattern <- "(^|[^A-Za-z0-9_])[Ii][Ff]\\s*[][({]"
  pre <- character(0)
  membership <- NULL

  # pass 1: indicators for data comparisons; collect membership terms
  side_terms <- vector("list", 2L)
  for (h in 1:2) {
    terms <- .split_tab_terms(sides[[h]])
    parsed <- lapply(terms$body, .parse_if_term)
    is_if <- !purrr::map_lgl(parsed, is.null)
    if (any(grepl(if_pattern, terms$body[!is_if]))) {
      if_statement <- stmt
      .cli_action(model_err$invalid_if_placement,
        action = c("abort", "inform"),
        call = call
      )
    }
    chunks <- paste(terms$sign, terms$body)
    for (k in which(is_if)) {
      if_cond <- parsed[[k]]$cond
      cond_info <- .classify_if_cond(if_cond)
      if (is.null(cond_info)) {
        .cli_action(model_err$invalid_if_cond,
          action = c("abort", "inform"),
          call = call
        )
      }
      if (cond_info$kind %=% "cmp") {
        ind <- .if_indicator(cond_info, quant, q_idx, synth, if_cond, call)
        pre <- c(pre, ind$pre)
        vt <- .split_tab_terms(parsed[[k]]$value)
        chunks[k] <- paste(
          ifelse(vt$sign == terms$sign[k], "+", "-"),
          paste0(ind$ref, " * ", vt$body),
          collapse = " "
        )
      } else {
        if (!is.null(membership)) {
          if_statement <- stmt
          .cli_action(model_err$invalid_if_multi,
            action = c("abort", "inform"),
            call = call
          )
        }
        membership <- list(
          side = h, term = k, cond_info = cond_info,
          sign = terms$sign[k], value = parsed[[k]]$value,
          if_cond = if_cond
        )
      }
    }
    side_terms[[h]] <- chunks
  }

  assemble <- function(side_chunks, quants, eq_name) {
    header <- paste0(purrr::map_chr(quants, "text"), collapse = "")
    lhs <- sub("^\\+\\s*", "", paste(side_chunks[[1]], collapse = " "))
    rhs <- sub("^\\+\\s*", "", paste(side_chunks[[2]], collapse = " "))
    if (lhs %=% "") lhs <- "0"
    if (rhs %=% "") rhs <- "0"
    paste0(
      "Equation ", qual, eq_name, " ", label, header, " ",
      lhs, " = ", rhs
    )
  }

  if (is.null(membership)) {
    return(c(pre, assemble(side_terms, quant, name)))
  }

  # pass 2: split the equation on the membership condition's domain
  cond_info <- membership$cond_info
  at <- match(tolower(cond_info$idx), tolower(q_idx))
  if (is.na(at) || isTRUE(quant[[at]]$cond) ||
    (cond_info$kind %=% "in_set" &&
      toupper(cond_info$set) %in% toupper(q_idx[!is.na(q_idx)]))) {
    if_cond <- membership$if_cond
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
  inter <- .synth_intersect_set(operand, range_set, synth)
  pre <- c(pre, inter$pre)
  comp_key <- toupper(paste0(range_set, "-", inter$name))
  comp <- synth[[comp_key]]
  if (is.null(comp)) {
    comp <- .synth_set_name(synth)
    synth[[comp_key]] <- comp
    pre <- c(pre, sprintf(
      "Set %s # if-rewrite %s minus %s # = %s - %s",
      comp, range_set, inter$name, range_set, inter$name
    ))
  }

  names2 <- .synth_eq_names(name, synth)
  q_in <- quant
  q_in[[at]]$text <- sprintf("(all,%s,%s)", cond_info$idx, inter$name)
  q_out <- quant
  q_out[[at]]$text <- sprintf("(all,%s,%s)", cond_info$idx, comp)

  chunks_in <- side_terms
  chunks_in[[membership$side]][membership$term] <- paste(
    membership$sign, paste0("[", membership$value, "]")
  )
  chunks_out <- side_terms
  chunks_out[[membership$side]] <-
    chunks_out[[membership$side]][-membership$term]

  c(
    pre,
    assemble(chunks_in, q_in, names2[1]),
    assemble(chunks_out, q_out, names2[2])
  )
}
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
    inter <- .synth_intersect_set(operand, range_set, synth)
    pre <<- c(pre, inter$pre)
    q2 <- quant
    q2[[at]]$text <- sprintf("(all,%s,%s)", cond_info$idx, inter$name)
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

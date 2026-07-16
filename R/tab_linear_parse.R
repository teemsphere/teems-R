# Linear-expression parser for TABLO equation definitions (condensation).
#
# A side of a linearized equation is flattened into a list of terms:
#   list(sign   = 1L | -1L,
#        quants = list(list(idx =, set =)),  # enclosing sums, outermost first
#        fac    = character(),               # coefficient factor texts
#        ops    = character(),               # "*" or "/" per factor
#        var    = list(name =, args =) | NULL)
# Subexpressions free of linear variables stay opaque single-factor texts
# so coefficient algebra is carried verbatim into rewritten equations.

#' @keywords internal
#' @noRd
.tokenize_expr <- function(text) {
  pattern <- paste0(
    "\"[^\"]*\"",
    "|[A-Za-z_][A-Za-z0-9_]*",
    "|(?:[0-9]+\\.?[0-9]*|\\.[0-9]+)(?:[eE][+-]?[0-9]+)?",
    "|[-+*/^(),\\[\\]{}]"
  )
  tokens <- regmatches(text, gregexpr(pattern, text, perl = TRUE))[[1]]
  leftover <- gsub(pattern, "", text, perl = TRUE)
  leftover <- gsub("\\s", "", leftover)
  if (nchar(leftover) > 0L) {
    stop(paste0("unrecognized characters {", leftover, "}"), call. = FALSE)
  }
  return(tokens)
}

#' @keywords internal
#' @noRd
.pk <- function(st) {
  if (st$pos > length(st$tokens)) {
    return(NA_character_)
  }
  st$tokens[[st$pos]]
}

#' @keywords internal
#' @noRd
.adv <- function(st) {
  tok <- .pk(st)
  st$pos <- st$pos + 1L
  tok
}

#' @keywords internal
#' @noRd
.expect <- function(st, tok) {
  got <- .adv(st)
  if (is.na(got) || got != tok) {
    stop(paste0("expected `", tok, "` but found `",
                ifelse(is.na(got), "end of expression", got), "`"),
         call. = FALSE)
  }
  invisible(got)
}

#' @keywords internal
#' @noRd
.is_ident <- function(tok) {
  !is.na(tok) && grepl("^[A-Za-z_]", tok) && !grepl("^\"", tok)
}

#' @keywords internal
#' @noRd
.coeff_node <- function(text) {
  list(kind = "coeff", text = text, terms = NULL)
}

#' @keywords internal
#' @noRd
.linear_node <- function(terms) {
  list(kind = "linear", text = NULL, terms = terms)
}

#' @keywords internal
#' @noRd
.new_term <- function(sign = 1L,
                      quants = list(),
                      fac = character(),
                      ops = character(),
                      var = NULL) {
  list(sign = sign, quants = quants, fac = fac, ops = ops, var = var)
}

#' @keywords internal
#' @noRd
.negate_terms <- function(terms) {
  lapply(terms, function(t) {
    t$sign <- -t$sign
    t
  })
}

#' @keywords internal
#' @noRd
.coerce_terms <- function(node) {
  if (node$kind %=% "linear") {
    return(node$terms)
  }
  list(.new_term(fac = node$text, ops = "*"))
}

# expr := ['+'|'-'] term (('+'|'-') term)*
#' @keywords internal
#' @noRd
.pe_expr <- function(st, var_lookup) {
  nodes <- list()
  signs <- integer()

  repeat {
    sign <- 1L
    while (.pk(st) %in% c("+", "-")) {
      if (.adv(st) %=% "-") {
        sign <- -sign
      }
    }
    node <- .pe_term(st, var_lookup)
    nodes[[length(nodes) + 1L]] <- node
    signs[[length(signs) + 1L]] <- sign
    if (.pk(st) %in% c("+", "-")) {
      next
    }
    break
  }

  if (all(purrr::map_chr(nodes, "kind") == "coeff")) {
    text <- ""
    for (n in seq_along(nodes)) {
      joint <- if (n == 1L) {
        ifelse(signs[[n]] == 1L, "", "-")
      } else {
        ifelse(signs[[n]] == 1L, " + ", " - ")
      }
      text <- paste0(text, joint, nodes[[n]]$text)
    }
    return(.coeff_node(text))
  }

  terms <- list()
  for (n in seq_along(nodes)) {
    new <- .coerce_terms(nodes[[n]])
    if (signs[[n]] == -1L) {
      new <- .negate_terms(new)
    }
    terms <- c(terms, new)
  }
  return(.linear_node(terms))
}

# term := factor (('*'|'/') factor)*
#' @keywords internal
#' @noRd
.pe_term <- function(st, var_lookup) {
  factors <- list(.pe_factor(st, var_lookup))
  ops <- "*"

  while (.pk(st) %in% c("*", "/")) {
    ops <- c(ops, .adv(st))
    factors[[length(factors) + 1L]] <- .pe_factor(st, var_lookup)
  }

  linear_at <- which(purrr::map_chr(factors, "kind") == "linear")

  if (length(linear_at) == 0L) {
    text <- factors[[1]]$text
    for (f in seq_along(factors)[-1]) {
      text <- paste0(text, ops[[f]], factors[[f]]$text)
    }
    return(.coeff_node(text))
  }

  if (length(linear_at) > 1L) {
    stop("product of two variable-bearing expressions (nonlinear)",
         call. = FALSE)
  }

  if (ops[[linear_at]] %=% "/") {
    stop("division by a variable-bearing expression (nonlinear)",
         call. = FALSE)
  }

  terms <- factors[[linear_at]]$terms
  for (f in seq_along(factors)[-linear_at]) {
    terms <- lapply(terms, function(t) {
      t$fac <- c(t$fac, factors[[f]]$text)
      t$ops <- c(t$ops, ops[[f]])
      t
    })
  }
  return(.linear_node(terms))
}

# factor := ['+'|'-'] (NUMBER | ELEMENT | ref | sum | '(' expr ')' | '[' expr ']')
#' @keywords internal
#' @noRd
.pe_factor <- function(st, var_lookup) {
  tok <- .pk(st)

  if (tok %in% c("+", "-")) {
    .adv(st)
    node <- .pe_factor(st, var_lookup)
    if (tok %=% "-") {
      if (node$kind %=% "coeff") {
        node$text <- paste0("-", node$text)
      } else {
        node$terms <- .negate_terms(node$terms)
      }
    }
    return(node)
  }

  if (tok %in% c("(", "[")) {
    .adv(st)
    close <- ifelse(tok %=% "(", ")", "]")
    node <- .pe_expr(st, var_lookup)
    .expect(st, close)
    if (node$kind %=% "coeff") {
      node$text <- paste0(tok, node$text, close)
    }
    return(node)
  }

  if (is.na(tok)) {
    stop("unexpected end of expression", call. = FALSE)
  }

  if (grepl("^\"", tok) || grepl("^[0-9.]", tok)) {
    .adv(st)
    return(.coeff_node(tok))
  }

  if (!.is_ident(tok)) {
    stop(paste0("unexpected token `", tok, "`"), call. = FALSE)
  }

  .adv(st)

  if (tolower(tok) %=% "sum" && .pk(st) %in% c("{", "(")) {
    open <- .adv(st)
    close <- ifelse(open %=% "{", "}", ")")
    idx <- .adv(st)
    if (!.is_ident(idx)) {
      stop("malformed sum index", call. = FALSE)
    }
    .expect(st, ",")
    set <- .adv(st)
    if (!.is_ident(set)) {
      stop("malformed sum set", call. = FALSE)
    }
    .expect(st, ",")
    node <- .pe_expr(st, var_lookup)
    .expect(st, close)
    if (node$kind %=% "coeff") {
      return(.coeff_node(paste0("sum{", idx, ",", set, ", ", node$text, "}")))
    }
    node$terms <- lapply(node$terms, function(t) {
      t$quants <- c(list(list(idx = idx, set = set)), t$quants)
      t
    })
    return(node)
  }

  args <- NULL
  if (.pk(st) %=% "(") {
    .adv(st)
    args <- .pe_args(st)
  }

  canonical <- var_lookup[[tolower(tok)]]
  if (!is.null(canonical)) {
    return(.linear_node(list(.new_term(
      var = list(name = canonical, args = args %|||% character())
    ))))
  }

  if (is.null(args)) {
    return(.coeff_node(tok))
  }

  for (a in args) {
    arg_idents <- .expr_idents(a)
    if (any(tolower(arg_idents) %in% names(var_lookup))) {
      stop(paste0("variable reference inside the arguments of `", tok, "`"),
           call. = FALSE)
    }
  }
  return(.coeff_node(paste0(tok, "(", paste(args, collapse = ","), ")")))
}

# Arguments of a reference: raw texts split on depth-1 commas.
#' @keywords internal
#' @noRd
.pe_args <- function(st) {
  args <- character()
  current <- character()
  depth <- 1L
  repeat {
    tok <- .adv(st)
    if (is.na(tok)) {
      stop("unbalanced parentheses in a reference", call. = FALSE)
    }
    if (tok %in% c("(", "[", "{")) {
      depth <- depth + 1L
    } else if (tok %in% c(")", "]", "}")) {
      depth <- depth - 1L
      if (depth == 0L) {
        args <- c(args, paste(current, collapse = ""))
        return(args)
      }
    } else if (tok %=% "," && depth == 1L) {
      args <- c(args, paste(current, collapse = ""))
      current <- character()
      next
    }
    current <- c(current, tok)
  }
}

# All identifier tokens in an expression text, quoted elements excluded.
#' @keywords internal
#' @noRd
.expr_idents <- function(text) {
  tokens <- regmatches(
    text,
    gregexpr("\"[^\"]*\"|[A-Za-z_][A-Za-z0-9_]*", text, perl = TRUE)
  )[[1]]
  tokens[!grepl("^\"", tokens)]
}

# Rename identifier tokens per `map` (named character: old -> new),
# leaving quoted elements untouched. Single simultaneous pass, so
# swap-type maps are safe.
#' @keywords internal
#' @noRd
.rename_expr_tokens <- function(text, map) {
  if (length(map) == 0L || !nzchar(text)) {
    return(text)
  }
  pattern <- "\"[^\"]*\"|[A-Za-z_][A-Za-z0-9_]*"
  m <- gregexpr(pattern, text, perl = TRUE)
  tokens <- regmatches(text, m)[[1]]
  hit <- !grepl("^\"", tokens) & tokens %in% names(map)
  tokens[hit] <- map[tokens[hit]]
  regmatches(text, m)[[1]] <- tokens
  return(text)
}

#' @keywords internal
#' @noRd
.rename_term <- function(term, map) {
  term$fac <- purrr::map_chr(term$fac, .rename_expr_tokens, map = map)
  term$quants <- lapply(term$quants, function(q) {
    if (q$idx %in% names(map)) {
      q$idx <- unname(map[[q$idx]])
    }
    q
  })
  if (!is.null(term$var) && length(term$var$args) > 0L) {
    term$var$args <- purrr::map_chr(term$var$args, .rename_expr_tokens, map = map)
  }
  term
}

#' @keywords internal
#' @noRd
.serialize_term <- function(term) {
  prod <- ""
  if (length(term$fac) > 0L) {
    prod <- term$fac[[1]]
    for (f in seq_along(term$fac)[-1]) {
      prod <- paste0(prod, term$ops[[f]], term$fac[[f]])
    }
  }
  if (!is.null(term$var)) {
    var_text <- term$var$name
    if (length(term$var$args) > 0L) {
      var_text <- paste0(var_text, "(", paste(term$var$args, collapse = ","), ")")
    }
    prod <- ifelse(nzchar(prod), paste0(prod, "*", var_text), var_text)
  }
  if (!nzchar(prod)) {
    prod <- "1"
  }
  for (q in rev(term$quants)) {
    prod <- paste0("sum{", q$idx, ",", q$set, ", ", prod, "}")
  }
  return(prod)
}

#' @keywords internal
#' @noRd
.serialize_linear <- function(terms) {
  if (length(terms) == 0L) {
    return("0")
  }
  text <- ""
  for (n in seq_along(terms)) {
    joint <- if (n == 1L) {
      ifelse(terms[[n]]$sign == 1L, "", "-")
    } else {
      ifelse(terms[[n]]$sign == 1L, " + ", " - ")
    }
    text <- paste0(text, joint, .serialize_term(terms[[n]]))
  }
  return(text)
}

# Parse one side of a linearized equation into flattened terms.
# `var_lookup` is a named list: tolower(variable name) -> canonical name.
# Errors are signalled with `stop()`; callers translate to cli aborts.
#' @keywords internal
#' @noRd
.parse_linear_side <- function(text, var_lookup) {
  st <- new.env(parent = emptyenv())
  st$tokens <- .tokenize_expr(text)
  st$pos <- 1L
  if (length(st$tokens) == 0L) {
    return(list())
  }
  node <- .pe_expr(st, var_lookup)
  if (st$pos <= length(st$tokens)) {
    stop(paste0("trailing tokens starting at `", .pk(st), "`"), call. = FALSE)
  }
  .coerce_terms(node)
}

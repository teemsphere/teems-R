#' GEMPACK set expressions (manual 10.1.1.1): named sets, quoted single
#' elements, '(' ')' grouping, and the operators UNION, INTERSECT, '+',
#' '-' and '\' (a synonym of '-'), applied left to right. UNION is
#' normalized to '^' and INTERSECT to '&'. NB keyword normalization is
#' substring-based (matching the solver's parser), so set names must
#' not contain "union" or "intersect".
#'
#' @keywords internal
#' @noRd
.set_expr_tokens <- function(d) {
  d <- gsub("\\\\", "-", d)
  d <- gsub("union", " ^ ", d, ignore.case = TRUE)
  d <- gsub("intersect", " & ", d, ignore.case = TRUE)
  m <- gregexpr('"[^"]*"|[()+^&-]|[^()+^&"[:space:]-]+', d)[[1]]
  if (m[1] %=% -1L) {
    return(character(0))
  }
  regmatches(d, list(m))[[1]]
}

#' Does a set definition hold an expression (as opposed to an explicit
#' element list)? Expressions arrive with their leading "=" preserved.
#'
#' @keywords internal
#' @noRd
.is_set_expr <- function(d) {
  !is.na(d) & grepl("^\\s*=", d)
}

#' Structural facts about an expression used for the manual's implied-
#' SUBSET rules: every named term, whether all operators are UNION/'+'
#' (or all INTERSECT), and the last top-level operator and term.
#'
#' @keywords internal
#' @noRd
.set_expr_info <- function(d) {
  toks <- .set_expr_tokens(d)
  is_op <- toks %in% c("+", "-", "^", "&")
  is_paren <- toks %in% c("(", ")")
  is_quote <- grepl('^"', toks)
  named <- toks[!is_op & !is_paren & !is_quote]
  ops <- toks[is_op]
  depth <- cumsum((toks == "(") - (toks == ")"))
  top_op_idx <- which(is_op & depth == 0)
  last_top_op <- if (length(top_op_idx)) toks[max(top_op_idx)] else NA_character_
  last_term <- NA_character_
  if (length(top_op_idx)) {
    after <- toks[seq(max(top_op_idx) + 1L, length(toks))]
    if (length(after) %=% 1L && !after %in% c("(", ")") && !grepl('^"', after)) {
      last_term <- after
    }
  }
  list(
    named = named,
    ops = ops,
    all_plus_union = length(ops) > 0 && all(ops %in% c("+", "^")),
    all_intersect = length(ops) > 0 && all(ops %=% "&"),
    simple_complement = length(ops) %=% 1L && ops[1] %=% "-" &&
      length(named) %=% 2L && !any(is_paren) && !any(is_quote),
    last_top_op = last_top_op,
    last_term = last_term
  )
}

#' Evaluate a set expression against resolved mappings (data.tables with
#' origin/mapping columns). Returns NULL when a referenced set is not
#' resolved yet (the caller's fixed-point loop retries). Validity per
#' the manual: '+' operands must be disjoint; '-' may only remove
#' elements that are present.
#'
#' @importFrom data.table data.table funion fsetdiff fintersect
#'
#' @keywords internal
#' @noRd
.eval_set_expr <- function(d, mappings, owner, call) {
  if (length(d) %!=% 1L || is.na(d)) {
    return(NULL)
  }
  toks <- .set_expr_tokens(d)
  pos <- 1L
  ready <- TRUE

  peek <- function() {
    if (pos <= length(toks)) toks[pos] else NA_character_
  }

  term <- function() {
    tk <- peek()
    if (is.na(tk)) {
      return(NULL)
    }
    if (tk %=% "(") {
      pos <<- pos + 1L
      v <- expr()
      if (isTRUE(peek() %=% ")")) pos <<- pos + 1L
      return(v)
    }
    pos <<- pos + 1L
    if (grepl('^"', tk)) {
      el <- tolower(gsub('"', "", tk))
      return(data.table::data.table(
        origin = el,
        mapping = el,
        key = c("origin", "mapping")
      ))
    }
    v <- mappings[[tk]]
    if (is.null(v)) ready <<- FALSE
    v
  }

  expr <- function() {
    acc <- term()
    while (isTRUE(peek() %in% c("+", "-", "^", "&"))) {
      op <- peek()
      pos <<- pos + 1L
      rhs <- term()
      if (!ready || is.null(acc) || is.null(rhs)) {
        ready <<- FALSE
        return(NULL)
      }
      if (op %=% "+") {
        overlap <- data.table::fintersect(acc, rhs)
        if (nrow(overlap) %!=% 0L) {
          d <- unique(overlap$mapping)
          .cli_action(deploy_err$invalid_plus,
            action = "abort",
            call = call
          )
        }
        acc <- data.table::funion(acc, rhs)
      } else if (op %=% "-") {
        missing_ele <- data.table::fsetdiff(rhs, acc, all = FALSE)
        if (nrow(missing_ele) %!=% 0L) {
          d <- unique(missing_ele$mapping)
          .cli_action(deploy_err$invalid_minus,
            action = "abort",
            call = call
          )
        }
        acc <- data.table::fsetdiff(acc, rhs, all = TRUE)
      } else if (op %=% "^") {
        acc <- data.table::funion(acc, rhs)
      } else {
        acc <- data.table::fintersect(acc, rhs)
      }
    }
    acc
  }

  out <- expr()
  if (!ready) {
    return(NULL)
  }
  out
}

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
  # set-equality definitions arrive with their leading "=" preserved
  d <- sub("^\\s*=\\s*", "", d)
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
#' the manual, at ELEMENT level throughout: '+' operands must be
#' disjoint; '-' may only remove elements that are present; '&' keeps
#' the accumulator's rows and aborts when a shared element's origin
#' coverage disagrees between the operands (ambiguous aggregation).
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
        # disjointness is an element-level requirement (manual
        # 10.1.1.1): a shared element with disjoint origin rows used
        # to slip past the row-level overlap test
        d <- intersect(unique(acc$mapping), unique(rhs$mapping))
        if (length(d) %!=% 0L) {
          .cli_action(deploy_err$invalid_plus,
            action = "abort",
            call = call
          )
        }
        acc <- data.table::funion(acc, rhs)
      } else if (op %=% "-") {
        # GEMPACK set operations act element-level on the (aggregated)
        # sets (manual 10.1.1.1): a subtracted element disappears
        # entirely, including every origin row that maps to it. A
        # row-level fsetdiff kept an aggregated element whenever any
        # origin outside the subtrahend mapped to it (e.g. NMRG =
        # COMM - MARG retained the margin commodity).
        rhs_ele <- unique(rhs$mapping)
        missing_ele <- setdiff(rhs_ele, unique(acc$mapping))
        if (length(missing_ele) %!=% 0L) {
          d <- missing_ele
          .cli_action(deploy_err$invalid_minus,
            action = "abort",
            call = call
          )
        }
        acc <- acc[!acc$mapping %in% rhs_ele, ]
      } else if (op %=% "^") {
        acc <- data.table::funion(acc, rhs)
      } else {
        # element-level intersection (manual 10.1.1.1): keep the
        # accumulator's rows for every element present in both
        # operands. A shared element whose origin coverage DISAGREES
        # between the operands is ambiguous under aggregation (whose
        # origins feed the data build?) -- abort rather than prefer
        # one side; the old row-level fintersect dropped such
        # elements from the result outright.
        shared <- intersect(unique(acc$mapping), unique(rhs$mapping))
        acc_sh <- acc[acc$mapping %in% shared, ]
        if (length(shared) %!=% 0L) {
          rhs_sh <- rhs[rhs$mapping %in% shared, ]
          acc_or <- lapply(split(acc_sh$origin, acc_sh$mapping), unique)
          rhs_or <- lapply(split(rhs_sh$origin, rhs_sh$mapping), unique)
          agree <- mapply(setequal, acc_or, rhs_or[names(acc_or)])
          if (!all(agree)) {
            d <- names(acc_or)[!agree]
            .cli_action(deploy_err$invalid_intersect,
              action = c("abort", "inform"),
              call = call
            )
          }
        }
        acc <- acc_sh
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

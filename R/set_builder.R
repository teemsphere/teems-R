#' Conditional set builders (GEMPACK manual 10.1.2)
#'
#' `Set NAME = (all,i,SRC: <cond>);` selects the elements of SRC for
#' which the DATA-dependent condition holds. The solver evaluates it
#' from the deployed input files ahead of set resolution
#' (teems-solver cmf_io.c tab_setbuilder_transform) and rewrites the
#' statement into an explicit list plus `Subset NAME is subset of
#' SRC`. teems keeps the statement verbatim in the deployed TAB and
#' mirrors the evaluation at deploy from the same aggregated tables,
#' so closure/shock validation, system squareness and compose see the
#' elements. Accepted condition shapes (the solver's):
#'   COEF(i) <op> <const>                        GDYN/gtapep SLUG
#'   COEF(i,"ele") / COEF("ele",i) <op> <const>  GTAPv7 ENDOWFLAG
#'   sum{j,S2: MAP(j) = i, COEF2(j)} <op> <const> GTAP-E UNITD* flags
#' with <op> one of = <> < > <= >= or eq ne lt gt le ge and <const> a
#' numeric literal. The condition operand must be a file-Read
#' coefficient (formula-computed operands cannot drive set resolution:
#' solver fatal, mirrored in .chk_tab_setbuilders).
#'
#' @keywords internal
#' @noRd
NULL

#' Parse a builder definition ("= (all,i,SRC: cond)" or the raw
#' statement remainder). Returns NULL when the shape is not a builder
#' the solver accepts, else a list with idx, src, cond, form
#' ("coef"/"mapsum"), op, const, and per-form fields: coef + args
#' (coef form); sum_idx, sum_set, map, coef (mapsum form).
#'
#' @keywords internal
#' @noRd
.parse_set_builder <- function(d) {
  d <- trimws(sub("^\\s*=\\s*", "", d))
  if (!startsWith(d, "(")) {
    return(NULL)
  }
  close <- .match_bracket(d, 1L)
  if (is.na(close) || nzchar(trimws(substring(d, close + 1L)))) {
    return(NULL)
  }
  inner <- substr(d, 2L, close - 1L)
  m <- regmatches(inner, regexec(
    "^\\s*[Aa][Ll][Ll]\\s*,\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*,\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*:(.*)$",
    inner
  ))[[1]]
  if (length(m) == 0L) {
    return(NULL)
  }
  idx <- m[2]
  src <- m[3]
  cond <- trimws(m[4])

  # comparison at bracket depth 0; symbol ops or the word spellings
  scan <- .tab_scan(cond)
  chs <- scan$chs
  top <- scan$depth_before == 0L & !scan$in_quote
  op_at <- NA_integer_
  op_len <- 0L
  n <- length(chs)
  i <- 1L
  while (i <= n) {
    if (top[i]) {
      c1 <- chs[i]
      c2 <- if (i < n) chs[i + 1L] else ""
      if (c1 %in% c("<", ">")) {
        op_at <- i
        op_len <- if (c2 %in% c("=", ">")) 2L else 1L
        break
      }
      if (c1 == "=") {
        op_at <- i
        op_len <- 1L
        break
      }
      if (c1 == " " && i + 3L <= n &&
        tolower(paste0(chs[i + 1L], chs[i + 2L])) %in%
          c("ne", "eq", "gt", "lt", "ge", "le") &&
        chs[i + 3L] == " ") {
        op_at <- i + 1L
        op_len <- 2L
        break
      }
    }
    i <- i + 1L
  }
  if (is.na(op_at)) {
    return(NULL)
  }
  op <- tolower(substr(cond, op_at, op_at + op_len - 1L))
  ops <- c(
    "=" = "eq", "<>" = "ne", ">" = "gt", "<" = "lt", ">=" = "ge", "<=" = "le",
    eq = "eq", ne = "ne", gt = "gt", lt = "lt", ge = "ge", le = "le"
  )
  if (!op %in% names(ops)) {
    return(NULL)
  }
  const <- trimws(substring(cond, op_at + op_len))
  if (!grepl("^[-+]?([0-9]+\\.?[0-9]*|\\.[0-9]+)([eE][-+]?[0-9]+)?$", const)) {
    return(NULL)
  }
  operand <- trimws(substr(cond, 1L, op_at - 1L))
  out <- list(
    idx = idx, src = src, cond = cond,
    op = ops[[op]], const = as.numeric(const), operand = operand
  )

  if (grepl("^[Ss][Uu][Mm]\\s*[][({]", operand)) {
    # sum{j,S2: MAP(j) = i, COEF2(j)}
    open <- regexpr("[][({]", operand)
    cl <- .match_bracket(operand, open)
    if (is.na(cl) || nzchar(trimws(substring(operand, cl + 1L)))) {
      return(NULL)
    }
    body <- substr(operand, open + 1L, cl - 1L)
    sm <- regmatches(body, regexec(paste0(
      "^\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*,\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*:\\s*",
      "([A-Za-z_][A-Za-z0-9_]*)\\s*[[({]\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*[])}]\\s*=\\s*",
      "([A-Za-z_][A-Za-z0-9_]*)\\s*,\\s*",
      "([A-Za-z_][A-Za-z0-9_]*)\\s*[[({]\\s*([A-Za-z_][A-Za-z0-9_]*)\\s*[])}]\\s*$"
    ), body))[[1]]
    if (length(sm) == 0L) {
      return(NULL)
    }
    j <- sm[2]
    if (tolower(sm[5]) != tolower(j) || tolower(sm[6]) != tolower(idx) ||
      tolower(sm[8]) != tolower(j)) {
      return(NULL)
    }
    out$form <- "mapsum"
    out$sum_idx <- j
    out$sum_set <- sm[3]
    out$map <- sm[4]
    out$coef <- sm[7]
    return(out)
  }

  # COEF(args): the loop index once, quoted elements elsewhere
  cm <- regmatches(operand, regexec(
    "^([A-Za-z_][A-Za-z0-9_]*)\\s*[[({](.*)[])}]\\s*$",
    operand
  ))[[1]]
  if (length(cm) == 0L) {
    return(NULL)
  }
  args <- trimws(strsplit(cm[3], ",", fixed = TRUE)[[1]])
  if (length(args) == 0L || any(!nzchar(args))) {
    return(NULL)
  }
  is_idx <- tolower(args) == tolower(idx)
  is_quoted <- grepl('^"[^"]+"$', args)
  if (sum(is_idx) != 1L || !all(is_idx | is_quoted)) {
    return(NULL)
  }
  out$form <- "coef"
  out$coef <- cm[2]
  out$args <- args
  out$loop_dim <- which(is_idx)
  out
}

#' Is this parsed set definition a builder?
#'
#' @keywords internal
#' @noRd
.is_set_builder <- function(d) {
  length(d) == 1L && !is.na(d) &&
    grepl("^\\s*=\\s*\\(\\s*all\\s*,", d, ignore.case = TRUE)
}

#' @keywords internal
#' @noRd
.sb_op_test <- function(v, op, c) {
  switch(op,
    eq = v == c, ne = v != c, gt = v > c, lt = v < c, ge = v >= c, le = v <= c
  )
}

#' Evaluate a builder against the aggregated coefficient tables.
#'
#' Mirrors tab_setbuilder_transform: the condition value is the
#' coefficient's DEPLOYED value (rounded exactly as .finalize_data
#' writes it), the loop index must range over the coefficient's
#' dimension set exactly, and an empty selection is fatal. Returns
#' the origin/mapping table of the selected SRC elements (SRC order),
#' or NULL while SRC is not resolved yet.
#'
#' @param b parsed builder (.parse_set_builder)
#' @param owner set being defined
#' @param mappings named list of resolved set mappings (NULL = pending)
#' @param coeff_data named list of aggregated coefficient tables (by
#'   header)
#' @param coeff_extract Coefficient rows of the model
#'
#' @keywords internal
#' @noRd
.eval_set_builder <- function(b,
                              owner,
                              mappings,
                              coeff_data,
                              coeff_extract,
                              call) {
  src_map <- mappings[[b$src]]
  if (is.null(src_map)) {
    return(NULL)
  }
  src_ele <- unique(src_map$mapping)
  bad_set <- owner

  if (b$form == "mapsum") {
    .cli_action(deploy_err$set_builder_mapsum,
      action = c("abort", "inform"),
      call = call
    )
  }

  ci <- match(tolower(b$coef), tolower(coeff_extract$name))
  hdr <- if (is.na(ci)) NA_character_ else coeff_extract$header[ci]
  dt <- if (is.na(hdr)) NULL else coeff_data[[hdr]]
  cond_coef <- b$coef
  if (is.null(dt)) {
    .cli_action(deploy_err$set_builder_data,
      action = "abort",
      call = call
    )
  }
  cols <- setdiff(names(dt), "Value")
  if (length(cols) != length(b$args)) {
    n_args <- length(b$args)
    n_dims <- length(cols)
    .cli_action(deploy_err$set_builder_args,
      action = "abort",
      call = call
    )
  }

  # deployed value: .finalize_data rounds Reals to ndigits and casts
  # (integer) coefficients
  val <- dt$Value
  is_int <- !is.na(coeff_extract$qualifier_list[ci]) &&
    grepl("integer", coeff_extract$qualifier_list[ci], ignore.case = TRUE)
  if (is_int || rlang::is_integerish(val)) {
    val <- as.integer(val)
  } else {
    val <- round(val, .o_ndigits())
  }

  keep_rows <- rep(TRUE, nrow(dt))
  for (d in seq_along(cols)) {
    if (d == b$loop_dim) next
    ele <- tolower(gsub('"', "", b$args[d]))
    col_vals <- tolower(dt[[cols[d]]])
    if (!ele %in% col_vals) {
      bad_ele <- ele
      dim_set <- cols[d]
      .cli_action(deploy_err$set_builder_ele,
        action = "abort",
        call = call
      )
    }
    keep_rows <- keep_rows & col_vals == ele
  }
  loop_vals <- tolower(dt[[cols[b$loop_dim]]])
  loop_ele <- unique(loop_vals)
  if (!setequal(loop_ele, tolower(src_ele))) {
    loop_idx <- b$idx
    dim_set <- cols[b$loop_dim]
    src_set <- b$src
    .cli_action(deploy_err$set_builder_dim,
      action = "abort",
      call = call
    )
  }

  v <- val[keep_rows]
  e <- loop_vals[keep_rows]
  # dense tables carry one row per element; a missing row is a zero
  sel <- vapply(tolower(src_ele), function(x) {
    hit <- which(e == x)
    vv <- if (length(hit) == 0L) 0 else v[hit[1]]
    isTRUE(.sb_op_test(vv, b$op, b$const))
  }, logical(1))

  if (!any(sel)) {
    src_set <- b$src
    builder_cond <- b$cond
    .cli_action(deploy_err$set_builder_empty,
      action = "abort",
      call = call
    )
  }
  kept <- src_ele[sel]
  out <- src_map[src_map$mapping %in% kept, ]
  data.table::setattr(out, "origin_conflict", NULL)
  out
}

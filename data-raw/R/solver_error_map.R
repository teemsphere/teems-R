# Maps solver diagnostic-log "Error:" lines to abort classes.
# Patterns are regexes matched (case-insensitively) against each
# "Error:" line with the prefix stripped; the FIRST matching row wins,
# so closure/shock and data patterns that overlap the generic TAB
# wording sit above the TAB block. Sources: the solver fatal-error
# format strings in teems-solver src (tab_parse.c, cmf_io.c,
# formula.c, jacobian.c, main.c); the row inventory lives in
# dev/validation_table.md. The manual column is the GEMPACK manual
# section cited by the solver message, NA when none.
build_solver_error_map <- function() {
  rows <- list(
    # closure / shock files (closure_read wording: "(in <var>)";
    # shocks_read wording: "(shock file)")
    c("is not in set .* \\(in ", "closure", NA),
    c("is not declared \\(in ", "closure", NA),
    c("shock file", "closure", NA),
    c("closure file", "closure", NA),
    c("^variable [^ ]+ is not declared", "closure", NA),
    # data files
    c("header .* not found", "data", NA),
    c("not found in the data file", "data", NA),
    c("cannot open", "data", NA),
    # runtime numeric evaluation
    c("zero divided by zero", "numeric", "10.11.1"),
    c("division by zero in a formula", "numeric", "10.11.1"),
    c("assertion failed", "numeric", "25.3"),
    c("has a value (above|below) its declared", "numeric", "25.4.4"),
    c("fractional power of a negative number", "numeric", NA),
    c("zero pivot", "numeric", "14.1.10"),
    # names (names_validate)
    c("declared as both a", "tab", "11.2.1"),
    c("declared more than once", "tab", "11.2.1"),
    c("is a reserved word", "tab", "11.2.1"),
    c("prefix is reserved for change variables", "tab", NA),
    c("share a name \\(p_/c_ prefixes", "tab", NA),
    # declaration qualifiers (tab_qualifiers_parse)
    c("unknown (variable|coefficient) qualifier", "tab", "10.3"),
    c("qualifier NO_SPLIT", "tab", "10.3"),
    c("LINEAR_NAME=", "tab", "10.3"),
    c("empty qualifier", "tab", "10.3"),
    c("unbalanced parentheses in .* qualifier", "tab", "10.3"),
    # bounds + Default statements
    c("duplicate (lower|upper) bound", "tab", "10.19.1"),
    c("bound defaults are not supported", "tab", "10.19"),
    c("default=levels\\) is not supported", "tab", "10.19"),
    c("default=add_homotopy\\) is not supported", "tab", "10.19"),
    c("unknown (coefficient|variable|formula|equation) default", "tab", "10.19"),
    c("Default statements apply only", "tab", "10.19"),
    # PostSim sections
    c("PostSim", "tab", "12.2"),
    # sets
    c("references itself in a set expression", "tab", "10.1.1.1"),
    c("defined as equal to itself", "tab", "10.1.2.1"),
    c("set difference subtracts a larger set", "tab", NA),
    c("intertemporal set", "tab", NA),
    c("element range abbreviation", "tab", NA),
    c("more elements than", "tab", "10.1.1.1"),
    c("in the definition of", "tab", "10.1.1.1"),
    c("set .* is not declared", "tab", NA),
    c("not a declared set", "tab", "10.1.2.1"),
    c("malformed .*set declaration", "tab", NA),
    c("malformed element list", "tab", NA),
    c("negative size in TAB file", "tab", NA),
    c("elements of set .* are not in set", "tab", NA),
    # intrinsics / formula compilation
    c("POS function is not supported", "tab", "11.5.6"),
    c("takes exactly 2 arguments", "tab", "11.5"),
    c("takes at least 2 arguments", "tab", "11.5.1"),
    c("arguments? in an intrinsic function call", "tab", "11.5"),
    c("Formula & Equation", "tab", "10.9.1"),
    c("malformed formula", "tab", NA),
    c("malformed if\\(\\)", "tab", NA),
    c("formula too long to compile", "tab", NA),
    # reads
    c("Read without a header", "tab", "11.11.8"),
    c("from terminal is not supported", "tab", "10.6"),
    c("is not a declared variable, coefficient, or parameter", "tab", "10.6"),
    c("malformed (partial )?Read statement", "tab", "10.6"),
    # condensation / backsolve
    c("backsolv", "tab", "14.1"),
    c("must not reach the solver", "tab", NA)
  )
  map <- as.data.frame(
    do.call(rbind, rows),
    stringsAsFactors = FALSE
  )
  names(map) <- c("pattern", "class", "manual")
  map
}

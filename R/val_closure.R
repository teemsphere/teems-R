#' @importFrom purrr map
#' 
#' @keywords internal
#' @note Move to S3 to skip checks on internal closures
#' @noRd
.validate_closure <- function(closure,
                              var_omit,
                              swap_in,
                              swap_out,
                              shock,
                              shock_file,
                              sets,
                              var_extract,
                              call) {
  closure_file <- attr(closure, "file")
  closure <- subset(closure, !grepl("!", closure))
  temp <- gsub("\\([^)]*\\)", "", closure)

  closure <- unlist(purrr::map2(
    closure,
    temp,
    function(cls, t) {
      if (grepl("\\s", t)) {
        strsplit(t, " ")
      } else {
        cls
      }
    }
  ))

  closure <- subset(closure, !closure %in% var_omit)
  cls_var <- purrr::map_chr(strsplit(closure, "\\("), 1)

  if (!all(cls_var %in% var_extract$name)) {
    var_discrepancy <- setdiff(tolower(cls_var), tolower(var_extract$name))
    l_var <- length(var_discrepancy)
    .cli_action(cls_err$no_var,
      action = "abort",
      call = call
    )
  }
  closure <- .classify_cls(
    closure = closure,
    sets = sets
  )

  closure <- lapply(closure,
    .exp_cls_entry,
    var_extract = var_extract,
    sets = sets$ele,
    call = call
  )

  var_names <- purrr::map_chr(closure, attr, "var_name")
  multi_entry <- closure[duplicated(var_names) | duplicated(var_names, fromLast = TRUE)]
  for (e in unique(purrr::map_chr(multi_entry, attr, "var_name"))) {
    over_check <- multi_entry[purrr::map_chr(multi_entry, attr, "var_name") == e]
    over_check <- data.table::rbindlist(lapply(over_check, attr, "ele"))
    if (any(duplicated(over_check))) {
      overlap <- over_check[duplicated(over_check)]
      n_overlap <- nrow(overlap)
      overlap <- capture.output(print(overlap))[-c(1, 2)]
      .cli_action(swap_err$overlap_ele,
        action = "abort",
        call = call
      )
    }
  }

  attr(closure, "file") <- closure_file
  return(closure)
}
#' @importFrom tibble tibble
#' @importFrom purrr pluck pmap
#' 
#' @keywords internal
#' @noRd
.classify_cls <- function(closure,
                          sets) {

  #set_pattern <- paste(toupper(sets$name), collapse = "|")
  set_pattern <- paste(sets$name, collapse = "|")
  closure <- purrr::map(closure, function(c) {
    var_name <- strsplit(c, "\\(")[[1]][1]
    
    new_class <- if (!grepl("\\(|\"", c)) {
      c("full", class(c))
    } else if (grepl("\\(", c) && !grepl("\"", c)) {
      c("subset", class(c))
    } else if (grepl("\"", c) && grepl(set_pattern, c)) {
      c("mixed", class(c))
    } else if (grepl("\"", c) && !grepl(set_pattern, c)) {
      c("ele", class(c))
    } else {
      NA
    }
    
    structure(c,
              var_name = var_name,
              class = new_class)
  })
  
  if (any(is.na(purrr::map(closure, inherits, "NA")))) {
    invalid_entry <- closure[is.na(purrr::map(closure, attr, "type"))]
    .cli_action(cls_err$entry_type,
                action = "abort",
                call = call)
  }

  return(closure)
}

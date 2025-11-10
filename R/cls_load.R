#' @importFrom purrr map_chr
#' 
#' @keywords internal
#' @noRd
.load_closure <- function(closure_file,
                          tab_file,
                          call) {

  if (is.null(closure_file)) {
    if (inherits(tab_file, "internal")) {
    closure <- internal_cls[[tab_file]]
    closure_file <- paste0(tab_file, ".cls")
    } else {
      .cli_action(cls_err$no_cls,
                  action = "abort",
                  call = call)
    }
  } else {
    closure <- readLines(closure_file)
    closure_file <- basename(closure_file)
  }

  closure <- gsub(";", "", closure)
  closure <- closure[closure != ""]
  
  if (!any(grepl("exogenous", closure[[1]], ignore.case = TRUE)) ||
      !any(grepl("rest endogenous", closure[[length(closure)]], ignore.case = TRUE))) {
    # add examples
    .cli_action(cls_err$missing_specification,
                action = "abort",
                call = call)
  }

  closure <- sub("exogenous", "", closure, ignore.case = TRUE)
  closure <- sub("rest endogenous", "", closure, ignore.case = TRUE)

  closure <- trimws(closure)
  closure <- closure[closure != ""]
  attr(closure, "file") <- closure_file
  return(closure)
}
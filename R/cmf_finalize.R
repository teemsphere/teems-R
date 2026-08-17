#' @importFrom purrr map_chr
#'
#' @keywords internal
#' @noRd
.finalize_cmf <- function(model,
                          shock_file,
                          tab_file,
                          cls_file,
                          write_coefficients = FALSE) {

  write_dir   <- normalizePath(.o_tempdir(), "/")
  input_names <- paste0(unique(model[!is.na(model$file), "file"][[1]]))
  input_files <- file.path(write_dir, paste0(input_names, ".txt"))
  names(input_files) <- input_names

  tab_path <- file.path(write_dir, tab_file)
  cls_path <- file.path(write_dir, cls_file)
  cmf_path <- sub("\\.tab", "\\.cmf", tab_path)
  shf_path <- file.path(write_dir, shock_file)
  
  cmf_comp <- c(
    paste("tabfile", paste0("\"", tab_path, "\"", ";")),
    paste("closure", paste0("\"", cls_path, "\"", ";")),
    paste("shock", paste0("\"", shf_path, "\"", ";"))
  )

  writeout <- .writeout(model = model,
                        write_dir = write_dir,
                        coeff = write_coefficients)

  input_data <- paste(
    "iodata",
    paste0('"', names(input_files), '"'),
    paste0('"', input_files, '";')
  )

  var_output <- paste("soldata", '"SolFiles"', paste0(
    '"',
    file.path(write_dir, "out", "variables", "bin", "sol"),
    '";'
  ))

  cmf <- c(
    input_data,
    cmf_comp,
    var_output,
    writeout
  )

  cmf <- purrr::map_chr(cmf, function(c) {
    gsub(write_dir, "/opt/teems", c, fixed = TRUE)
  })

  attr(cmf, "write_path") <- cmf_path
  class(cmf) <- c("cmf", class(cmf))

  return(cmf)
}
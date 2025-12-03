#' @keywords internal
#' @noRd
.ems_write <- function(input,
                       ...) {
  UseMethod(".ems_write")
}

#' @keywords internal
#' @noRd
#' @method .ems_write dat
#' @export
.ems_write.dat <- function(input,
                           write_dir,
                           ...) {
  .ragged_write(
    input = input,
    write_dir = write_dir
  )
}

#' @keywords internal
#' @noRd
#' @method .ems_write par
#' @export
.ems_write.par <- function(input,
                           write_dir,
                           ...) {
  .ragged_write(
    input = input,
    write_dir = write_dir
  )
}

#' @keywords internal
#' @noRd
#' @method .ems_write set
#' @export
.ems_write.set <- function(input,
                           write_dir,
                           ...) {
  write_path <- file.path(write_dir, paste0(attr(input, "file"), ".txt"))

  cat(attr(input, "lead"),
    file = write_path,
    sep = "\n",
    append = TRUE
  )

  cat(input,
    file = write_path,
    sep = "\n",
    append = TRUE
  )

  cat("\n",
    file = write_path,
    sep = "",
    append = TRUE
  )
  return(write_path)
}

#' @keywords internal
#' @noRd
#' @method .ems_write tab
#' @export
.ems_write.tab <- function(input,
                           write_dir,
                           ...) {
  write_path <- file.path(write_dir, attr(input, "file"))
  writeLines(input, write_path)
  return(write_path)
}

#' @keywords internal
#' @noRd
#' @method .ems_write closure
#' @export
.ems_write.closure <- function(input,
                               write_dir,
                               ...) {
  write_path <- file.path(write_dir, attr(input, "file"))

  input <- .format_closure(
    closure = input
  )

  writeLines(
    text = input,
    con = write_path
  )

  return(write_path)
}

#' @keywords internal
#' @noRd
#' @method .ems_write cmf
#' @export
.ems_write.cmf <- function(input,
                           ...) {
  write_path <- attr(input, "write_path")
  writeLines(input,
    con = write_path
  )
  class(write_path) <- "cmf"
  return(write_path)
}

#' @keywords internal
#' @noRd
#' @method .ems_write shock
#' @export
.ems_write.shock <- function(input,
                             write_dir,
                             ...) {

  write_path <- file.path(write_dir, attr(input, "file"))
    for (shk in input) {
      .write_shock(
        shock = shk,
        write_path = write_path
      )
    }
  
  return(write_path)
}

#' @keywords internal
#' @noRd
#' @method .ems_write list
#' @export
.ems_write.list <- function(input,
                            write_dir,
                            ...) {
  
  write_path <- file.path(write_dir, attr(input, "file"))
  saveRDS(input, write_path)
  
  return(write_path)
}

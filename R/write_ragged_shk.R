#' @importFrom data.table dcast fwrite setorder
#' @importFrom utils head
#'
#' @keywords internal
#' @noRd
.shk_ragged_write <- function(input,
                              write_dir,
                              lead,
                              dim_sizes,
                              write_path) {
 
  idx <- length(colnames(input))

  if (is.null(lead)) {
    cat(attr(input, "lead"),
      file = write_path,
      sep = "\n",
      append = TRUE
    )
  } else {
    cat(lead,
      file = write_path,
      sep = "\n",
      append = TRUE
    )
  }

  if (idx %=% 1L) {
    data.table::fwrite(
      x = input,
      file = write_path,
      quote = FALSE,
      append = TRUE,
      sep = " "
    )
  } else if (idx %=% 2L) {
    data.table::setorder(input)
    data.table::fwrite(
      x = input[, -1],
      file = write_path,
      quote = FALSE,
      append = TRUE,
      sep = " "
    )

    cat(";\n",
      file = write_path,
      sep = "",
      append = TRUE
    )
  } else if (idx %=% 3L) {
    input <- data.table::dcast(input,
                               paste(colnames(input)[2:1], collapse = "~"),
                               value.var = "Value")
    data.table::fwrite(
      x = input[, -1],
      file = write_path,
      quote = FALSE,
      append = TRUE,
      sep = " "
    )
    cat(";\n",
      file = write_path,
      sep = "",
      append = TRUE
    )
  } else {
    input <- input[, c(rev(utils::head(seq_len(idx), -1)), idx), with = FALSE]
    data.table::setorder(input)
    arr <- array(input[[idx]], dim_sizes)

    ls_dt <- .slice_array(
      arr = arr,
      dim_sizes = dim_sizes
    )

    lapply(
      seq_along(ls_dt),
      function(i) {
        data.table::fwrite(
          ls_dt[[i]],
          file = write_path,
          sep = " ",
          col.names = FALSE,
          append = TRUE
        )

        if (i == length(ls_dt)) {
          cat(";\n",
            file = write_path,
            sep = "",
            append = TRUE
          )
        }
        cat("\n",
          file = write_path,
          sep = "",
          append = TRUE
        )
      }
    )
  }


  if (idx < 4) {
    cat("\n",
      file = write_path,
      sep = "",
      append = TRUE
    )
  }
  return(write_path)
}
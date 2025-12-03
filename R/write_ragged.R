#' @importFrom data.table fwrite setorder
#' @importFrom utils head
#'
#' @keywords internal
#' @noRd
.ragged_write <- function(input,
                          write_dir) {
  write_path <- file.path(write_dir, paste0(attr(input, "file"), ".txt"))
  dim_sizes <- attr(input, "dim_sizes")
  idx <- length(colnames(input))

  cat(attr(input, "lead"),
    file = write_path,
    sep = "\n",
    append = TRUE
  )

  if (idx %=% 1L) {
    data.table::fwrite(
      x = input,
      file = write_path,
      quote = FALSE,
      append = TRUE
    )
  } else if (idx %=% 2L) {
    data.table::setorder(input)
    data.table::fwrite(
      x = input[, -1],
      file = write_path,
      quote = FALSE,
      append = TRUE
    )
  } else if (idx %=% 3L) {
    input <- input[, c(rev(utils::head(seq_len(idx), -1)), idx), with = FALSE]
    data.table::setorder(input)
    arr <- array(input$Value, dim_sizes)

    data.table::fwrite(
      x = as.data.table(arr),
      file = write_path,
      col.names = FALSE,
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
      ls_dt,
      function(dt) {
        data.table::fwrite(
          dt,
          file = write_path,
          col.names = FALSE,
          append = TRUE
        )
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
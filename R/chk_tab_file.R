#' @keywords internal
#' @noRd
.check_tab_file <- function(tab_file,
                            call) {
  tab <- readChar(
    tab_file,
    file.info(tab_file)[["size"]],
    useBytes = TRUE
  )
  tab <- .tab_to_utf8(tab)

  statements <- .check_statements(
    tab = tab,
    call = call
  )

  return(statements)
}

#' Normalise TAB text to valid UTF-8
#'
#' TAB files from older toolchains are frequently Latin-1 (accented
#' characters inside comments/labels) and CRLF-terminated; a byte-read
#' string with invalid UTF-8 crashes the perl regex splits downstream.
#' A UTF-8 BOM is dropped; text that is not valid UTF-8 is re-encoded
#' from Latin-1 (every byte sequence is valid Latin-1, so this cannot
#' fail; unmappable bytes are dropped).
#'
#' @keywords internal
#' @noRd
.tab_to_utf8 <- function(tab) {
  tab <- sub("^\xEF\xBB\xBF", "", tab, useBytes = TRUE)
  if (!validUTF8(tab)) {
    tab <- iconv(tab, from = "latin1", to = "UTF-8", sub = "")
  }
  Encoding(tab) <- "UTF-8"
  tab
}

#' @keywords internal
#' @noRd
.read_input <- function(input,
                        data_type,
                        metadata = NULL,
                        attach_metadata = FALSE,
                        call = NULL) {
  UseMethod(".read_input")
}

#' @importFrom utils read.csv
#' @method .read_input csv
#' @export
#' @keywords internal
#' @noRd
.read_input.csv <- function(input,
                            data_type,
                            metadata = NULL,
                            attach_metadata = FALSE,
                            call = NULL) {
  input <- utils::read.csv(input)
  if (data_type %=% "set") {
    input <- input[[1]]
  }

  return(input)
}


#' @details Function modified from
#'   https://rdrr.io/github/USDA-ERS/MTED-HARr/src/R/read_har.r
#'
#' @importFrom purrr pluck
#' @method .read_input har
#' @export
#' @keywords internal
#' @noRd
.read_input.har <- function(input,
                            data_type,
                            metadata = NULL,
                            attach_metadata = FALSE,
                            call = NULL) {
  if (is.character(input)) {
    cf <- readBin(input, raw(), n = file.size(input))
  } else {
    # Read all bytes into a vector
    cf <- raw()
    while (length(a <- readBin(input, raw(), n = 1e9)) > 0) {
      cf <- c(cf, a)
    }
    close(input)
  }

  if (cf[1] == 0xfd) {
    currentHeader <- ""
    headers <- list()
    i <- 2
    while (i < length(cf)) {
      # read the first byte
      fb <- cf[i]
      i <- i + 1
      bitsLength <- as.integer(rawToBits(fb))[3:8]
      toRead <- as.integer(rawToBits(fb))[1:2]
      toReadBytes <- Reduce(\(a, f) {
        a <- a + 2^(f - 1) * toRead[f]
      }, 1:length(toRead), 0)

      if (toReadBytes > 0) {
        for (i in (i):(i + toReadBytes - 1)) {
          bitsLength <- c(bitsLength, rawToBits(cf[i]))
        }
        i <- i + 1
      }

      recordLength <- Reduce(
        \(a, f) {
          a <- a + 2^(f - 1) * bitsLength[f]
        },
        1:length(bitsLength),
        0
      )

      if (recordLength == 4) {
        currentHeader <- trimws(rawToChar(cf[(i):(i + recordLength - 1)]))
      }
      if (is.null(headers[[currentHeader]])) {
        headers[[currentHeader]] <- list()
      }

      if (is.null(headers[[currentHeader]]$records)) {
        headers[[currentHeader]]$records <- list()
      }

      headers[[currentHeader]]$records[[length(headers[[currentHeader]]$records) +
        1]] <- cf[(i):(i + recordLength - 1)]
      i <- i + recordLength
      totalLength <- recordLength + 1 + toReadBytes
      endingBits <- intToBits(totalLength)
      maxPosition <- max(which(endingBits == 1))

      if (maxPosition <= 6) {
        needEnd <- 0
      } else {
        needEnd <- 0 + ceiling((maxPosition - 6) / 8)
      }

      expectedEnd <- packBits(c(intToBits(needEnd)[1:2], intToBits(totalLength))[1:(8 *
        (needEnd + 1))], "raw")
      expectedEnd <- expectedEnd[length(expectedEnd):1]

      if (any(cf[i:(i + length(expectedEnd) - 1)] != expectedEnd)) {
        stop("Surprising end of record")
      }

      i <- i + length(expectedEnd)
    }
  } else {
    headers <- lapply(
      har_split_records(cf),
      function(r) list(records = r)
    )
  }

  # Process first and second records
  for (h in names(headers)) {
    headers[[h]]$header <- trimws(rawToChar(headers[[h]]$records[[1]][1:4]))
    headers[[h]]$type <- rawToChar(headers[[h]]$records[[2]][5:10])
    # headers[[h]]$label <- trimws(rawToChar(headers[[h]]$records[[2]][11:80]))
    headers[[h]]$numberOfDimensions <- readBin(headers[[h]]$records[[2]][81:84], "integer",
      size =
        4
    )

    headers[[h]]$dimensions <- c()

    for (i in 1:headers[[h]]$numberOfDimensions) {
      headers[[h]]$dimensions <- c(
        headers[[h]]$dimensions,
        readBin(headers[[h]]$records[[2]][(85 + (i - 1) * 4):(85 + i * 4)], "integer",
          size =
            4
        )
      )
    }
  }

  # Process character headers 1CFULL
  for (h in names(headers)) {
    if (headers[[h]]$type == "1CFULL") {
      contents <- har_payload_concat(
        headers[[h]]$records[3:length(headers[[h]]$records)],
        16L
      )

      # do not remove empty space in the history header
      # LREG in GTAP11 uses LATIN1 encoding
      if (tolower(h) == "xxhs") {
        toRet <- har_fixed_width_strings(contents, headers[[h]]$dimensions[[2]], FALSE)
      } else if (h == "LREG") {
        toRet <- trimws(iconv(
          har_fixed_width_strings(contents, headers[[h]]$dimensions[[2]], FALSE),
          from = "latin1",
          to = "UTF-8"
        ))
      } else {
        toRet <- har_fixed_width_strings(contents, headers[[h]]$dimensions[[2]], TRUE)
      }

      headers[[h]]$data <- toRet
    }
  }

  # Process character headers 2IFULL
  for (h in names(headers)) {
    if (headers[[h]]$type == "2IFULL") {
      m <- matrix(
        har_payload_i32(
          headers[[h]]$records[3:length(headers[[h]]$records)],
          32L,
          prod(headers[[h]]$dimensions)
        ),
        nrow =
          headers[[h]]$dimensions[[1]],
        ncol =
          headers[[h]]$dimensions[[2]]
      )
      headers[[h]]$data <- m
    }
  }

  # Process real headers 2RFULL
  for (h in names(headers)) {
    if (headers[[h]]$type == "2RFULL") {
      m <- array(
        har_payload_f32(
          headers[[h]]$records[3:length(headers[[h]]$records)],
          32L,
          prod(headers[[h]]$dimensions)
        ),
        dim = headers[[h]]$dimensions
      )
      headers[[h]]$data <- m
    }
  }

  # Process real  headers REFULL
  for (h in names(headers)) {
    if (headers[[h]]$type %in% c("REFULL", "RESPSE")) {
      # Get used dimensions and their names from record 3
      headers[[h]]$definedDimensions <- readBin(headers[[h]]$records[[3]][5:8], "integer",
        size =
          4
      )
      headers[[h]]$usedDimensions <- readBin(headers[[h]]$records[[3]][13:16], "integer",
        size =
          4
      )

      if (headers[[h]]$usedDimensions > 0) {
        dnames <- har_fixed_width_strings(
          headers[[h]]$records[[3]][33:(33 + headers[[h]]$usedDimensions * 12 - 1)],
          12L,
          FALSE
        )
        dimNames <- Map(\(f) {
          NULL
        }, 1:headers[[h]]$usedDimensions)
        actualDimsNamesFlags <- headers[[h]]$records[[3]][(33 + headers[[h]]$usedDimensions *
          12) + 0:6]
        actualDimsNames <- ifelse(actualDimsNamesFlags == 0x6b, TRUE, FALSE)
        uniqueDimNames <- unique(dnames[actualDimsNames])

        if (length(uniqueDimNames) > 0) {
          for (d in 1:length(uniqueDimNames)) {
            nele <- readBin(headers[[h]]$records[[3 + d]][13:16], "integer", size = 4)

            ele_names <- har_fixed_width_strings(
              headers[[h]]$records[[3 + d]][17:(17 + nele * 12 - 1)],
              12L,
              TRUE
            )

            for (dd in which(dnames == uniqueDimNames[d])) {
              dimNames[[dd]] <- ele_names
              # Add dimension name
              names(dimNames)[dd] <- trimws(uniqueDimNames[d])
            }
          }
        }

        dataStart <- 3 + length(uniqueDimNames) + 1

        if (headers[[h]]$type == "REFULL") {
          numberOfFrames <- readBin(headers[[h]]$records[[dataStart]][5:8], "integer")
          numberOfDataFrames <- (numberOfFrames - 1) / 2
          dataFrames <- (dataStart) + 1:numberOfDataFrames * 2

          m <- array(
            har_payload_f32(
              headers[[h]]$records[dataFrames],
              8L,
              prod(headers[[h]]$dimensions)
            ),
            dim = headers[[h]]$dimensions[1:headers[[h]]$usedDimensions],
            dimnames = dimNames
          )
        } else {
          dataVector <- har_spse_fill(
            headers[[h]]$records[(dataStart + 1):length(headers[[h]]$records)],
            16L,
            prod(headers[[h]]$dimensions)
          )

          m <- array(dataVector,
            dim = headers[[h]]$dimensions[1:headers[[h]]$usedDimensions],
            dimnames = dimNames
          )
        }
      } else {
        m <- array(
          readBin(
            headers[[h]]$records[[length(headers[[h]]$records)]][9:length(headers[[h]]$records[[3]])],
            "double",
            size = 4,
            n = prod(headers[[h]]$dimensions)
          ),
          dim = headers[[h]]$dimensions[1:headers[[h]]$usedDimensions]
        )
      }

      headers[[h]]$data <- m
    }
  }

  if (attach_metadata) {
    DREL <- purrr::pluck(headers, "DREL", "data")
    DVER <- purrr::pluck(headers, "DVER", "data")
    metadata <- .har_meta(
      DREL = DREL,
      DVER = DVER,
      data_type = data_type
    )

    metadata[["full_database_version"]] <- metadata[["database_version"]]
    metadata[["database_version"]] <- gsub("(\\d.*?)[A-Za-z]", "\\1", metadata[["database_version"]])
  }


  # manually pull out set names for pre v11
  # no telling how robust this is
  if (data_type %=% "set") {
    ranges <- c(
      H1 = 19, H2 = 25, H3 = 25, H4 = 25, H5 = 25,
      H6 = 25, H7 = 25, H8 = 25, H9 = 25, MARG = 25, TARS = 20
    )
    
    switch(metadata$database_version,
      "GTAPv9" = , # falls through to GTAPv10
      "GTAPv10" = {
        for (key in names(ranges)) {
          headers[[key]]$name <- trimws(rawToChar(headers[[key]]$records[[2]][14:ranges[key]]))
        }
      },
      "GTAPv11" = {
        headers <- lapply(headers, \(h) {
          h$name <- h$header
          return(h)
        })
      },
      "GTAPv12" = {
        headers <- lapply(headers, \(h) {
          h$name <- h$header
          return(h)
        })
      }
    )
  }

  headers <- lapply(
    headers,
    \(h) {
      header <- h$header
      name <- h$name
      .data <- h$data
      if (!is.null(.data)) {
        if (is.null(name)) {
          class(.data) <- c(header, data_type, metadata$data_format, class(.data))
        } else {
          class(.data) <- c(header, name, data_type, metadata$data_format, class(.data))
        }
      }
      return(.data)
    }
  )

  class(headers) <- c(data_type, class(headers))
  if (attach_metadata) {
    attr(headers, "metadata") <- metadata
  }

  return(headers)
}
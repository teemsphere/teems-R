#' @importFrom purrr pluck map
#' @importFrom data.table CJ setkey setnames fintersect
#' @importFrom utils type.convert
#' 
#' @keywords internal
#' @noRd
.exp_cls_entry <- function(cls_entry,
                           var_extract,
                           sets,
                           call) {
  UseMethod(".exp_cls_entry")
}

#' @method .exp_cls_entry full
#' @export
.exp_cls_entry.full <- function(cls_entry,
                                var_extract,
                                sets,
                                call) {
  var_sets <- purrr::pluck(var_extract, "ls_upper_idx", attr(cls_entry, "var_name"))

  if (!var_sets %=% NA) {
    exp_entry <- do.call(
      data.table::CJ,
      c(with(sets, mget(var_sets, ifnotfound = "")), sorted = FALSE)
    )
    data.table::setkey(exp_entry)
  } else {
    exp_entry <- NA
  }

  attr(cls_entry, "comp") <- colnames(exp_entry)
  attr(cls_entry, "ele") <- exp_entry
  return(cls_entry)
}

#' @method .exp_cls_entry ele
#' @export
.exp_cls_entry.ele <- function(cls_entry,
                               var_extract,
                               sets,
                               call) {
  entry_ele <- sub(")", "", purrr::pluck(strsplit(cls_entry, "\\("), 1, 2))
  entry_ele <- strsplit(entry_ele, ",")[[1]]
  var_sets <- purrr::pluck(var_extract, "ls_upper_idx", attr(cls_entry, "var_name"))

  entry_ele <- purrr::map(entry_ele, function(e) {
    utils::type.convert(gsub("\"", "", e), as.is = TRUE)
  })

  full_entry <- do.call(
    data.table::CJ,
    c(with(sets, mget(var_sets, ifnotfound = "")), sorted = FALSE)
  )

  entry_ele <- data.table::setnames(do.call(data.table::CJ, entry_ele), var_sets)
  data.table::setkey(entry_ele)
  if (!nrow(data.table::fintersect(entry_ele, full_entry)) %=% 1L) {
    .cli_action(cls_err$ele_invalid,
      action = "abort",
      call = call
    )
  }

  attr(cls_entry, "comp") <- unlist(entry_ele)
  attr(cls_entry, "ele") <- entry_ele
  return(cls_entry)
}

#' @method .exp_cls_entry mixed
#' @export
.exp_cls_entry.mixed <- function(cls_entry,
                                 var_extract,
                                 sets,
                                 call) {
  entry_mixed <- sub(")", "", purrr::pluck(strsplit(cls_entry, "\\("), 1, 2))
  entry_mixed <- strsplit(entry_mixed, ",")[[1]]
  var_sets <- purrr::pluck(var_extract, "ls_upper_idx", attr(cls_entry, "var_name"))

  full_entry <- do.call(
    data.table::CJ,
    c(with(sets, mget(var_sets, ifnotfound = "")), sorted = FALSE)
  )

  entry_mixed <- purrr::map(entry_mixed, function(e) {
    if (!grepl("\"", e)) {
      # we could check subsets directly but need better set-subset mapping
      with(sets, get(e))
    } else {
      utils::type.convert(gsub("\"", "", e), as.is = TRUE)
    }
  })

  entry_mixed <- data.table::setnames(do.call(data.table::CJ, entry_mixed), var_sets)
  data.table::setkey(entry_mixed)
  if (!nrow(data.table::fsetdiff(entry_mixed, full_entry)) %=% 0L) {
    invalid_entries <- data.table::fsetdiff(entry_mixed, full_entry)
    n_invalid_entries <- nrow(invalid_entries)
    .cli_action(cls_err$mixed_invalid,
      action = "abort",
      call = call
    )
  }

  attr(cls_entry, "comp") <- strsplit(gsub("\"|\\)", "", pluck(strsplit(cls_entry, "\\("), 1, 2)), ",")[[1]]
  attr(cls_entry, "ele") <- entry_mixed
  return(cls_entry)
}

#' @method .exp_cls_entry subset
#' @export
.exp_cls_entry.subset <- function(cls_entry,
                                  var_extract,
                                  sets,
                                  call) {
  entry_subset <- sub(")", "", purrr::pluck(strsplit(cls_entry, "\\("), 1, 2))
  entry_subset <- strsplit(entry_subset, ",")[[1]]
  var_sets <- purrr::pluck(var_extract, "ls_upper_idx", attr(cls_entry, "var_name"))

  if (all(entry_subset == var_sets)) {
    class(cls_entry) <- c("full", class(cls_entry))
    UseMethod(".exp_cls_entry", cls_entry)
  }

  full_entry <- do.call(
    data.table::CJ,
    c(with(sets, mget(var_sets, ifnotfound = "")), sorted = FALSE)
  )
  attr(cls_entry, "comp") <- entry_subset
  entry_subset <- with(sets, mget(entry_subset))
  entry_subset <- data.table::setnames(do.call(data.table::CJ, entry_subset), var_sets)
  data.table::setkey(entry_subset)
  if (!nrow(data.table::fsetdiff(entry_subset, full_entry)) %=% 0L) {
    .cli_action(cls_err$subset_invalid,
      action = "abort",
      call = call
    )
  }

  attr(cls_entry, "ele") <- entry_subset
  return(cls_entry)
}
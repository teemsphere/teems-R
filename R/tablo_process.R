#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map_chr pluck
#' @importFrom cli cli_h1 cli_dl cli_fmt
#'
#' @keywords internal
#' @noRd
.process_tablo <- function(tab_file,
                           var_omit = NULL,
                           type = NULL,
                           quiet = FALSE,
                           call) {

  tab <- .check_tab_file(
    tab_file = tab_file,
    call = call
  )

  if (!is.null(var_omit)) {
    for (var in unique(var_omit)) {
      tab <- .omit_var(
        var_omit = var,
        statements = tab
      )
    }
  }

  extract <- .generate_extracts(
    tab = tab,
    call = call
  )

  ele_names <- subset(
    extract$set,
    is.na(header) & qualifier_list == "(non_intertemporal)" & is.na(comp1) & is.na(comp2),
    definition,
    1
  )

  if (any(purrr::map_lgl(
    ele_names,
    function(e) {
      any(tolower(e) != e)
    }
  ))) {
    upper_ele <- unlist(ele_names[tolower(ele_names) != ele_names])

    for (nme in unique(upper_ele)) {
      pattern <- paste0("\\b", nme, "\\b")
      tab <- gsub(pattern, tolower(nme), tab)
    }

    extract <- .generate_extracts(
      tab = tab,
      call = call
    )
  }

  if (any(grepl("\"CGDS\"", tab, ignore.case = TRUE))) {
    tab <- gsub("\"CGDS\"", "\"zcgds\"", tab, ignore.case = TRUE)
    extract <- .generate_extracts(
      tab = tab,
      call = call
    )
  }

  var_extract <- .parse_tab_obj(
    extract = extract$model,
    obj_type = "variable",
    call = call
  )

  coeff_extract <- .parse_tab_obj(
    extract = extract$model,
    obj_type = "coefficient",
    call = call
  )

  if (any(grepl("\\(intertemporal\\)", purrr::pluck(extract, "set", "qualifier_list")))) {
    .check_int_headers(
      coeff_extract = coeff_extract,
      call = call
    )
  }

  if (.o_verbose() && !quiet) {
    math_extract <- .parse_tab_maths(
      tab_extract = extract$model,
      call = call
    )

    n_var <- nrow(var_extract)
    n_eq <- nrow(subset(math_extract, math_extract$type %in% "Equation"))
    n_form <- nrow(subset(math_extract, math_extract$type %in% "Formula"))
    n_coeff <- nrow(coeff_extract)
    n_sets <- nrow(extract$set)

    cli::cli_h1("Tablo file summary statistics:")
    cli::cli_dl(c(
      "Variables" = n_var,
      "Equations" = n_eq,
      "Coefficients" = n_coeff,
      "Formulas" = n_form,
      "Sets" = n_sets
    ))
  }

  tab <- paste0(tab, ";")
  tab <- tibble::tibble(
    tab = tab,
    row_id = seq(1, length(tab))
  )

  tab_parsed <- rbind(var_extract, coeff_extract, extract$set)
  tab <- tibble::as_tibble(merge(tab_parsed, tab, by = "row_id", all = TRUE))
  tab <- tab[order(tab$row_id), ]

  tab$type <- ifelse(is.na(tab$type),
    purrr::pluck(extract, "model", "type"),
    tab$type
  )

  tab$row_id <- NULL
  tab$type <- tools::toTitleCase(tolower(tab$type))
  tab <- subset(tab, type != "Write")
  # drop File used for output, need a separate fun arg for this
  tab <- subset(tab, !(type == "File" & grepl("(new)", tab, ignore.case = )))

  if (any(tab$header %in% .o_full_exclude())) {
    x_header <- intersect(tab$header, .o_full_exclude())
    for (h in unique(x_header)) {
      x_coeff <- tab$name[match(h, tab$header)]
      tab <- subset(tab, !grepl(x_coeff, tab))
    }
  }

  tab <- structure(tab,
    tab_file = tab_file,
    var_omit = var_omit,
    class = c("model", class(tab))
  )

  return(tab)
}

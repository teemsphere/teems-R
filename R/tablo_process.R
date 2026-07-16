#' @importFrom tibble tibble as_tibble
#' @importFrom purrr map_chr pluck
#' @importFrom cli cli_h1 cli_dl cli_fmt
#'
#' @keywords internal
#' @note This will become a method for "process_model"
#' @noRd
.process_tablo <- function(tab_file,
                           omit = NULL,
                           backsolve = NULL,
                           ignore_condense = FALSE,
                           type = NULL,
                           quiet = FALSE,
                           call) {

  tab <- .check_tab_file(
    tab_file = tab_file,
    call = call
  )

  tab <- .rewrite_tab_if(
    tab = tab,
    call = call
  )

  tab <- .rewrite_tab_netcut(
    tab = tab,
    call = call
  )

  condensed <- .condense_model(
    tab = tab,
    omit = omit,
    backsolve = backsolve,
    ignore_condense = ignore_condense,
    quiet = quiet,
    call = call
  )
  tab <- condensed$tab

  extract <- .generate_extracts(
    tab = tab,
    call = call
  )
  
  ele_names <- extract$set[with(extract$set,
    expr = {is.na(header) &
            qualifier_list == "(non_intertemporal)" &
            is.na(comp1) &
            is.na(comp2)}
    ), ]$definition

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
    tab <- gsub("\"CGDS\"", "\"cgds\"", tab, ignore.case = TRUE)
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

  math_extract <- .parse_tab_maths(
    extract = extract$model,
    call = call
  )

  .check_netcut(
    var_extract = var_extract,
    math_extract = math_extract,
    set_extract = extract$set,
    call = call
  )

  if (.o_verbose() && !quiet) {
    n_var <- nrow(var_extract)
    n_eq <- nrow(math_extract[math_extract$type %in% "Equation",])
    n_form <- nrow(math_extract[math_extract$type %in% "Formula",])
    n_coeff <- nrow(coeff_extract)
    n_sets <- nrow(extract$set)

    summary_items <- c(
      "Variables" = n_var,
      "Equations" = n_eq,
      "Coefficients" = n_coeff,
      "Formulas" = n_form,
      "Sets" = n_sets
    )
    if (condensed$n_omit + condensed$n_backsolve > 0L) {
      summary_items <- c(
        summary_items,
        "Omitted" = condensed$n_omit,
        "Backsolved" = condensed$n_backsolve
      )
    }

    model_summary <- cli::cli_fmt({
      cli::cli_h1("Model summary:")
      cli::cli_dl(summary_items)
    })
  }
  
  read_extract <- .parse_tab_read(
    extract = extract$model,
    call = call
  )

  tab <- paste0(tab, ";")

  tab <- tibble::tibble(
    tab = tab,
    row_id = seq_along(tab)
  )

  tab_parsed <- rbind(var_extract, coeff_extract, extract$set, math_extract, read_extract)
  tab <- tibble::as_tibble(merge(tab_parsed, tab, by = "row_id", all = TRUE))
  tab <- tab[order(tab$row_id), ]

  tab$type <- ifelse(is.na(tab$type),
    purrr::pluck(extract, "model", "type"),
    tab$type
  )

  tab$row_id <- NULL
  tab$type <- tools::toTitleCase(tolower(tab$type))
  tab <- tab[tolower(tab$type) != "write",]
  # drop File used for output, need a separate fun arg for this
  tab <- tab[!(tolower(tab$type) == "file" & grepl("(new)", tab$tab, ignore.case = TRUE)),]
  # potentially handle postsim here or simply throw error

  if (any(tab$header %in% .o_full_exclude())) {
    x_header <- intersect(tab$header, .o_full_exclude())
    for (h in unique(x_header)) {
      x_coeff <- tab$name[match(h, tab$header)]
      tab <- tab[!grepl(x_coeff, tab$tab),]
    }
  }

  tab$condense <- NA_character_
  tab$condense_eq <- NA_character_
  if (!is.null(condensed$flags) && nrow(condensed$flags) > 0L) {
    flag_key <- paste(condensed$flags$type, tolower(condensed$flags$name))
    tab_key <- paste(tab$type, tolower(tab$name))
    r_idx <- match(tab_key, flag_key)
    tab$condense <- condensed$flags$condense[r_idx]
    tab$condense_eq <- condensed$flags$condense_eq[r_idx]
  }


  if (.o_verbose() && !quiet) {
    attr(tab, "model_summary") <- model_summary
  }
  
  attr(tab, "tab_file") <- basename(tab_file)
  return(tab)
}

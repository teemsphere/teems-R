#' @importFrom data.table data.table fsetdiff funion fintersect
#' @importFrom purrr pmap map map_chr map_lgl
#' @importFrom stats na.omit
#' 
#' @keywords internal
#' @noRd
.finalize_sets <- function(sets,
                           set_extract,
                           coeff_extract,
                           time_steps,
                           reference_year,
                           call,
                           data_call,
                           model_call) {

  if (!all(stats::na.omit(set_extract$header) %in% names(sets))) {
    m_map <- setdiff(stats::na.omit(set_extract$header), names(sets))
   .cli_action(data_err$missing_mapping,
               action = "abort",
               call = data_call) 
  }

  intertemporal <- if (any(grepl(
    "\\(intertemporal\\)",
    set_extract$qualifier_list
  ))) {
    TRUE
  } else {
    FALSE
  }

  if (intertemporal && is.null(time_steps)) {
      .cli_action(data_err$missing_tsteps,
                  action = "abort",
                  call = data_call)
  }
  
  if (!is.null(time_steps) && !intertemporal) {
    .cli_action(data_err$nonreq_tsteps,
                action = "abort",
                call = data_call)
  }

  if (intertemporal) {
    CYRS <- tibble::tibble(all_time = seq(0, length(time_steps) - 1),
                           Value = reference_year + time_steps)
    attr(set_extract, "time_steps") <- time_steps
    attr(set_extract, "CYRS") <- CYRS
    n_timestep_coeff <- coeff_extract$name[match(.o_n_timestep_header(), coeff_extract$header)]
    set_extract$mapping <- purrr::map2(
      set_extract$qualifier_list,
      set_extract$definition,
      function(q, d) {
        if (q %=% "(non_intertemporal)") {
          NULL
        } else {
          .convert_int_sets(
            expr = d,
            n_timestep = length(time_steps),
            n_timestep_coeff = n_timestep_coeff
          )
        }
      }
    )

    r_idx <- match(set_extract$header, names(sets))
    set_extract$mapping <- ifelse(!is.na(r_idx),
                                  sets[r_idx],
                                  set_extract$mapping)
  } else {
    r_idx <- match(set_extract$header, names(sets))
    set_extract$mapping <- sets[r_idx]
  }
  # check for correct sets
  names(set_extract$mapping) <- set_extract$name
  set_op_pattern <- paste0("UNION|\\+|\\-|\\INTERSECT")
  set_extract$mapping <- purrr::map2(
    set_extract$definition,
    set_extract$mapping,
    function(d, m) {
      if (is.null(m) && !isTRUE(grepl(set_op_pattern, d, ignore.case = TRUE))) {
        m <- data.table::data.table(
          origin = d,
          mapping = d,
          key = c("origin", "mapping")
        )
      }
      return(m)
    }
  )

  safety_count <- 0
  exit_loop <- nrow(set_extract) - 2
  while (any(purrr::map_lgl(set_extract$mapping, is.null))) {
    safety_count <- safety_count + 1
    if (safety_count > exit_loop) {
      null_sets <- names(set_extract$mapping)[purrr::map_lgl(set_extract$mapping, is.null)]
      .cli_action(set_err$while_loop,
        action = "abort",
        call = call
      )
    }

    set_extract$mapping <- purrr::pmap(
      list(
        set_extract$mapping,
        set_extract$definition,
        set_extract$comp1,
        set_extract$comp2
      ),
      function(m, d, c1, c2) {
        if (is.null(m)) {
          x <- subset(
            set_extract,
            name %in% c1,
            mapping,
            1
          )[[1]]
          y <- subset(
            set_extract,
            name %in% c2,
            mapping,
            1
          )[[1]]
          if (any(is.null(x), is.null(y))) {
            return(NULL)
          } else if (grepl("\\+", d)) {
            if (!nrow(data.table::fintersect(x, y)) %=% 0L) {
              .cli_action(set_err$invalid_plus,
                          action = "abort",
                          call = model_call)
            }
            m <- data.table::funion(x, y)
          } else if (grepl("\\-", d)) {
            m <- data.table::fsetdiff(x, y, all = TRUE)
          } else if (grepl("union", d, ignore.case = TRUE)) {
            m <- data.table::funion(x, y)
          } else if (grep("intersect", d, ignore.case = TRUE)) {
            m <- data.table::fintersect(x, y)
          }
        }
        return(m)
      }
    )
  }
  
  set_extract$ele <- purrr::map(set_extract$mapping, function(s) {
    unique(s$mapping)
  })

  attr(set_extract, "intertemporal") <- intertemporal
  return(set_extract)
}
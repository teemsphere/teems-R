#' @importFrom purrr pluck
#' 
#' @keywords internal
#' @noRd
.retrieve_output <- function(var_tbl,
                             var_data,
                             type,
                             comp_extract,
                             paths,
                             sets,
                             time_steps,
                             call) {

  compose_variable <- type %in% c("variable", "all")
  compose_coefficient <- type %in% c("coefficient", "all")
  output <- list()

  if (compose_variable) {
    output$variable <- .compose_var(
      data_dt = var_data,
      var_extract = comp_extract$variable,
      vars = var_tbl,
      sets = sets,
      time_steps = time_steps,
      call = call
    )
  }
  
  if (compose_coefficient) {
    output$coefficient <- .compose_coeff(
      paths = paths$coeff,
      coeff_extract = comp_extract$coefficient,
      sets = sets,
      time_steps = time_steps,
      call = call
    )
  }

  # PostSim coefficients (computed after the solve, dumped by the
  # solver into out/postsim/) compose like ordinary coefficients but
  # carry their own type
  if (compose_coefficient && !is.null(paths$postsim)) {
    output$postsim <- .compose_coeff(
      paths = paths$postsim,
      coeff_extract = comp_extract$coefficient,
      sets = sets,
      time_steps = time_steps,
      call = call,
      type_label = "postsim"
    )
  }

  if (type == "all") {
    output <- rbind(output$variable, output$coefficient, output$postsim)
  } else if (!is.null(output$postsim)) {
    output <- rbind(output[[1]], output$postsim)
  } else {
    output <- output[[1]]
  }

  return(output)
}

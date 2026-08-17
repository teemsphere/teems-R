#' @importFrom purrr map_lgl
#' @importFrom tools file_path_sans_ext
#'
#' @keywords internal
#' @noRd
.in_situ_cmf <- function(input_files,
                         model_file,
                         model_dir,
                         shock_file,
                         closure_file,
                         call) {
  input_files <- .check_named_dots(input_files)

  if (isFALSE(input_files)) {
    .cli_action(solve_err$no_input_names,
      action = "abort",
      call = call
    )
  }
  
  if (!dir.exists(model_dir)) {
    model_dir <- normalizePath(model_dir, "/", FALSE)
    .cli_action(solve_err$no_model_dir,
                action = "abort",
                call = call
    )
  }
  
  model_file <- .check_input(
    file = model_file,
    valid_ext = "tab",
    call = call
  )

  shock_file <- .check_input(
    file = shock_file,
    valid_ext = "shf",
    call = call
  )

  closure_file <- .check_input(
    file = closure_file,
    valid_ext = "cls",
    call = call
  )

  cmf <- .cmf_core(
    input_files = input_files,
    model_file = model_file,
    closure_file = closure_file,
    shock_file = shock_file,
    model_dir = model_dir
  )

  model <- .process_tablo(
    tab_file = model_file,
    quiet = TRUE,
    call = call
  )

  req_inputs <- setdiff(unique(model$file), NA)

  if (!all(req_inputs %in% names(input_files))) {
    missing_files <- setdiff(req_inputs, names(input_files))
    .cli_action(solve_err$missing_insitu_inputs,
      action = "abort",
      call = call
    )
  }

  if (!all(purrr::map_lgl(input_files, file.exists))) {
    nonexist_files <- input_files[!purrr::map_lgl(input_files, file.exists)]
    .cli_action(solve_err$insitu_no_file,
      action = "abort",
      call = call
    )
  }

  tab <- .finalize_tab(model = model)
  .ems_write(
    input = tab,
    write_dir = model_dir
  )
  cmf_writeout <- .writeout(
    model = model,
    write_dir = model_dir,
    coeff = FALSE
  )
  cmf <- c(cmf, cmf_writeout)

  cmf_file <- paste0(tools::file_path_sans_ext(basename(model_file)), ".cmf")
  cmf_path <- file.path(model_dir, cmf_file)
  attr(cmf, "write_path") <- cmf_path
  class(cmf) <- c("cmf", class(cmf))
  cmf_path <- .ems_write(input = cmf)

  return(cmf_path)
}
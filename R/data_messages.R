#' @importFrom rlang current_env
#' @importFrom utils packageVersion
#'
#' @keywords internal
#' @noRd
.inform_metadata <- function(metadata) {
  if (.o_verbose()) {
    list2env(
      metadata,
      rlang::current_env()
    )
    .cli_action(gen_info$dat,
      action = rep(x = "inform", 3)
    )
  }
}

.check_database_version <- function(vetted,
                                    provided,
                                    call,
                                    quiet) {
  if (!provided %in% vetted) {
    teems_version <- utils::packageVersion("teems")
    .cli_action(gen_wrn$db_version,
                action = c("warn", "inform"),
                call = call
    )
  }
}

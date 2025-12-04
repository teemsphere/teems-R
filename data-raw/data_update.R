.update_data <- function() {
  store_dir <- "./data-raw/_targets"
  Sys.setenv(TAR_CONFIG = "./data-raw/_targets.yaml")
  targets::tar_make(script = "./data-raw/teems_data-raw.R",
                    store = store_dir)
}

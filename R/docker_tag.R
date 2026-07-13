#' @keywords internal
#' @noRd
.supported_isa_levels <- function(sysname = Sys.info()[["sysname"]],
                                  machine = Sys.info()[["machine"]]) {
  if (is.element(machine, c("aarch64", "arm64"))) {
    return("armv8-a")
  }
  if (!is.element(machine, c("x86_64", "x86-64", "AMD64"))) {
    return(character(0))
  }

  levels <- character(0)
  if (sysname %=% "Linux") {
    ld_so <- Sys.which("ld.so")
    if (!nzchar(ld_so)) {
      ld_so <- "/lib64/ld-linux-x86-64.so.2"
    }
    if (file.exists(ld_so)) {
      out <- tryCatch(
        suppressWarnings(system2(ld_so, "--help", stdout = TRUE, stderr = FALSE)),
        error = function(e) character(0)
      )
      hits <- regmatches(
        x = out,
        m = regexpr("x86-64-v[0-9]+(?= \\(supported, searched\\))", out, perl = TRUE)
      )
      levels <- sort(unique(unlist(hits)), decreasing = TRUE)
    }
  } else if (sysname %=% "Darwin") {
    feats <- tryCatch(
      suppressWarnings(system2("sysctl", c("-n", "machdep.cpu.leaf7_features"),
        stdout = TRUE, stderr = FALSE
      )),
      error = function(e) character(0)
    )
    if (any(grepl("AVX2", feats))) {
      levels <- c("x86-64-v3", "x86-64-v2")
    }
  }

  if (!length(levels)) {
    levels <- "x86-64-v2"
  }
  return(levels)
}

#' @keywords internal
#' @noRd
.docker_image_present <- function(image_name) {
  if (!nzchar(Sys.which("docker"))) {
    return(FALSE)
  }
  out <- tryCatch(
    suppressWarnings(system2("docker", c("images", "-q", image_name),
      stdout = TRUE, stderr = FALSE
    )),
    error = function(e) character(0)
  )
  return(any(nzchar(out)))
}

#' @keywords internal
#' @noRd
.resolve_docker_tag <- function() {
  explicit <- ems_options$docker_tag
  if (!is.null(explicit)) {
    return(explicit)
  }

  for (level in .supported_isa_levels()) {
    candidates <- unique(c(level, sub("^x86-64-", "", level)))
    for (tag in candidates) {
      if (.docker_image_present(paste0("teems:", tag))) {
        .cli_action(solve_info$docker_tag_auto,
          action = "inform"
        )
        return(tag)
      }
    }
  }
  return("latest")
}

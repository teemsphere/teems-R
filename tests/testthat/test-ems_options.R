skip_on_cran()

withr::defer(ems_option_reset(), teardown_env())
variant <- Sys.info()["sysname"]

test_that("ems_option_get errors on invalid name", {
  expect_snapshot_error(ems_option_get("not_an_option"))
})

test_that("ems_option_get returns list of all options when name is NULL", {
  opts <- ems_option_get()
  expect_type(opts, "list")
  expect_true(length(opts) > 0)
})

test_that("ems_option_get returns logical for verbose", {
  expect_type(ems_option_get("verbose"), "logical")
})

test_that("ems_option_get returns character for tempdir", {
  expect_type(ems_option_get("tempdir"), "character")
})

test_that("ems_option_get returns numeric for ndigits", {
  expect_type(ems_option_get("ndigits"), "integer")
})

test_that("ems_option_get returns logical for check_shock_status", {
  expect_type(ems_option_get("check_shock_status"), "logical")
})

test_that("ems_option_get returns character for timestep_header", {
  expect_type(ems_option_get("timestep_header"), "character")
})

test_that("ems_option_get returns character for n_timestep_header", {
  expect_type(ems_option_get("n_timestep_header"), "character")
})

test_that("ems_option_get returns character for docker_tag", {
  expect_type(ems_option_get("docker_tag"), "character")
})

test_that("ems_option_get returns numeric for accuracy_threshold", {
  expect_type(ems_option_get("accuracy_threshold"), "double")
})

test_that("ems_option_set sets verbose", {
  ems_option_set(verbose = FALSE)
  expect_false(ems_option_get("verbose"))
  ems_option_reset()
})

test_that("ems_option_set sets tempdir", {
  ems_option_set(tempdir = getwd())
  expect_equal(ems_option_get("tempdir"), getwd())
  ems_option_reset()
})

test_that("ems_option_set sets ndigits", {
  ems_option_set(ndigits = 8)
  expect_equal(ems_option_get("ndigits"), 8)
  ems_option_reset()
})

test_that("ems_option_set sets check_shock_status", {
  ems_option_set(check_shock_status = FALSE)
  expect_false(ems_option_get("check_shock_status"))
  ems_option_reset()
})

test_that("ems_option_set sets docker_tag", {
  ems_option_set(docker_tag = "v1.0")
  expect_equal(ems_option_get("docker_tag"), "v1.0")
  ems_option_reset()
})

test_that("ems_option_set sets accuracy_threshold", {
  ems_option_set(accuracy_threshold = 0.95)
  expect_equal(ems_option_get("accuracy_threshold"), 0.95)
  ems_option_reset()
})

test_that("ems_option_set sets timestep_header", {
  ems_option_set(timestep_header = "AYRS")
  expect_equal(ems_option_get("timestep_header"), "AYRS")
  ems_option_reset()
})

test_that("ems_option errors when write_dir does not exist", {
  expect_snapshot_error(
    ems_option_set(tempdir = file.path(basename(getwd()), "does_not_exist")),
    variant = variant
  )
})

test_that("ems_option_set sets n_timestep_header", {
  ems_option_set(n_timestep_header = "NINTERVAL")
  expect_equal(ems_option_get("n_timestep_header"), "NINTERVAL")
  ems_option_reset()
})

test_that("ems_option_set returns NULL invisibly", {
  expect_null(ems_option_set(verbose = FALSE))
  ems_option_reset()
})

test_that("ems_option_reset restores default verbose", {
  ems_option_set(verbose = FALSE)
  ems_option_reset()
  expect_true(ems_option_get("verbose"))
})

test_that("ems_option_reset restores default ndigits", {
  ems_option_set(ndigits = 10)
  ems_option_reset()
  expect_equal(ems_option_get("ndigits"), 6)
})

test_that("ems_option_reset restores default docker_tag", {
  ems_option_set(docker_tag = "v1.0")
  ems_option_reset()
  expect_equal(ems_option_get("docker_tag"), "latest")
})

test_that("ems_option_reset returns NULL invisibly", {
  expect_null(ems_option_reset())
})

test_that("ems_option_set examples work", {
  # Set multiple options
  ems_option_set(verbose = FALSE, ndigits = 8)

  # Retrieve value of `verbose`
  ems_option_get("verbose")

  expect_false(ems_option_get("verbose"))
  expect_equal(ems_option_get("ndigits"), 8)
  # Reset options to default values
  ems_option_reset()
})

test_that("ems_option_get examples work", {
  # Retrieve all options values
  expect_type(ems_option_get(), "list")

  # Retrieve option value for `ndigits`
  expect_type(ems_option_get("ndigits"), "integer")
  ems_option_reset()
})

test_that("ems_option_reset examples work", {
  # Set multiple options
  ems_option_set(verbose = FALSE, ndigits = 8)

  # Retrieve modified option value for `verbose`
  ems_option_get("verbose")
  expect_false(ems_option_get("verbose"))
  # Reset options to default values
  ems_option_reset()
  expect_true(ems_option_get("verbose"))
  # Retrieve default option value for `verbose`
  ems_option_get("verbose")
})

test_that("supported ISA levels resolve per platform", {
  expect_identical(.supported_isa_levels(machine = "arm64"), "armv8-a")
  expect_identical(.supported_isa_levels(machine = "aarch64"), "armv8-a")
  expect_identical(
    .supported_isa_levels(sysname = "Windows", machine = "x86_64"),
    "x86-64-v2"
  )
  expect_identical(.supported_isa_levels(machine = "ppc64le"), character(0))
  if (Sys.info()[["sysname"]] == "Linux" && Sys.info()[["machine"]] == "x86_64") {
    levels <- .supported_isa_levels()
    expect_true(all(grepl("^x86-64-v[0-9]+$", levels)))
    expect_true(is.element("x86-64-v2", levels))
    expect_identical(levels, sort(levels, decreasing = TRUE))
  }
})

test_that("docker tag auto-selection", {
  # an explicit docker_tag always wins
  ems_option_set(docker_tag = "custom")
  expect_identical(.resolve_docker_tag(), "custom")
  ems_option_reset()

  # highest supported local variant preferred; short-form tag matches too
  local_mocked_bindings(
    .supported_isa_levels = function(...) c("x86-64-v3", "x86-64-v2"),
    .docker_image_present = function(image_name) image_name == "teems:v3"
  )
  expect_snapshot(tag <- .resolve_docker_tag())
  expect_identical(tag, "v3")

  # no variant image present: silent fallback to latest
  local_mocked_bindings(
    .docker_image_present = function(image_name) FALSE
  )
  expect_silent(expect_identical(.resolve_docker_tag(), "latest"))
})

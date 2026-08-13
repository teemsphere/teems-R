fx <- test_path("fixtures", "probe")

healthy <- .probe_object(
  probe_path = file.path(fx, "healthy.probe.json"),
  stats_path = file.path(fx, "healthy.stats.json")
)
broken <- .probe_object(
  probe_path = file.path(fx, "broken.probe.json")
)

test_that("healthy probe report parses", {
  expect_s3_class(healthy, "teems_probe")
  expect_true(healthy$valid)
  expect_identical(healthy$version, 2L)
  expect_false(healthy$structural$defective)
  expect_false(healthy$realized$defective)
  expect_identical(healthy$structural$rank, healthy$vecsize)
  # the statement table tiles the condensed system exactly
  expect_identical(sum(healthy$statements$rows), healthy$vecsize)
  expect_gt(nrow(healthy$incidence), 0L)
  expect_identical(nrow(healthy$defects), 0L)
  # fine decomposition
  expect_identical(healthy$cores$largest, 4341L)
  expect_identical(healthy$cores$top$eqs[[1]]$name[[1]], "e_qfa")
  # stats.json companion
  expect_identical(healthy$structure$vecsize, healthy$vecsize)
})

test_that("broken probe report parses with named defects", {
  expect_s3_class(broken, "teems_probe")
  expect_false(broken$valid)
  expect_true(broken$structural$defective)
  expect_identical(broken$structural$rank, broken$vecsize - 3L)
  # exact named defects on both sides
  expect_identical(
    broken$structural$under_determined$name,
    rep("dprobeb", 3L)
  )
  expect_identical(
    broken$structural$over_constrained$name,
    rep("e_dprobe2", 3L)
  )
  # element tuples parsed from the solver-side labels
  expect_identical(broken$structural$under_determined$elements[[1]], "0")
  # aggregations
  expect_identical(broken$structural$under_by_var$name, "dprobeb")
  expect_identical(broken$structural$under_by_var$count, 3L)
  expect_identical(
    sort(broken$structural$dm_over_by_eq$name),
    c("e_dprobe1", "e_dprobe2")
  )
  # DM block sizes
  expect_identical(broken$structural$dm$m3, 6L)
  expect_identical(broken$structural$dm$n3, 3L)
  # combined defect tibble spans both patterns and sides
  expect_identical(nrow(broken$defects), 12L)
  expect_setequal(broken$defects$pattern, c("structural", "realized"))
})

test_that("probe print methods run", {
  expect_snapshot(print(healthy))
  expect_snapshot(print(broken))
})

test_that("probe plots render on a null device", {
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off())
  expect_invisible(plot(healthy, type = "incidence"))
  expect_invisible(plot(healthy, type = "cores"))
  expect_invisible(plot(broken, type = "dm"))
  expect_invisible(plot(broken, type = "incidence"))
})

test_that("dm plot errors on a structurally valid probe", {
  grDevices::pdf(NULL)
  withr::defer(grDevices::dev.off())
  expect_snapshot_error(plot(healthy, type = "dm"))
})

test_that("cores plot errors without fine data", {
  no_fine <- healthy
  no_fine$cores <- NULL
  expect_snapshot_error(plot(no_fine, type = "cores"))
})

test_that("probe report errors when the report is absent", {
  # the message embeds an absolute path, so no snapshot
  expect_error(
    .probe_object(probe_path = file.path(fx, "nonexistent.probe.json")),
    regexp = "No probe report was produced"
  )
})

test_that("ems_probe errors when cmf_path is missing", {
  expect_snapshot_error(ems_probe())
})

test_that("ems_probe errors when fine is not a logical scalar", {
  expect_snapshot_error(ems_probe(cmf_path = "some.cmf", fine = "yes"))
})

test_that("probe verdict informs on a valid system", {
  expect_message(
    .probe_verdict(probe = healthy, cmf_path = "some.cmf", call = NULL),
    regexp = "structurally valid"
  )
})

test_that("pre_probe verdict aborts on a structurally singular system", {
  # the message embeds the absolute report path, so no snapshot
  expect_error(
    .probe_verdict(probe = broken, cmf_path = "some.cmf", call = NULL),
    regexp = "structurally singular.*rank 10527 of 10530"
  )
})

# probe-informed condensation advice (roadmap 6.2 via 6.10): the verdict
# reads the measured block structure, so it is exercised against stats
# variants of the healthy fixture
probe_stats_variant <- function(...) {
  stats <- jsonlite::fromJSON(file.path(fx, "healthy.stats.json"))
  stats <- utils::modifyList(stats, list(...))
  path <- file.path(tempfile(), "sol.stats.json")
  dir.create(dirname(path))
  writeLines(jsonlite::toJSON(stats, auto_unbox = TRUE, null = "null"), path)
  .probe_object(
    probe_path = file.path(fx, "healthy.probe.json"),
    stats_path = path
  )$condense
}

test_that("probe reports no condensation verdict for a small plain system", {
  cond <- probe_stats_variant()
  expect_false(cond$condensed)
  expect_false(cond$partitioned)
  expect_identical(cond$verdict, "none")
})

test_that("probe advises against condensation when a partition exists", {
  cond <- probe_stats_variant(
    nbacksolve = 68, nbselems = 2000,
    bordered = TRUE, ndblock = 35, netcut = 400, partition_set = "REG"
  )
  expect_true(cond$condensed)
  expect_true(cond$partitioned)
  expect_identical(cond$verdict, "hurts")
  expect_equal(cond$elimination_share, 2000 / 12524, tolerance = 1e-12)
  expect_equal(cond$border_share, 400 / 10524, tolerance = 1e-12)
})

test_that("probe confirms condensation on an LU-bound system", {
  cond <- probe_stats_variant(nbacksolve = 68, nbselems = 2000)
  expect_true(cond$condensed)
  expect_false(cond$partitioned)
  expect_identical(cond$verdict, "helps")
})

test_that("probe suggests condensation for a large LU-bound system", {
  expect_identical(probe_stats_variant(vecsize = 1.35e6)$verdict, "candidate")
  # too small for the measured gain to show
  expect_identical(probe_stats_variant(vecsize = 2e5)$verdict, "none")
  # a partitioned system of the same size is never a candidate
  expect_identical(
    probe_stats_variant(vecsize = 1.35e6, bordered = TRUE, ndblock = 35)$verdict,
    "none"
  )
})

test_that("probe prints each condensation verdict", {
  expect_snapshot({
    for (v in list(
      list(nbacksolve = 68, nbselems = 2000, bordered = TRUE, ndblock = 35,
           netcut = 400, partition_set = "REG"),
      list(nbacksolve = 68, nbselems = 2000),
      list(vecsize = 1.35e6)
    )) {
      .probe_print_condense(do.call(probe_stats_variant, v))
    }
  })
})

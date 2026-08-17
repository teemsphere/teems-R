# matrix_method = "auto" decision rule (ROADMAP 6.10): pure rules over
# probe evidence, exercised on -solmed probe stats.json fixtures from
# the GTAPv7 (static) and GTAP-RE (intertemporal) goldens plus
# synthetic variants. Thresholds are placeholders (see .auto_thresholds)
# and are passed explicitly where a rule branch needs a value the
# placeholders keep dormant.
fx <- test_path("fixtures", "probe")
static_stats <- .probe_stats(jsonlite::fromJSON(file.path(fx, "structural_static.stats.json")))
inter_stats <- .probe_stats(jsonlite::fromJSON(file.path(fx, "structural_inter.stats.json")))
th <- .auto_thresholds()

test_that("probe stats keep the partition candidate table and the chosen set", {
  expect_s3_class(static_stats$partition_auto, "tbl_df")
  expect_identical(nrow(static_stats$partition_auto), 12L)
  expect_identical(static_stats$partition_chosen, "reg")
  expect_identical(static_stats$partition_source, "structural")
  expect_identical(static_stats$chain_source, "none")
  expect_identical(inter_stats$chain_source, "structural")
  expect_identical(inter_stats$chain_set, "alltime")
  expect_identical(inter_stats$ntime, 3L)
})

test_that("partition replay follows the solver's selection at the solve's rank count", {
  # the solver's own choice at 1 rank
  p <- .auto_partition(static_stats, n_tasks = 1L)
  expect_identical(p$set, "reg")
  expect_identical(p$n_blocks, 3L)
  expect_identical(p$netcut, 74L)
  expect_identical(p$border_neq, 224L)
  expect_equal(p$border_share, 224 / 3485)
  # 3 blocks cannot serve 4 tasks: the next viable candidate takes over
  p4 <- .auto_partition(static_stats, n_tasks = 4L)
  expect_identical(p4$set, "demd")
  expect_identical(p4$n_blocks, 9L)
  # border_neq is only known for the solver's chosen set
  expect_true(is.na(p4$border_neq))
  expect_equal(p4$border_share, 535 / 3485)
  # nothing viable at 40 tasks
  expect_null(.auto_partition(static_stats, n_tasks = 40L))
  # nested geometry on the intertemporal fixture
  pi <- .auto_partition(inter_stats, n_tasks = 4L)
  expect_identical(pi$set, "reg")
  expect_identical(pi$n_blocks, 12L)
  expect_identical(pi$netcut, 294L)
  # no structure at all
  expect_null(.auto_partition(NULL, n_tasks = 2L))
})

test_that("static auto chooses DBBD from probe evidence and LU otherwise", {
  # large system, viable partition with enough blocks, small border
  d <- .auto_decide(FALSE, 2L, 2.5e6, structure = static_stats)
  expect_identical(d$method, "DBBD")
  expect_identical(d$source, "probe")
  expect_true(d$probed)
  # too small for DBBD despite a viable partition
  expect_identical(.auto_decide(FALSE, 2L, 3485, structure = static_stats)$method, "LU")
  # not enough blocks for the tasks -> next candidate's border too wide
  expect_identical(.auto_decide(FALSE, 4L, 2.5e6, structure = static_stats)$method, "LU")
  # a border ceiling that admits demd's 15% border flips it
  wide <- th
  wide$border_share_max <- 0.2
  expect_identical(.auto_decide(FALSE, 4L, 2.5e6, structure = static_stats, th = wide)$method, "DBBD")
  # single task: LU with the DBBD hint
  d1 <- .auto_decide(FALSE, 1L, 2.5e6, structure = static_stats)
  expect_identical(d1$method, "LU")
  expect_true(d1$dbbd_hint)
  # many-blocks size gate
  many <- static_stats
  many$partition_auto$nblocks[many$partition_auto$set == "reg"] <- 120L
  expect_identical(.auto_decide(FALSE, 2L, 1.6e6, structure = many)$method, "DBBD")
  expect_identical(.auto_decide(FALSE, 2L, 1.6e6, structure = static_stats)$method, "LU")
  # size unknown -> LU
  expect_identical(.auto_decide(FALSE, 2L, NULL, structure = static_stats)$method, "LU")
})

test_that("static auto without a probe is metadata-only", {
  d <- .auto_decide(FALSE, 2L, 2.5e6)
  expect_identical(d$method, "LU")
  expect_identical(d$source, "metadata")
  expect_false(d$probed)
  expect_true(.auto_decide(FALSE, 1L, 2.5e6)$dbbd_hint)
  expect_true(.auto_decide(FALSE, 1L, 1.6e6, n_reg = 163L)$dbbd_hint)
  expect_false(.auto_decide(FALSE, 1L, 1.6e6, n_reg = 33L)$dbbd_hint)
  expect_identical(.auto_decide(FALSE, 2L, NULL)$source, "none")
})

test_that("intertemporal auto stays SBBD unless the NDBBD escalation is enabled", {
  expect_identical(.auto_decide(TRUE, 4L, 10524)$method, "SBBD")
  expect_identical(.auto_decide(TRUE, 4L, 10524, structure = inter_stats)$method, "SBBD")
  # placeholder keeps the escalation dormant even at many tasks
  expect_identical(.auto_decide(TRUE, 32L, 4.4e6, structure = inter_stats)$method, "SBBD")
  esc <- th
  esc$ndbbd_n_tasks <- 4L
  d <- .auto_decide(TRUE, 4L, 10524, structure = inter_stats, th = esc)
  expect_identical(d$method, "NDBBD")
  expect_identical(d$chain_set, "alltime")
  expect_identical(d$chain_border, 18L)
  # not enough nested blocks for the tasks -> SBBD
  expect_identical(.auto_decide(TRUE, 16L, 10524, structure = inter_stats, th = esc)$method, "SBBD")
  # wide nested border -> SBBD
  narrow <- esc
  narrow$border_share_max <- 0.01
  expect_identical(.auto_decide(TRUE, 4L, 10524, structure = inter_stats, th = narrow)$method, "SBBD")
})

test_that("a declared intertemporal model without a chain falls to the static family", {
  d <- .auto_decide(TRUE, 2L, 2.5e6, structure = static_stats)
  expect_true(d$no_chain)
  expect_identical(d$method, "DBBD")
  expect_identical(.auto_decide(TRUE, 1L, 10524, structure = static_stats)$method, "LU")
})

test_that("auto evidence and record lines render every input", {
  d <- .auto_decide(FALSE, 2L, 2.5e6, structure = static_stats)
  expect_match(.auto_evidence(d), "^2,500,000 equations, no chain, partition reg \\(3 blocks, border 6.4%\\), n_tasks 2$")
  d <- .auto_decide(TRUE, 4L, 10524, structure = inter_stats)
  expect_match(.auto_evidence(d), "chain alltime \\(3 blocks\\), partition reg \\(12 blocks, border 2.8%\\)")
  d <- .auto_decide(FALSE, 4L, 3485)
  expect_match(.auto_evidence(d), "^3,485 equations, n_tasks 4; structural probe skipped")
  lines <- .auto_record_lines(d)
  expect_length(lines, 2L)
  expect_match(lines[1], "^Matrix method auto: LU \\(deploy metadata: 3,485 equations")
  d <- .auto_decide(FALSE, 2L, 2.5e6, structure = static_stats)
  expect_match(.auto_record_lines(d)[1], "^Matrix method auto: DBBD \\(structural probe: 2,500,000 equations")
  expect_match(lines[2], "probe_min_size 1500000, dbbd_size 2000000, dbbd_size_many_blocks 1500000, dbbd_n_blocks 100, border_share_max 0.1, ndbbd_n_tasks Inf")
  expect_null(.auto_record_lines(NULL))
})

skip_on_cran()

# M4 (dev/validation_table.md M-rows): Mapping statements (GEMPACK
# manual 11.9) through the R pipeline. Model-stage checks mirror the
# solver's tab_parse.c mapping fatals; deploy-stage checks re-derive
# the (by_elements) header under the active aggregation
# (compose-when-consistent, abort-when-split) ahead of the solver's
# by_elements read fatals. Solver ground truth:
# teems-solver/.audit/mapping-test-kit (35 legs).

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "tab_mapping")
if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}
dir.create(write_dir, recursive = TRUE)
ems_option_set(verbose = FALSE, tempdir = write_dir)
withr::defer(ems_option_reset(), teardown_env())

model_files <- ems_example("GTAPv7", write_dir)
model_file <- model_files[["model_file"]]
closure_file <- model_files[["closure_file"]]
base_txt <- readChar(model_file, file.info(model_file)$size)

mutate_tab <- function(text, name = "mut.tab") {
  path <- file.path(write_dir, name)
  writeChar(paste0(base_txt, "\n", text, "\n"), path, eos = NULL)
  path
}

expect_preflight_error <- function(text) {
  expect_snapshot_error(
    .process_tablo(tab_file = mutate_tab(text), quiet = TRUE, call = NULL)
  )
}

byele_read <- "Read (by_elements) REGTOBLOC from file GTAPSETS header \"MBLC\";"

mapping_block <- paste(
  "Set BLOC (blk1, blk2);",
  "Mapping (onto) REGTOBLOC from REG to BLOC;",
  byele_read,
  "Coefficient (all,r,REG) REGW(r) # per-region weights #;",
  "Formula (all,r,REG) REGW(r) = 3;",
  "Coefficient (all,b,BLOC) BTOT(b) # conditional bloc totals #;",
  "Formula (all,b,BLOC) BTOT(b) = sum{r,REG: REGTOBLOC(r)=b, REGW(r)};",
  "Variable (all,b,BLOC) vbloc(b) # bloc driver #;",
  "Variable vtot # sum pickup #;",
  "Equation E_vtot vtot = sum{r,REG, vbloc(REGTOBLOC(r))};",
  sep = "\n"
)

# --- model stage -------------------------------------------------------

test_that("mapping declarations and by_elements reads parse", {
  model <- .process_tablo(
    tab_file = mutate_tab(mapping_block),
    quiet = TRUE,
    call = NULL
  )
  map_row <- model[model$type == "Mapping", ]
  expect_identical(nrow(map_row), 1L)
  expect_identical(map_row$name, "REGTOBLOC")
  expect_identical(map_row$qualifier_list, "(onto)")
  expect_identical(map_row$comp1, "REG")
  expect_identical(map_row$comp2, "BLOC")

  rd <- model[model$type == "Read" & model$name %in% "REGTOBLOC", ]
  expect_identical(rd$qualifier_list, "(by_elements)")
  expect_identical(rd$header, "MBLC")
  expect_identical(rd$file, "GTAPSETS")

  # statements reach the deployed TAB verbatim
  tab <- .finalize_tab(model)
  expect_match(tab, "Mapping (onto) REGTOBLOC from REG to BLOC;", fixed = TRUE)
  expect_match(tab, byele_read, fixed = TRUE)
})

test_that("mapping set references are canonicalized to declared case", {
  model <- .process_tablo(
    tab_file = mutate_tab(paste(
      "Set BLOC (blk1, blk2);",
      "Mapping RB from reg to bloc;",
      "Read (by_elements) RB from file GTAPSETS header \"MBLC\";",
      sep = "\n"
    )),
    quiet = TRUE,
    call = NULL
  )
  map_row <- model[model$type == "Mapping", ]
  expect_identical(map_row$comp1, "REG")
  expect_identical(map_row$comp2, "BLOC")
  expect_true(is.na(map_row$qualifier_list))
})

test_that("mapping-equality sum conditions keep the full RHS in comp2", {
  model <- .process_tablo(
    tab_file = mutate_tab(mapping_block),
    quiet = TRUE,
    call = NULL
  )
  cond <- model[model$type == "Formula" &
    !is.na(model$comp2) & grepl("REGTOBLOC", model$comp2), ]
  expect_identical(
    cond$comp2,
    "sum{r,REG: REGTOBLOC(r)=b, REGW(r)}"
  )
})

test_that("malformed mapping declarations abort", {
  expect_preflight_error("Mapping REGTOBLOC of REG onto BLOC;")
})

test_that("mapping with undeclared sets aborts", {
  expect_preflight_error(paste(
    "Mapping REGTOBLOC from REG to BLOC;",
    byele_read,
    sep = "\n"
  ))
})

test_that("mapping name clashes abort", {
  # variable clash
  expect_preflight_error(paste(
    "Set BLOC (blk1, blk2);",
    "Mapping pop from REG to BLOC;",
    "Read (by_elements) pop from file GTAPSETS header \"MBLC\";",
    sep = "\n"
  ))
  # set clash
  expect_preflight_error(paste(
    "Set BLOC (blk1, blk2);",
    "Mapping BLOC from REG to BLOC;",
    "Read (by_elements) BLOC from file GTAPSETS header \"MBLC\";",
    sep = "\n"
  ))
})

test_that("duplicate mapping declarations abort", {
  expect_preflight_error(paste(
    "Set BLOC (blk1, blk2);",
    "Mapping RB from REG to BLOC;",
    "Mapping RB from REG to BLOC;",
    "Read (by_elements) RB from file GTAPSETS header \"MBLC\";",
    sep = "\n"
  ))
})

test_that("by_elements read of a non-mapping aborts", {
  expect_preflight_error(
    "Read (by_elements) POP from file GTAPDATA header \"POP\";"
  )
})

test_that("plain read of a mapping aborts", {
  expect_preflight_error(paste(
    "Set BLOC (blk1, blk2);",
    "Mapping REGTOBLOC from REG to BLOC;",
    "Read REGTOBLOC from file GTAPSETS header \"MBLC\";",
    sep = "\n"
  ))
})

test_that("mapping without a read aborts", {
  expect_preflight_error(paste(
    "Set BLOC (blk1, blk2);",
    "Mapping REGTOBLOC from REG to BLOC;",
    sep = "\n"
  ))
})

test_that("partial reads still abort with by_elements allowed", {
  expect_preflight_error(
    "Read (LOWER) POP from file GTAPDATA header \"POP\";"
  )
})

test_that("a mapping over a conflicted intersection set aborts", {
  # INTERSECT is evaluated permissively (element-level, stamped by
  # .eval_set_expr on disagreeing origin coverage); the by_elements
  # compose is the one origin-row consumer, so the stamp is fatal here
  model <- tibble::tibble(
    type = c("Mapping", "Read"),
    name = c("RB", "RB"),
    qualifier_list = c(NA, "(by_elements)"),
    comp1 = c("ANX", NA),
    comp2 = c("BLOC", NA),
    header = c(NA, "MBLC"),
    file = c(NA, "GTAPSETS")
  )
  anx_map <- data.table::data.table(origin = "o1", mapping = "x")
  data.table::setattr(anx_map, "origin_conflict", "x")
  bloc_map <- data.table::data.table(
    origin = c("blk1", "blk2"),
    mapping = c("blk1", "blk2")
  )
  sets <- tibble::tibble(
    name = c("ANX", "BLOC"),
    header = c(NA_character_, NA_character_),
    mapping = list(anx_map, bloc_map),
    ele = list("x", c("blk1", "blk2"))
  )
  expect_snapshot_error(
    .finalize_map_data(
      model = model,
      sets = sets,
      set_raw = list(MBLC = "blk1"),
      call = NULL,
      data_call = NULL
    )
  )
})

# --- deploy stage (aggregation compose; needs GTAP data) ---------------

skip_if(!nzchar(dat_input), "GTAP data not available")

conv <- GTAP_convert(dat_input, par_input, set_input)
reg_src <- tolower(as.vector(conv$set$REG))
reg_map <- mappings[["GTAPv12"]][["GTAPv7"]][["REG"]]
reg_agg <- reg_map$big3[match(reg_src, tolower(reg_map$REG))]

map_data <- function(mblc_vals, header = "MBLC") {
  s <- conv$set
  if (!is.null(mblc_vals)) {
    class(mblc_vals) <- c(header, "set", "GTAPv7", "character")
    s[[header]] <- mblc_vals
  }
  suppressMessages(ems_data(
    dat_input = conv$dat,
    par_input = conv$par,
    set_input = s,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  ))
}

consistent_mblc <- ifelse(reg_agg == "usa", "blk2", "blk1")
tab_file <- mutate_tab(mapping_block, name = "mapped.tab")

test_that("a consistent mapping composes under aggregation", {
  nest_temp("map_compose", write_dir)
  d <- map_data(consistent_mblc)
  model <- ems_model(tab_file, closure_file)
  cmf_path <- ems_deploy(d, model, swap_in = "vbloc")
  run_dir <- dirname(cmf_path)

  gtapsets <- readLines(file.path(run_dir, "GTAPSETS.txt"))
  lead <- grep("\"MBLC\"", gtapsets)
  expect_identical(
    gtapsets[lead],
    "3 Strings Length 4 Header \"MBLC\" LongName \"REGTOBLOC mapping\";"
  )
  # deployed REG order is chn, row, usa (sorted): usa is blk2
  expect_identical(gtapsets[lead + 1:3], c("blk1", "blk1", "blk2"))

  metadata <- readRDS(file.path(run_dir, "metadata.rds"))
  expect_true(metadata$mapped_equations)
})

test_that("mapped formulas alone do not set the equation flag", {
  nest_temp("map_fmlflag", write_dir)
  formula_block <- paste(
    "Set BLOC (blk1, blk2);",
    "Mapping (onto) REGTOBLOC from REG to BLOC;",
    byele_read,
    "Coefficient (all,b,BLOC) BTOT(b) # bloc totals #;",
    "Formula (all,b,BLOC) BTOT(b) = 1;",
    sep = "\n"
  )
  d <- map_data(consistent_mblc)
  model <- ems_model(mutate_tab(formula_block, name = "fml.tab"), closure_file)
  cmf_path <- ems_deploy(d, model)
  metadata <- readRDS(file.path(dirname(cmf_path), "metadata.rds"))
  expect_false(metadata$mapped_equations)
})

deploy_error <- function(d, tf = tab_file, ...) {
  model <- ems_model(tf, closure_file)
  ems_deploy(d, model, ...)
}

test_that("mapping header missing from the data aborts", {
  expect_snapshot_error(
    deploy_error(map_data(NULL), swap_in = "vbloc")
  )
})

test_that("mapping header count mismatch aborts", {
  expect_snapshot_error(
    deploy_error(map_data(c("blk1", "blk2")), swap_in = "vbloc")
  )
})

test_that("mapping values outside the codomain abort", {
  bad <- consistent_mblc
  bad[1] <- "blk9"
  expect_snapshot_error(
    deploy_error(map_data(bad), swap_in = "vbloc")
  )
})

test_that("a split mapping under aggregation aborts", {
  split <- consistent_mblc
  split[which(reg_agg == "row")[1]] <- "blk2"
  expect_snapshot_error(
    deploy_error(map_data(split), swap_in = "vbloc")
  )
})

test_that("onto coverage is re-checked on the aggregated sets", {
  uncovered <- rep("blk1", length(reg_src))
  expect_snapshot_error(
    deploy_error(map_data(uncovered), swap_in = "vbloc")
  )
})

# --- e2e solve legs (need a teems image with the mapping solver,
# --- teems-solver M1-M3 @ f3f3a83; run with ems_option_set(docker_tag
# --- = "dev") against a current rebuild) ------------------------------

solver_has_mapping <- function() {
  img <- paste0("teems:", .resolve_docker_tag())
  if (!.docker_image_present(img)) {
    return(FALSE)
  }
  out <- suppressWarnings(system2(
    "docker",
    c(
      "run", "--rm", img, "/bin/bash", "-c",
      shQuote("grep -c by_elements /opt/teems-solver/solver/teems-solver")
    ),
    stdout = TRUE,
    stderr = FALSE
  ))
  length(out) > 0L && !is.na(suppressWarnings(as.integer(out[1]))) &&
    as.integer(out[1]) > 0L
}

skip_if_no_mapping_e2e <- function() {
  skip_if(
    !solver_has_mapping(),
    "teems image absent or predates the mapping solver"
  )
}

test_that("a conditional-sum formula solves with composed mapping values (e2e)", {
  nest_temp("map_e2e_cond", write_dir)
  skip_if_no_mapping_e2e()
  # REGW = 3 per region; big3 composes MBLC to blk1 = {chn, row},
  # blk2 = {usa}: the assertions pin BTOT at 6/3, which only holds if
  # the solver evaluates the condition against the composed header
  cond_block <- paste(
    "Set BLOC (blk1, blk2);",
    "Mapping (onto) REGTOBLOC from REG to BLOC;",
    byele_read,
    "Coefficient (all,r,REG) REGW(r) # per-region weights #;",
    "Formula (all,r,REG) REGW(r) = 3;",
    "Coefficient (all,b,BLOC) BTOT(b) # conditional bloc totals #;",
    "Formula (all,b,BLOC) BTOT(b) = sum{r,REG: REGTOBLOC(r)=b, REGW(r)};",
    "Set B1 (blk1);",
    "Subset B1 is subset of BLOC;",
    "Set B2 (blk2);",
    "Subset B2 is subset of BLOC;",
    "Assertion # Cond Blk1 Six # (all,x,B1) BTOT(x) gt 5.5;",
    "Assertion # Cond Blk1 Six Hi # (all,x,B1) BTOT(x) lt 6.5;",
    "Assertion # Cond Blk2 Three # (all,y,B2) BTOT(y) gt 2.5;",
    "Assertion # Cond Blk2 Three Hi # (all,y,B2) BTOT(y) lt 3.5;",
    sep = "\n"
  )
  d <- map_data(consistent_mblc)
  model <- ems_model(mutate_tab(cond_block, name = "cond.tab"), closure_file)
  cmf_path <- ems_deploy(d, model)
  out <- suppressMessages(ems_solve(cmf_path))
  expect_s3_class(out, "data.frame")
})

test_that("a mapped-index equation solves to the pinned total (e2e)", {
  nest_temp("map_e2e_eq", write_dir)
  skip_if_no_mapping_e2e()
  # vtot = sum{r,REG, vbloc(REGTOBLOC(r))} with vbloc shocked 10
  # uniformly: 3 aggregated regions pick up 10 each
  d <- map_data(consistent_mblc)
  model <- ems_model(tab_file, closure_file)
  cmf_path <- ems_deploy(
    d,
    model,
    shock = ems_uniform_shock(var = "vbloc", value = 10),
    swap_in = "vbloc"
  )
  out <- suppressMessages(ems_solve(cmf_path))
  vtot <- out[out$name == "vtot", ]
  expect_identical(nrow(vtot), 1L)
  expect_equal(
    as.numeric(vtot$dat[[1]][["Value"]]),
    30,
    tolerance = 1e-6
  )
})

test_that("mapped equations force the LU matrix method", {
  nest_temp("map_matsol", write_dir)
  skip_if(
    !.docker_image_present(paste0("teems:", .resolve_docker_tag())),
    "teems image not available"
  )
  d <- map_data(consistent_mblc)
  model <- ems_model(tab_file, closure_file)
  cmf_path <- ems_deploy(d, model, swap_in = "vbloc")
  expect_error(
    ems_solve(cmf_path, matrix_method = "DBBD"),
    "mapped equations"
  )
})

test_that("a non-onto mapping tolerates uncovered codomain elements", {
  nest_temp("map_noonto", write_dir)
  no_onto <- sub("Mapping (onto)", "Mapping", mapping_block, fixed = TRUE)
  uncovered <- rep("blk1", length(reg_src))
  cmf_path <- deploy_error(
    map_data(uncovered),
    tf = mutate_tab(no_onto, name = "noonto.tab"),
    swap_in = "vbloc"
  )
  gtapsets <- readLines(file.path(dirname(cmf_path), "GTAPSETS.txt"))
  lead <- grep("\"MBLC\"", gtapsets)
  expect_identical(gtapsets[lead + 1:3], c("blk1", "blk1", "blk1"))
})

skip_on_cran()

# C0-R (teems-solver docs/mapping_complementarity_design.md section 5):
# levels statements through the R pipeline. The solver linearizes
# Equation (levels) by change differentiation and expands
# Formula&Equation (tab_levels_transform, teems-solver 126698d); R
# splits Formula&Equation into its two 10.9.1 halves at the statement
# stage, parses Equation (levels) qualifiers ahead of the name, and
# mirrors the solver's p_/c_-leading levels-name fatal. Solver ground
# truth: teems-solver/.audit/levels-test-kit (13 legs).

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "tab_levels")
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

levels_block <- paste(
  "Variable (levels) LX # levels percent operand #;",
  "Formula (initial) LX = 2;",
  "Variable (levels) LY;",
  "Formula (initial) LY = 3;",
  "Variable (levels) LZ;",
  "Formula (initial) LZ = 6;",
  "Equation (levels) E_LZ # levels product # LZ = LX * LY;",
  "Variable (change,levels) CA;",
  "Formula (initial) CA = 2;",
  "Variable (change,levels) CB;",
  "Formula (initial) CB = 3;",
  "Variable (change,levels) CC;",
  "Formula (initial) CC = 6;",
  "Equation (levels) E_CC CC = CA * CB;",
  "Variable (change,levels) (all,r,REG) LV(r) # summed operand #;",
  "Formula (initial) (all,r,REG) LV(r) = 1;",
  "Variable (change,levels) LW2 # sum via Formula&Equation #;",
  "Formula&Equation E_LW2 LW2 = sum(r,REG, LV(r));",
  sep = "\n"
)

# --- model stage -------------------------------------------------------

test_that("levels statements parse", {
  model <- .process_tablo(
    tab_file = mutate_tab(levels_block),
    quiet = TRUE,
    call = NULL
  )

  lz <- model[model$type == "Equation" & model$name %in% "E_LZ", ]
  expect_identical(nrow(lz), 1L)
  expect_identical(lz$qualifier_list, "(levels)")
  expect_identical(lz$label, "levels product")

  cc <- model[model$type == "Equation" & model$name %in% "E_CC", ]
  expect_identical(cc$qualifier_list, "(levels)")

  lx <- model[model$type == "Variable" & model$name %in% "LX", ]
  expect_identical(lx$qualifier_list, "(levels)")
  ca <- model[model$type == "Variable" & model$name %in% "CA", ]
  expect_identical(ca$qualifier_list, "(change,levels)")
})

test_that("Formula&Equation expands into its two 10.9.1 halves", {
  model <- .process_tablo(
    tab_file = mutate_tab(levels_block),
    quiet = TRUE,
    call = NULL
  )

  eq <- model[model$type == "Equation" & model$name %in% "E_LW2", ]
  expect_identical(nrow(eq), 1L)
  expect_identical(eq$qualifier_list, "(levels)")

  fm <- model[model$type == "Formula" &
    !is.na(model$comp1) & grepl("^LW2", model$comp1), ]
  expect_identical(nrow(fm), 1L)
  expect_identical(fm$qualifier_list, "(initial)")

  # both halves reach the deployed TAB (the solver re-linearizes the
  # Equation (levels) half)
  tab <- .finalize_tab(model)
  expect_match(tab, "Formula (initial) LW2 = sum(r,REG, LV(r))",
    fixed = TRUE
  )
  expect_match(tab, "Equation (levels) E_LW2 LW2 = sum(r,REG, LV(r))",
    fixed = TRUE
  )
})

test_that("malformed Formula & Equation aborts", {
  expect_preflight_error(
    "Formula & Equation E_BAD (all,r,REG) qgdp(r);"
  )
})

test_that("p_/c_-leading levels variable name aborts", {
  expect_preflight_error(paste(
    "Variable (levels) p_bad # solver scanners cannot carry this #;",
    "Formula (initial) p_bad = 1;",
    sep = "\n"
  ))
})

# --- e2e solve legs (need a teems image with the C0 levels solver,
# --- teems-solver 126698d+; run with ems_option_set(docker_tag =
# --- "dev") against a current rebuild) --------------------------------

solver_has_levels <- function() {
  img <- paste0("teems:", .resolve_docker_tag())
  if (!.docker_image_present(img)) {
    return(FALSE)
  }
  out <- suppressWarnings(system2(
    "docker",
    c(
      "run", "--rm", img, "/bin/bash", "-c",
      shQuote("grep -c 'levels equation' /opt/teems-solver/solver/teems-solver")
    ),
    stdout = TRUE,
    stderr = FALSE
  ))
  length(out) > 0L && !is.na(suppressWarnings(as.integer(out[1]))) &&
    as.integer(out[1]) > 0L
}

skip_if_no_levels_e2e <- function() {
  skip_if(
    !solver_has_levels(),
    "teems image absent or predates the levels solver"
  )
}

skip_if(!nzchar(dat_input), "GTAP data not available")

conv <- GTAP_convert(dat_input, par_input, set_input)

lv_data <- function() {
  suppressMessages(ems_data(
    dat_input = conv$dat,
    par_input = conv$par,
    set_input = conv$set,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  ))
}

test_that("levels equations solve to pinned values (e2e)", {
  nest_temp("levels_e2e", write_dir)
  skip_if_no_levels_e2e()
  # solver-kit values-leg shape: percent product LZ = LX*LY with p_lx
  # shocked 10 -> p_lz = 10 exactly (change diff, constant cofactor);
  # change product CC = CA*CB with c_ca shocked 0.5 -> c_cc = 1.5;
  # F&E sum LW2 over big3 REG with c_lv shocked 1 -> c_lw2 = 3
  d <- lv_data()
  model <- ems_model(mutate_tab(levels_block, name = "levels.tab"), closure_file)
  cmf_path <- ems_deploy(
    d,
    model,
    shock = list(
      ems_uniform_shock(var = "LX", value = 10),
      ems_uniform_shock(var = "CA", value = 0.5),
      ems_uniform_shock(var = "LV", value = 1)
    ),
    swap_in = c("LX", "LY", "CA", "CB", "LV")
  )
  out <- suppressMessages(ems_solve(cmf_path))
  expect_s3_class(out, "data.frame")
  pin <- function(nm) {
    as.numeric(out[tolower(out$name) == tolower(nm), ]$dat[[1]][["Value"]])
  }
  expect_equal(pin("LZ"), 10, tolerance = 1e-6)
  expect_equal(pin("CC"), 1.5, tolerance = 1e-6)
  expect_equal(pin("LW2"), 3, tolerance = 1e-6)
})

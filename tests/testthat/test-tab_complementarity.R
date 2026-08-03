skip_on_cran()

# C1-R + C2-R (teems-solver docs/mapping_complementarity_design.md
# sections 7-8): Complementarity statements through the R pipeline.
# The solver parses the statement, generates the 51.7.2 derived
# statements (comp@e/@d/@l/@u, del_comp@, the E_$comp weight
# coefficients) and solves endogenous components with the C2
# approximate-run state machinery; exogenized components stay inert.
# R mirrors the parse/validation fatals at preflight, counts one
# E_$comp equation element per ACTIVE (endogenous) component in
# .check_system_square, and compose exposes comp@e/@l/@u. Solver
# ground truth: teems-solver/.audit/comp-test-kit (38 checks).

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "tab_comp")
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

expect_preflight_error <- function(text, ...) {
  expect_snapshot_error(
    .process_tablo(tab_file = mutate_tab(text), quiet = TRUE, call = NULL, ...)
  )
}

comp_block <- paste(
  "Variable (levels) CX # comp variable #;",
  "Formula (initial) CX = 2;",
  "Variable (levels) CY # drives the expression #;",
  "Formula (initial) CY = 5;",
  "Complementarity (variable = CX, lower_bound = 0) CMPA CY - 3;",
  sep = "\n"
)

test_that("complementarity statements classify and pass through", {
  model <- .process_tablo(
    tab_file = mutate_tab(comp_block),
    quiet = TRUE,
    call = NULL
  )
  expect_true("Complementarity" %in% model$type)
  tab <- .finalize_tab(model)
  expect_match(tab, "Complementarity (variable = CX, lower_bound = 0) CMPA CY - 3;",
    fixed = TRUE
  )
})

test_that("GMig2-shaped complementarity parses (3-dim, bounds forms)", {
  model <- .process_tablo(
    tab_file = mutate_tab(paste(
      "Variable (change,levels) (all,i,COMM)(all,r,REG)(all,s,REG) NM(i,r,s);",
      "Formula (initial) (all,i,COMM)(all,r,REG)(all,s,REG) NM(i,r,s) = 0;",
      "Variable (change,levels) (all,i,COMM)(all,r,REG)(all,s,REG) RM(i,r,s);",
      "Formula (initial) (all,i,COMM)(all,r,REG)(all,s,REG) RM(i,r,s) = 0;",
      "Complementarity (variable = NM, lower_bound = 0)",
      "E_C_RM (all,i,COMM)(all,r,REG)(all,s,REG)",
      "    RM(i,r,s) - NM(i,r,s);",
      "Variable (levels) BL;",
      "Formula (initial) BL = 1;",
      "Variable (levels) BX;",
      "Formula (initial) BX = 3;",
      "Coefficient (parameter) BP;",
      "Formula (initial) BP = 9;",
      "Complementarity (variable = BX, lower_bound = BL, upper_bound = BP) CMPC BX - BL;",
      sep = "\n"
    )),
    quiet = TRUE,
    call = NULL
  )
  expect_identical(sum(model$type == "Complementarity"), 2L)
})

test_that("malformed complementarity aborts", {
  expect_preflight_error(paste(
    "Variable (levels) CX;",
    "Formula (initial) CX = 2;",
    "Complementarity (variable = CX, wrong_key = 0) CMPA CX - 3;",
    sep = "\n"
  ))
})

test_that("missing variable qualifier aborts", {
  expect_preflight_error(paste(
    "Variable (levels) CY;",
    "Formula (initial) CY = 5;",
    "Complementarity (lower_bound = 0) CMPA CY - 3;",
    sep = "\n"
  ))
})

test_that("non-levels complementarity variable aborts", {
  expect_preflight_error(paste(
    "Variable (levels) CY;",
    "Formula (initial) CY = 5;",
    "Complementarity (variable = qgdp, lower_bound = 0) CMPA CY - 3;",
    sep = "\n"
  ))
})

test_that("missing bound aborts", {
  expect_preflight_error(paste(
    "Variable (levels) CX;",
    "Formula (initial) CX = 2;",
    "Variable (levels) CY;",
    "Formula (initial) CY = 5;",
    "Complementarity (variable = CX) CMPA CY - 3;",
    sep = "\n"
  ))
})

test_that("invalid bound aborts", {
  expect_preflight_error(paste(
    "Coefficient NPB # non-parameter #;",
    "Formula NPB = 1;",
    "Variable (levels) CX;",
    "Formula (initial) CX = 2;",
    "Variable (levels) CY;",
    "Formula (initial) CY = 5;",
    "Complementarity (variable = CX, lower_bound = NPB) CMPA CY - 3;",
    sep = "\n"
  ))
})

test_that("long complementarity name aborts", {
  expect_preflight_error(paste(
    "Variable (levels) CX;",
    "Formula (initial) CX = 2;",
    "Variable (levels) CY;",
    "Formula (initial) CY = 5;",
    "Complementarity (variable = CX, lower_bound = 0) CMPTOOLONGX CY - 3;",
    sep = "\n"
  ))
})

test_that("quantifier count mismatch aborts", {
  expect_preflight_error(paste(
    "Variable (levels) (all,r,REG) QX(r);",
    "Formula (initial) (all,r,REG) QX(r) = 1;",
    "Variable (levels) CY;",
    "Formula (initial) CY = 5;",
    "Complementarity (variable = QX, lower_bound = 0) CMPA CY - 3;",
    sep = "\n"
  ))
})

test_that("condensed complementarity variable aborts", {
  expect_snapshot_error(
    .process_tablo(
      tab_file = mutate_tab(comp_block),
      omit = "CX",
      quiet = TRUE,
      call = NULL
    )
  )
})

# --- deploy-time inert-mode guard (no docker needed) -----------------

skip_if(!nzchar(dat_input), "GTAP data not available")

conv <- GTAP_convert(dat_input, par_input, set_input)

cp_data <- function() {
  suppressMessages(ems_data(
    dat_input = conv$dat,
    par_input = conv$par,
    set_input = conv$set,
    REG = "big3",
    ACTS = "macro_sector",
    ENDW = "labor_agg"
  ))
}

comp_e2e_block <- paste(
  comp_block,
  "Assertion (postsim) # Comp Expr Low # CMPA@e > 2.9999;",
  "Assertion (postsim) # Comp Expr High # CMPA@e < 3.0001;",
  sep = "\n"
)

test_that("endogenous complementarity variable deploys (C2 active mode)", {
  nest_temp("comp_guard", write_dir)
  d <- cp_data()
  model <- ems_model(mutate_tab(comp_block, name = "comp1.tab"), closure_file)
  # CX stays endogenous: the component is ACTIVE (solver C2 state
  # machinery); its E_$comp row squares the count, so the deploy that
  # aborted at C1 now writes files
  cmf_path <- ems_deploy(
    d,
    model,
    swap_in = "CY"
  )
  expect_true(file.exists(cmf_path))
})

test_that("active complementarity components join the squaring count", {
  nest_temp("comp_square", write_dir)
  d <- cp_data()
  model <- ems_model(mutate_tab(comp_block, name = "comp1b.tab"), closure_file)
  # neither CX nor CY exogenized: two endogenous elements against the
  # single E_$comp row of the active component -- not square
  expect_snapshot_error(
    ems_deploy(
      d,
      model
    )
  )
})

test_that("fully exogenous complementarity variable deploys", {
  nest_temp("comp_deploy", write_dir)
  d <- cp_data()
  model <- ems_model(mutate_tab(comp_block, name = "comp2.tab"), closure_file)
  cmf_path <- ems_deploy(
    d,
    model,
    swap_in = c("CX", "CY")
  )
  expect_true(file.exists(cmf_path))
})

# --- e2e solve legs (need a teems image with the C2 solver,
# --- teems-solver ed2b069+; run with ems_option_set(docker_tag =
# --- "dev") against a current rebuild) --------------------------------

solver_has_comp <- function() {
  img <- paste0("teems:", .resolve_docker_tag())
  if (!.docker_image_present(img)) {
    return(FALSE)
  }
  out <- suppressWarnings(system2(
    "docker",
    c(
      "run", "--rm", img, "/bin/bash", "-c",
      shQuote("grep -c 'steps_approx_run' /opt/teems-solver/solver/teems-solver")
    ),
    stdout = TRUE,
    stderr = FALSE
  ))
  length(out) > 0L && !is.na(suppressWarnings(as.integer(out[1]))) &&
    as.integer(out[1]) > 0L
}

test_that("inert complementarity solves with derived values pinned (e2e)", {
  nest_temp("comp_e2e", write_dir)
  skip_if(
    !solver_has_comp(),
    "teems image absent or predates the C2 complementarity solver"
  )
  # solver-kit values-leg shape: CX exogenous (inert), CY shocked 20%
  # (5 -> 6), comp expression CY - 3 tracked by the derived cmpa@e
  # (2 -> 3), pinned by the (postsim) assertions in the TAB -- a wrong
  # value fails the solve
  d <- cp_data()
  model <- ems_model(mutate_tab(comp_e2e_block, name = "comp3.tab"), closure_file)
  cmf_path <- ems_deploy(
    d,
    model,
    shock = ems_uniform_shock(var = "CY", value = 20),
    swap_in = c("CX", "CY")
  )
  out <- suppressMessages(ems_solve(cmf_path))
  expect_s3_class(out, "data.frame")
  # C2 compose exposure: the derived expression variable rides the
  # solution and composes with its accumulated change (levels 2 -> 3
  # under the shock; cmpa@e is a change variable)
  expect_true("cmpa@e" %in% out$name)
  expect_equal(out$dat[["cmpa@e"]]$Value, 1, tolerance = 1e-4)
})

active_block <- paste(
  "Variable (change,levels) AIM # import volume #;",
  "Formula (initial) AIM = 8;",
  "Variable (change,levels) ATQ # power of the quota tariff #;",
  "Formula (initial) ATQ = 1;",
  "Variable (change) ASH # exogenous driver #;",
  "Equation E_AIM  p_AIM = p_ASH - p_ATQ;",
  "Complementarity (variable = ATQ, lower_bound = 1) CMPF 10 - AIM;",
  "Assertion (postsim) # Quota Binds Low # AIM > 9.99;",
  "Assertion (postsim) # Quota Binds High # AIM < 10.01;",
  "Assertion (postsim) # Tariff Absorbs Low # ATQ > 3.39;",
  "Assertion (postsim) # Tariff Absorbs High # ATQ < 3.41;",
  sep = "\n"
)

test_that("active complementarity solves the approximate run (e2e)", {
  nest_temp("comp_e2e_active", write_dir)
  skip_if(
    !solver_has_comp(),
    "teems image absent or predates the C2 complementarity solver"
  )
  # solver-kit cactive-leg shape: ATQ endogenous (ACTIVE), quota 10,
  # driver shocked +4.4 so imports cross the quota mid-run (state
  # 1 -> 2 with a redone step); analytic ends AIM = 10 (Newton pull
  # onto the quota) and ATQ = 1 + 4.4 - 2 = 3.4 (telescoping),
  # pinned by the (postsim) assertions -- a wrong value fails the
  # solve -- and by the composed values here
  d <- cp_data()
  model <- ems_model(mutate_tab(active_block, name = "comp4.tab"), closure_file)
  cmf_path <- ems_deploy(
    d,
    model,
    shock = ems_uniform_shock(var = "ASH", value = 4.4),
    swap_in = "ASH"
  )
  out <- suppressMessages(ems_solve(cmf_path))
  expect_s3_class(out, "data.frame")
  expect_equal(out$dat[["AIM"]]$Value, 2, tolerance = 1e-3)
  expect_equal(out$dat[["ATQ"]]$Value, 2.4, tolerance = 1e-3)
  expect_true("cmpf@e" %in% out$name)
  # expression levels 2 -> 0 (quota exactly met): change -2
  expect_equal(out$dat[["cmpf@e"]]$Value, -2, tolerance = 1e-3)
})

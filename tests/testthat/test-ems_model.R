skip_on_cran()

write_dir <- file.path(tools::R_user_dir("teems", "cache"), "model")
temp_dir <- file.path(write_dir, "tmp")

if (dir.exists(write_dir)) {
  unlink(write_dir, recursive = TRUE)
}

dir.create(temp_dir, recursive = TRUE)
ems_option_set(
  verbose = FALSE,
  tempdir = write_dir
)
withr::defer(ems_option_reset(), teardown_env())

model_files <- ems_example("GTAP-RE", write_dir)
model_file <- model_files[["model_file"]]
closure_file <- model_files[["closure_file"]]

dat_input <- Sys.getenv("GTAP12_dat")
par_input <- Sys.getenv("GTAP12_par")
set_input <- Sys.getenv("GTAP12_set")

dat <- ems_data(
  dat_input,
  par_input,
  set_input,
  REG = "big3",
  ACTS = "macro_sector",
  ENDW = "labor_agg",
  time_steps = c(0, 1, 2)
)

test_that("ems_model requires both model_file and closure_file", {
  expect_snapshot_error(ems_model())
})

test_that("ems_model requires closure_file when only model_file provided", {
  expect_snapshot_error(ems_model(model_file))
})

test_that("ems_model requires model_file when only closure_file provided", {
  expect_snapshot_error(ems_model(closure_file = closure_file))
})

test_that("ems_model rejects non-character model_file", {
  expect_snapshot_error(ems_model(1, closure_file))
})

test_that("ems_model rejects non-character closure_file", {
  expect_snapshot_error(ems_model(model_file, TRUE))
  expect_snapshot_error(ems_model(model_file, 1))
})

test_that("ems_model rejects non-existent model_file file", {
  expect_snapshot_error(ems_model("not_a_file", closure_file))
})

test_that("ems_model rejects non-existent closure_file", {
  expect_snapshot_error(ems_model(model_file, "not_a_file"))
})

test_that("ems_model rejects non-character omit", {
  expect_snapshot_error(ems_model(model_file, closure_file, 1))
})

test_that("ems_model rejects invalid variable names in omit", {
  expect_snapshot_error(ems_model(model_file, closure_file, "not_a_var"))
})

test_that("ems_model rejects invalid coefficient arguments", {
  expect_snapshot_error(ems_model(model_file, closure_file, NOT_A_COEFF = 2))
})

test_that("ems_model returns valid tibble", {
  model <- ems_model(model_file, closure_file)
  expect_s3_class(model, "tbl_df")
  expect_true(all(c("tab", "type", "name", "header", "comp1", "comp2") %in% names(model)))
  expect_true(nrow(model) > 0)
  expect_true(!is.null(attr(model, "tab_file")))
})

test_that("aggregated numeric to a formula", {
  model <- ems_model(
    model_file,
    closure_file,
    KAPPA = 0.54321
  )

  expect_true(model[grepl("KAPPA", model$comp1), ]$comp2 == 0.54321)
  expect_true(grepl("0.54321", model[grepl("KAPPA", model$comp1), ]$tab))
})

test_that("invalid numeric to a formula", {
  expect_snapshot_error(ems_model(
    model_file,
    closure_file,
    KAPPA = c(0, 1)
  ))
})

test_that("aggregated numeric to a read", {
  model <- ems_model(
    model_file,
    closure_file,
    ETRAQ = -4.321
  )

  expect_true(model[grepl("ETRAQ", model$comp1), ]$comp2 == -4.321)
  expect_true(grepl("-4.321", model[grepl("ETRAQ", model$comp1), ]$tab))
})

test_that("aggregated data frame to a read", {
  set.seed(42)
  COMMc <- c("crops", "food", "livestock", "mnfcs", "svces")
  REGr <- c("usa", "chn", "row")
  ALLTIMEt <- seq(0, length(c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16)) - 1)
  SUBPAR <- expand.grid(
    COMMc = COMMc,
    REGr = REGr,
    ALLTIMEt = ALLTIMEt
  )
  SUBPAR$Value <- runif(nrow(SUBPAR))

  model <- ems_model(
    model_file,
    closure_file,
    SUBPAR = SUBPAR
  )

  expect_true(nrow(data.table::fsetdiff(
    data.table::as.data.table(SUBPAR),
    attr(model, "SUBPAR")
  )) == 0)
})

test_that("aggregated csv to a read", {
  set.seed(42)
  COMMc <- c("crops", "food", "livestock", "mnfcs", "svces")
  REGr <- c("usa", "chn", "row")
  ALLTIMEt <- seq(0, length(c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16)) - 1)

  SUBPAR <- expand.grid(
    COMMc = COMMc,
    REGr = REGr,
    ALLTIMEt = ALLTIMEt
  )
  SUBPAR$Value <- runif(nrow(SUBPAR))
  SUBPAR_csv <- file.path(temp_dir, "SUBPAR.csv")
  SUBPAR$Value <- round(SUBPAR$Value, 6)
  write.csv(SUBPAR, SUBPAR_csv, row.names = FALSE)
  model <- ems_model(
    model_file,
    closure_file,
    SUBPAR = SUBPAR_csv
  )

  expect_true(nrow(data.table::fsetdiff(
    data.table::as.data.table(SUBPAR),
    attr(model, "SUBPAR")
  )) == 0)
})

test_that("aggregated data frame to a formula", {
  set.seed(42)
  REGr <- c("usa", "chn", "row")
  ALLTIMEt <- seq(0, length(c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16)) - 1)
  CPHI <- expand.grid(
    REGr = REGr,
    ALLTIMEt = ALLTIMEt
  )

  CPHI$Value <- runif(nrow(CPHI))
  model <- ems_model(
    model_file,
    closure_file,
    CPHI = CPHI
  )

  expect_true(nrow(data.table::fsetdiff(
    data.table::as.data.table(CPHI),
    attr(model, "CPHI")
  )) == 0)
})

test_that("aggregated csv to a formula", {
  set.seed(42)
  REGr <- c("usa", "chn", "row")
  ALLTIMEt <- seq(0, length(c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16)) - 1)
  CPHI <- expand.grid(
    REGr = REGr,
    ALLTIMEt = ALLTIMEt
  )

  CPHI$Value <- runif(nrow(CPHI))
  CPHI_csv <- file.path(temp_dir, "CPHI.csv")
  CPHI$Value <- round(CPHI$Value, 2)
  write.csv(CPHI, CPHI_csv, row.names = FALSE)
  model <- ems_model(
    model_file,
    closure_file,
    CPHI = CPHI_csv
  )

  expect_true(nrow(data.table::fsetdiff(
    data.table::as.data.table(CPHI),
    attr(model, "CPHI")
  )) == 0)
})


test_that("unbalanced PostSim markers", {
  err_model <- write_modified_model(model_file, "POSTSIM (BEGIN) ;")
  expect_snapshot_error(ems_model(err_model, closure_file))
})

test_that("invalid tab statement", {
  err_model <- write_modified_model(model_file, "DISPLAY VKB ;")
  expect_snapshot(ems_model(err_model, closure_file),
                  error = TRUE,
                  transform = function(lines) {
                    gsub(utils::packageVersion("teems"), "version_number", lines)
                  },)
})

test_that("invalid intertemporal header", {
  err_model <- write_modified_model(model_file, "AYRS",
    .fn = function(x, y) gsub("YEAR", y, x)
  )
  expect_snapshot_error(ems_model(err_model, closure_file))
})

test_that("valid custom intertemporal header", {
  err_model <- write_modified_model(model_file, "AYRS",
    .fn = function(x, y) gsub("YEAR", y, x)
  )
  ems_option_set(timestep_header = "AYRS")
  withr::defer(ems_option_set(timestep_header = "YEAR"))
  expect_s3_class(
    ems_model(err_model, closure_file),
    "data.frame"
  )
})

test_that("invalid read statement", {
  err_model <- write_modified_model(model_file, "Read but no file;")
  expect_snapshot_error(ems_model(err_model, closure_file))
})

test_that("a set builder on an undeclared/unread coefficient aborts", {
  err_model <- write_modified_model(
    model_file,
    "Set ENDWM # mobile endowments # = (all,e,ENDW:ENDOWFLAG(e,\"mobile\") ne 0);"
  )
  expect_snapshot_error(ems_model(err_model, closure_file))
})

test_that("set equality (GEMPACK manual 10.1.2.1)", {
  ok_model <- write_modified_model(model_file, "Set ENDWM2 # mobile endowments 2 # = ENDWM;")
  expect_s3_class(
    ems_model(ok_model, closure_file),
    "data.frame"
  )
})

test_that("intertemporal set equality", {
  err_model <- write_modified_model(model_file, "Set ALLTIME2 # all time copy # = ALLTIME;")
  expect_snapshot_error(ems_model(err_model, closure_file))
})

test_that("unparseable set definition", {
  err_model <- write_modified_model(model_file, "Set BADSET # bad definition # = ENDWM ENDWS;")
  expect_snapshot_error(ems_model(err_model, closure_file))
})

test_that("invalid set qualifier", {
  err_model <- write_modified_model(
    model_file,
    "Set (static) ENDTIME # End time step # size 1 (P[NTSP-1]);"
  )
  expect_snapshot_error(ems_model(err_model, closure_file))
})

test_that("multiple set operators", {
  ok_model <- write_modified_model(
    model_file,
    "Set ENDWCFS # multiple op # = ENDWC + ENDWF + ENDWS;"
  )
  expect_no_error(ems_model(ok_model, closure_file))
})

test_that("conditional set builders (GEMPACK manual 10.1.2)", {
  # the accepted shapes parse; the source set becomes the implied
  # superset and the statement is kept verbatim for the solver
  ok_model <- write_modified_model(
    model_file,
    paste(
      'Set COMMX # builder, quoted args # = (all,c,COMM: VDFB(c,"crops","chn","t0") > 0);',
      "Set COMMY # builder, word op # = (all,c,COMM: VDFB(c,\"crops\",\"chn\",\"t0\") GE 0);",
      "Coefficient (all,a,ACTS) ACW(a) # activity weights #;",
      'Read ACW from file GTAPDATA header "ACW";',
      "Set COMMZ # builder, mapping sum # = (all,c,COMM: sum{a,ACTS: MAPCA(a) = c, ACW(a)} > 0);",
      "Mapping MAPCA from ACTS to COMM;",
      'Read (by_elements) MAPCA from file GTAPSETS header "MAPC";',
      sep = "\n"
    )
  )
  model <- ems_model(ok_model, closure_file)
  sb <- model[model$name %in% c("COMMX", "COMMY", "COMMZ"), ]
  expect_equal(sb$comp1, rep("COMM", 3L))
  expect_true(all(grepl("^= \\(all,c,COMM: ", unlist(sb$definition))))
  expect_true(any(grepl('COMMX # builder, quoted args # = (all,c,COMM: VDFB(c,"crops","chn","t0") > 0)', model$tab, fixed = TRUE)))
  comm <- model[which(model$name %in% "COMM"), ]
  expect_true(all(c("COMMX", "COMMY", "COMMZ") %in% comm$subsets[[1]]))

  # unsupported condition shapes are named
  expect_snapshot_error(ems_model(
    write_modified_model(model_file, 'Set BADX = (all,c,COMM: VDFB(c,"crops","chn","t0") > 0 and VDFB(c,"food","chn","t0") > 0);'),
    closure_file
  ))
  expect_snapshot_error(ems_model(
    write_modified_model(model_file, 'Set BADX = (all,c,COMM: VDFB(c,"crops","chn","t0") > VDB(c,"chn","t0"));'),
    closure_file
  ))
  # formula-computed operands cannot drive set resolution (solver fatal)
  expect_snapshot_error(ems_model(
    write_modified_model(model_file, 'Set BADX = (all,c,COMM: VDB(c,"chn","t0") > 0);'),
    closure_file
  ))
  # the source set must be declared
  expect_snapshot_error(ems_model(
    write_modified_model(model_file, 'Set BADX = (all,c,NOSET: VDFB(c,"crops","chn","t0") > 0);'),
    closure_file
  ))
})

test_that("a Set built from an excluded coefficient aborts", {
  # ENDOWFLAG "EFLG" is in the default full_exclude: the upstream
  # GTAPv7 ENDWM/ENDWS builders cannot be evaluated
  err_model <- write_modified_model(
    model_file,
    paste(
      "Set ENDWT # endowment types # (mobile, sluggish);",
      "Coefficient (all,e,ENDW)(all,k,ENDWT) ENDOWFLAG(e,k) # flags #;",
      'Read ENDOWFLAG from file GTAPDATA header "EFLG";',
      'Set ENDWMX # mobile # = (all,e,ENDW: ENDOWFLAG(e,"mobile") ne 0);',
      sep = "\n"
    )
  )
  expect_snapshot_error(ems_model(err_model, closure_file))
})

test_that("set products and $POS are rejected by name", {
  expect_snapshot_error(ems_model(
    write_modified_model(model_file, "Set UNITC (c);\nSet UCOM = UNITC x COMM;"),
    closure_file
  ))
  expect_snapshot_error(ems_model(
    write_modified_model(
      model_file,
      paste(
        "Mapping UCOM2COMM from COMM to COMM;",
        "Formula (all,c,COMM) UCOM2COMM(c) = $POS(c);",
        sep = "\n"
      )
    ),
    closure_file
  ))
})

test_that("set expressions parse (GEMPACK manual 10.1.1.1)", {
  expr_model <- write_modified_model(
    model_file,
    paste(
      "Set TESTA # three-term plus # = ENDWC + ENDWF + ENDWS;",
      "Set TESTB # brackets with intersect # = (ENDW - ENDWC) INTERSECT ENDWMS;",
      'Set TESTC # union with quoted elements # = ENDWF UNION "land" UNION "capital";',
      sep = "\n"
    )
  )
  expect_no_error(ems_model(expr_model, closure_file))
})

test_that("IF in formula RHS (GEMPACK manual 11.4.6)", {
  ok_model <- write_modified_model(
    model_file,
    paste(
      "Coefficient (all,c,COMM)(all,r,REG)(all,t,ALLTIME) IFTESTA(c,r,t) # if in-set #;",
      "Formula (all,c,COMM)(all,r,REG)(all,t,ALLTIME) IFTESTA(c,r,t) = VDB(c,r,t) + IF[c in MARG, VST(c,r,t)];",
      "Coefficient (all,c,COMM)(all,r,REG)(all,t,ALLTIME) IFTESTB(c,r,t) # if comparison #;",
      "Formula (all,c,COMM)(all,r,REG)(all,t,ALLTIME) IFTESTB(c,r,t) = 1 + IF[VDB(c,r,t) gt 0, VDB(c,r,t)];",
      "Coefficient (all,r,REG)(all,t,ALLTIME) IFTESTC(r,t) # if element #;",
      'Formula (all,r,REG)(all,t,ALLTIME) IFTESTC(r,t) = IF[r="chn", VTRPROV(r,t)];',
      sep = "\n"
    )
  )
  model <- ems_model(ok_model, closure_file)
  expect_s3_class(model, "data.frame")
  # in-set and element conditions synthesize intersection sets
  expect_true(all(c("IFS1", "IFS2") %in% model$name))
})

test_that("expression IF conditions (LULC shape, manual 11.4.5/11.4.6)", {
  ok_model <- write_modified_model(
    model_file,
    paste(
      "Coefficient (all,c,COMM)(all,r,REG)(all,t,ALLTIME) IFXT(c,r,t) # expr cond #;",
      "Formula (initial) (all,c,COMM)(all,r,REG)(all,t,ALLTIME) IFXT(c,r,t) = IF[VDB(c,r,t)*VST(c,r,t) <= 0, VDB(c,r,t)] + IF[VDB(c,r,t)*VST(c,r,t) > 0, VST(c,r,t)/[VDB(c,r,t) + 1]];",
      "Coefficient (all,c,COMM)(all,r,REG)(all,t,ALLTIME) IFXU(c,r,t) # two-sided expr cond #;",
      "Formula (all,c,COMM)(all,r,REG)(all,t,ALLTIME) IFXU(c,r,t) = IF[VDB(c,r,t) GE VST(c,r,t) + VDPB(c,r,t), 1];",
      "Variable (all,c,COMM)(all,r,REG)(all,t,ALLTIME) ifxv(c,r,t) # expr cond in an equation #;",
      "Equation E_ifxv (all,c,COMM)(all,r,REG)(all,t,ALLTIME) ifxv(c,r,t) = IF[VDB(c,r,t)*VST(c,r,t) > 0, pds(c,r,t)];",
      sep = "\n"
    )
  )
  model <- ems_model(ok_model, closure_file)
  tab <- model$tab
  # one shared helper for the two <= 0 / > 0 terms, inheriting (initial)
  h1 <- tab[grepl("Formula (initial) (all,c,COMM)(all,r,REG)(all,t,ALLTIME) IFX1(c,r,t) = VDB(c,r,t)*VST(c,r,t)", tab, fixed = TRUE)]
  expect_length(h1, 1L)
  expect_true(any(grepl("(all,t,ALLTIME: IFX1(c,r,t) <= 0) IFXT(c,r,t) = IFXT(c,r,t) + [VDB(c,r,t)]", tab, fixed = TRUE)))
  expect_true(any(grepl("(all,t,ALLTIME: IFX1(c,r,t) > 0) IFXT(c,r,t) = IFXT(c,r,t) + [VST(c,r,t)/[VDB(c,r,t) + 1]]", tab, fixed = TRUE)))
  # two-sided comparison: helper = lhs - rhs against 0, (always) like its host
  h2 <- tab[grepl("Formula (all,c,COMM)(all,r,REG)(all,t,ALLTIME) IFX2(c,r,t) = [VDB(c,r,t)] - [VST(c,r,t) + VDPB(c,r,t)]", tab, fixed = TRUE)]
  expect_length(h2, 1L)
  expect_true(any(grepl("(all,t,ALLTIME: IFX2(c,r,t) >= 0) IFXU(c,r,t)", tab, fixed = TRUE)))
  # an Equation host: (always) helper + the indicator route
  h3 <- tab[grepl("Formula (all,c,COMM)(all,r,REG)(all,t,ALLTIME) IFX3(c,r,t) = VDB(c,r,t)*VST(c,r,t)", tab, fixed = TRUE)]
  expect_length(h3, 1L)
  expect_true(any(grepl("IFX3(c,r,t) > 0", tab[grepl("^Formula .*IFC1", tab)], fixed = TRUE)))
  expect_true(any(grepl("E_ifxv", tab, fixed = TRUE) & grepl("= IFC1(c,r,t) * pds(c,r,t)", tab, fixed = TRUE)))
  expect_true(all(c("IFX1", "IFX2", "IFX3", "IFC1") %in% model$name))

  # a variable inside a condition is named
  expect_snapshot_error(ems_model(
    write_modified_model(
      model_file,
      paste(
        "Variable (all,c,COMM)(all,r,REG)(all,t,ALLTIME) ifbad(c,r,t) # bad #;",
        "Equation E_ifbad (all,c,COMM)(all,r,REG)(all,t,ALLTIME) ifbad(c,r,t) = IF[VDB(c,r,t)*pds(c,r,t) > 0, pds(c,r,t)];",
        sep = "\n"
      )
    ),
    closure_file
  ))
  # compounds stay named
  expect_snapshot_error(ems_model(
    write_modified_model(
      model_file,
      paste(
        "Coefficient (all,c,COMM)(all,r,REG)(all,t,ALLTIME) IFBAD(c,r,t) # bad #;",
        "Formula (all,c,COMM)(all,r,REG)(all,t,ALLTIME) IFBAD(c,r,t) = IF[VDB(c,r,t) > 0 and VST(c,r,t) > 0, VDB(c,r,t)];",
        sep = "\n"
      )
    ),
    closure_file
  ))
})

test_that("unsupported IF placement", {
  err_model <- write_modified_model(
    model_file,
    paste(
      "Coefficient (all,r,REG)(all,t,ALLTIME) IFBAD(r,t) # bad if #;",
      "Formula (all,r,REG)(all,t,ALLTIME) IFBAD(r,t) = 2 * IF[r in REG, VTRPROV(r,t)];",
      sep = "\n"
    )
  )
  expect_snapshot_error(ems_model(err_model, closure_file))
})

test_that("coefficient-vs-coefficient IF conditions parse", {
  ok_model <- write_modified_model(
    model_file,
    paste(
      "Coefficient (all,r,REG)(all,t,ALLTIME) IFCC(r,t) # coefref vs coefref #;",
      "Formula (all,r,REG)(all,t,ALLTIME) IFCC(r,t) = IF[VTRPROV(r,t) gt VT(t), VTRPROV(r,t)];",
      sep = "\n"
    )
  )
  model <- ems_model(ok_model, closure_file)
  expect_true(any(grepl("IFX1(r,t) = [VTRPROV(r,t)] - [VT(t)]", model$tab, fixed = TRUE)))
  expect_true(any(grepl("(all,t,ALLTIME: IFX1(r,t) > 0) IFCC(r,t)", model$tab, fixed = TRUE)))
})

test_that("IF in equation RHS (GEMPACK manual 11.4.7)", {
  ok_model <- write_modified_model(
    model_file,
    paste(
      "Variable (all,c,COMM)(all,r,REG)(all,t,ALLTIME) iftest(c,r,t) # if test var #;",
      "Equation E_iftest # in-set split # (all,c,COMM)(all,r,REG)(all,t,ALLTIME) iftest(c,r,t) = pds(c,r,t) + IF[c in MARG, qst(c,r,t)];",
      "Variable (all,c,COMM)(all,r,REG)(all,t,ALLTIME) iftest2(c,r,t) # if test var 2 #;",
      "Equation E_iftest2 # comparison indicator # (all,c,COMM)(all,r,REG)(all,t,ALLTIME) iftest2(c,r,t) = IF[VDB(c,r,t) gt 0, pds(c,r,t)];",
      sep = "\n"
    )
  )
  model <- ems_model(ok_model, closure_file)
  expect_s3_class(model, "data.frame")
  # membership IF splits the equation; comparison IF synthesizes an indicator
  expect_true(all(c("E_iftestA", "E_iftestB", "IFC1") %in% model$name))
})

test_that("multiple membership IF conditions in an equation", {
  err_model <- write_modified_model(
    model_file,
    paste(
      "Variable (all,c,COMM)(all,r,REG)(all,t,ALLTIME) iftest(c,r,t) # if test var #;",
      "Equation E_iftest # bad # (all,c,COMM)(all,r,REG)(all,t,ALLTIME) iftest(c,r,t) = IF[c in MARG, qst(c,r,t)] + IF[r in REG, pds(c,r,t)];",
      sep = "\n"
    )
  )
  expect_snapshot_error(ems_model(err_model, closure_file))
})

test_that("same-index membership IF partition in an equation (GTAPv7 E_CNTqfr shape)", {
  ok_model <- write_modified_model(
    model_file,
    paste(
      "Variable (all,c,COMM)(all,r,REG)(all,t,ALLTIME) iftest(c,r,t) # if test var #;",
      paste0(
        "Equation E_iftest # partition # (all,c,COMM)(all,r,REG)(all,t,ALLTIME) iftest(c,r,t) = ",
        'IF[c="crops", pds(c,r,t)] + IF[c in MARG, qst(c,r,t)] - IF[c="svces", 2*pds(c,r,t)];'
      ),
      sep = "\n"
    )
  )
  model <- ems_model(ok_model, closure_file)
  expect_s3_class(model, "data.frame")
  # one equation per membership term over its intersect set plus the
  # remainder over COMM - (S1 + S2 + S3)
  expect_true(all(paste0("E_iftest", c("A", "B", "C", "D")) %in% model$name))
  eqs <- model$tab[grepl("^Equation E_iftest[A-D] ", model$tab)]
  expect_length(eqs, 4L)
  expect_match(eqs[1], "[pds(c,r,t)]", fixed = TRUE)
  expect_no_match(eqs[1], "qst", fixed = TRUE)
  expect_match(eqs[2], "[qst(c,r,t)]", fixed = TRUE)
  expect_match(eqs[3], "- [2*pds(c,r,t)]", fixed = TRUE)
  expect_no_match(eqs[4], "pds|qst")
  rem <- model$tab[grepl("if-rewrite COMM minus \\(", model$tab)]
  expect_length(rem, 1L)
  expect_match(rem, "= COMM - (IFS", fixed = TRUE)
})

test_that("netcut inflation warning (roadmap 6.5 E1)", {
  warn_model <- write_modified_model(
    model_file,
    "Equation E_nctest # netcut probe # (all,e,ENDW)(all,a,ACTS)(all,r,REG)(all,t,FWDTIME) qfe(e,a,r,t+1) = qfe(e,a,r,t);"
  )
  expect_snapshot_warning(ems_model(warn_model, closure_file))
})

test_that("minimal intertemporal variables raise no netcut warning", {
  # shipped GTAP-RE lead/lag variables (kb, pinv, rental) are REG x ALLTIME
  expect_no_warning(ems_model(model_file, closure_file))
})

test_that("netcut proxy rewrite (roadmap 6.5 E2)", {
  fix_model <- write_modified_model(
    model_file,
    paste(
      "Variable (all,r,REG)(all,t,FWDTIME) nctv(r,t) # probe #;",
      paste0(
        "Equation E_nctv # probe # (all,r,REG)(all,t,FWDTIME) nctv(r,t) = ",
        'qfe("capital","svces",r,t+1) - 0.5*qfe("capital","svces",r,t+1) - ',
        '0.5*qfe("capital","crops",r,t+1);'
      ),
      sep = "\n"
    )
  )
  expect_snapshot(model <- ems_model(fix_model, closure_file))
  # one proxy per distinct element slice; repeated references share it
  expect_true(all(c("NCV1", "E_NCV1", "NCV2", "E_NCV2") %in% model$name))
  probe <- model$tab[grepl("E_nctv ", model$tab, fixed = TRUE)]
  expect_match(probe, "NCV1(r,t+1)", fixed = TRUE)
  expect_match(probe, "NCV2(r,t+1)", fixed = TRUE)
  expect_no_match(probe, "qfe", fixed = TRUE)
  # the rewritten model is minimal: no netcut warning remains
  expect_no_warning(ems_model(fix_model, closure_file))
})

test_that("partial read statement", {
  err_model <- write_modified_model(
    model_file,
    'READ (all,i,COM) INTINP(i,"wool") FROM FILE params ; ! a partial read !'
  )
  expect_snapshot_error(ems_model(err_model, closure_file))
})

test_that("data frame input missing a set", {
  set.seed(42)
  REGr <- c("usa", "chn", "row")
  ALLTIMEt <- seq(0, length(c(0, 1, 2, 3, 4, 6, 8, 10, 12, 14, 16)) - 1)
  SUBPAR <- expand.grid(
    REGr = REGr,
    ALLTIMEt = ALLTIMEt
  )
  SUBPAR$Value <- runif(nrow(SUBPAR))

  expect_snapshot_error(ems_model(
    model_file,
    closure_file,
    SUBPAR = SUBPAR
  ))
})

test_that("invalid var in closure", {
  err_closure <- write_modified_closure(
    closure_file,
    "not_a_var"
  )

  expect_snapshot_error(ems_model(model_file, err_closure))
})

test_that("closure missing exo/endo spec", {
  err_closure <- readLines(closure_file)
  err_closure <- tail(err_closure, -1)
  err_file <- file.path(dirname(closure_file), "error.cls")
  writeLines(err_closure, err_file)
  expect_snapshot_error(ems_model(model_file, err_file))
})

test_that("ems_model errors when invalid closure mixed entry present preswap", {
  mod_closure <- readLines(closure_file)
  mod_closure <- mod_closure[-1]
  mod_closure <- mod_closure[mod_closure != "pop"]
  append_cls <- c("exogenous", 'pop("row",ALLTIME)', 'pop("chn",ALLTIME)', 'pop("zzz",ALLTIME)')
  mod_closure <- c(append_cls, mod_closure)
  invalid_cls <- file.path(dirname(closure_file), "invalid.cls")
  writeLines(mod_closure, invalid_cls)
  model <- ems_model(model_file, invalid_cls)
  nest_temp("invalid_closure", write_dir)
  expect_snapshot_error(ems_deploy(dat, model))
})

test_that("ems_model errors when invalid closure subset entry present preswap", {
  mod_closure <- readLines(closure_file)
  mod_closure <- mod_closure[-1]
  mod_closure <- mod_closure[mod_closure != "qe(ENDWC,REG,INITIME)"]
  append_cls <- c("exogenous", "qe(COMM,REG,INITIME)")
  mod_closure <- c(append_cls, mod_closure)
  invalid_cls <- file.path(dirname(closure_file), "invalid.cls")
  writeLines(mod_closure, invalid_cls)

  model <- ems_model(model_file, invalid_cls)
  nest_temp("invalid_closure2", write_dir)
  expect_snapshot_error(ems_deploy(dat, model))
})

test_that("ems_model errors when invalid closure pure element entry present preswap", {
  mod_closure <- readLines(closure_file)
  mod_closure <- mod_closure[-1]
  mod_closure <- mod_closure[mod_closure != "pop"]
  append_cls <- c(
    "exogenous",
    'pop("row","0")', 'pop("row","1")', 'pop("row","2")',
    'pop("chn","0")', 'pop("chn","1")', 'pop("chn","2")',
    'pop("usa","0")', 'pop("usa","1")', 'pop("zzz","2")'
  )
  mod_closure <- c(append_cls, mod_closure)
  invalid_cls <- file.path(dirname(closure_file), "invalid.cls")
  writeLines(mod_closure, invalid_cls)

  model <- ems_model(model_file, invalid_cls)
  nest_temp("invalid_closure3", write_dir)
  expect_snapshot_error(ems_deploy(dat, model))
})

test_that("ems_model errors when duplicate closure entry present preswap", {
  mod_closure <- readLines(closure_file)
  mod_closure <- mod_closure[-1]
  append_cls <- c("exogenous", "pop")
  mod_closure <- c(append_cls, mod_closure)
  invalid_cls <- file.path(dirname(closure_file), "invalid.cls")
  writeLines(mod_closure, invalid_cls)

  model <- ems_model(model_file, invalid_cls)
  nest_temp("invalid_closure4", write_dir)
  expect_snapshot_error(ems_deploy(dat, model))
})

test_that("ems_model errors dots passed without names", {
  expect_snapshot_error(
    ems_model(model_file,
      closure_file,
      omit = NULL,
      backsolve = NULL,
      auto_omit = FALSE,
      ignore_condense = FALSE,
      1
    )
  )
})

test_that("ems_model examples run", {
  # Simple static model retrieval and load
  GTAPv7 <- ems_example("GTAPv7", write_dir)
  model <- ems_model(GTAPv7[["model_file"]], GTAPv7[["closure_file"]])
  
  # Retrieve intertemporal model
  GTAP_RE <- ems_example("GTAP-RE", write_dir)
  expect_s3_class(model, "tbl_df")
  # Construct data frame
  sectors <- c("crops", "food", "livestock", "mnfcs", "svces")
  regions <- c("usa", "chn", "row")
  time_steps <- 0:5

  SUBPAR <- expand.grid(
    COMMc = sectors,
    REGr = regions,
    ALLTIMEt = time_steps
  )
  SUBPAR$Value <- runif(nrow(SUBPAR))
  # Model load with:
  # 1) variable omission
  # 2) uniform numeric value applied to KAPPA coefficient
  # 3) heterogeneous values allocated to SUBPAR via data frame
  model <- ems_model(
    model_file = GTAP_RE[["model_file"]],
    closure_file = GTAP_RE[["closure_file"]],
    omit = c("atall", "avaall", "tfe", "tfm", "tgd", "tgm", "tid", "tim"),
    KAPPA = 0.03,
    SUBPAR = SUBPAR
  )
  expect_s3_class(model, "tbl_df")
})

# --- condensation (roadmap 6.2: omit / backsolve, GEMPACK 10.16 & 14.1.10) ---

condense_graft <- paste(
  "Variable (all,r,REG)(all,t,ALLTIME) tva(r,t) # test var A #;",
  "Variable (all,r,REG)(all,t,ALLTIME) tvb(r,t) # test var B #;",
  "Variable (all,t,ALLTIME) tvc(t) # test var C #;",
  "Variable (all,t,ALLTIME) tve(t) # test var E #;",
  "Variable (all,t,ALLTIME) tvd(t) # test var D #;",
  "Equation E_tva (all,r,REG)(all,t,ALLTIME) tva(r,t) = 2*qgdp(r,t) + pop(r,t);",
  "Equation E_tvb (all,r,REG)(all,t,ALLTIME) tvb(r,t) = 3*tva(r,t);",
  "Equation E_tvc (all,t,ALLTIME) tvc(t) = sum{r,REG, GDP(r,t)*tva(r,t)};",
  "Equation E_tve (all,t,ALLTIME) tve(t) = walras_sup(t);",
  "Equation E_tvd (all,t,ALLTIME) tvd(t) = sum{r,REG, GDP(r,t)*tve(t)};",
  sep = "\n"
)

test_that("backsolve rewrites referencing equations and retains the defining equation", {
  bs_model <- write_modified_model(model_file, condense_graft)
  model <- ems_model(bs_model, closure_file, backsolve = "tva")

  var_row <- model[model$type == "Variable" & model$name %in% "tva", ]
  expect_identical(var_row$condense, "backsolve")
  expect_identical(var_row$condense_eq, "E_tva")
  eq_row <- model[model$type == "Equation" & model$name %in% "E_tva", ]
  expect_identical(eq_row$condense, "backsolve")

  # hand-verifiable rewrite: tvb(r,t) = 3*[2*qgdp(r,t) + pop(r,t)]
  e_tvb <- model$tab[model$type == "Equation" & model$name %in% "E_tvb"]
  expect_match(e_tvb, "tvb(r,t) = 3*2*qgdp(r,t) + 3*pop(r,t);", fixed = TRUE)

  # substitution under a sum whose index the variable carries stays in place
  e_tvc <- model$tab[model$type == "Equation" & model$name %in% "E_tvc"]
  expect_match(e_tvc, "sum{r,REG, GDP(r,t)*2*qgdp(r,t)}", fixed = TRUE)
  expect_match(e_tvc, "sum{r,REG, GDP(r,t)*pop(r,t)}", fixed = TRUE)

  # no residual reference outside the retained defining equation
  eqs <- model[model$type == "Equation", ]
  hits <- grepl("(?<![[:alnum:]_])tva(?![[:alnum:]_])", eqs$tab, perl = TRUE)
  expect_identical(eqs$name[hits], "E_tva")

  # deployed TAB: Backsolve statement present, declaration retained
  tab <- teems:::.finalize_tab(model)
  expect_match(tab, "Backsolve tva using E_tva ;", fixed = TRUE)
  expect_match(tab, "tva(r,t) # test var A #", fixed = TRUE)
})

test_that("substitution inverts idle sums onto synthesized coefficients", {
  bs_model <- write_modified_model(model_file, condense_graft)
  model <- ems_model(bs_model, closure_file, backsolve = "tve")

  # sum{r,REG, GDP(r,t)*tve(t)} -> CSUB1(t)*walras_sup(t),
  # CSUB1(t) = sum{r,REG, GDP(r,t)} (GEMPACK 14.1.12 pattern)
  e_tvd <- model$tab[model$type == "Equation" & model$name %in% "E_tvd"]
  expect_match(e_tvd, "tvd(t) = CSUB1(t)*walras_sup(t);", fixed = TRUE)
  csub_formula <- model$tab[model$type == "Formula" & grepl("CSUB1", model$tab)]
  expect_match(csub_formula, "CSUB1(t) = sum{r,REG, GDP(r,t)}", fixed = TRUE)
})

test_that("backsolve through a coefficient pivot synthesizes a reciprocal and warns", {
  expect_snapshot_warning(
    model <- ems_model(model_file, closure_file, backsolve = "qgdp")
  )
  csub_rows <- model$tab[model$type == "Formula" & grepl("CSUB", model$tab)]
  expect_true(any(grepl("= GDP(r,t)", csub_rows, fixed = TRUE)))
  # every other equation is qgdp-free
  eqs <- model[model$type == "Equation", ]
  hits <- grepl("(?<![[:alnum:]_])qgdp(?![[:alnum:]_])", eqs$tab, perl = TRUE)
  expect_identical(eqs$name[hits], "E_qgdp")
})

test_that("in-TAB Omit statements are honored and stripped", {
  omit_model <- write_modified_model(model_file, "Omit atall avaall ;")
  model <- ems_model(omit_model, closure_file)
  flagged <- model$name[model$condense %in% "omit"]
  expect_setequal(flagged, c("atall", "avaall"))
  # references zeroed, statement stripped, declaration row retained
  expect_false(any(grepl("atall\\(", model$tab[model$type == "Equation"])))
  expect_false(any(grepl("^Omit", model$tab)))
  expect_true("atall" %in% model$name[model$type == "Variable"])
  tab <- teems:::.finalize_tab(model)
  expect_false(grepl("(?<![[:alnum:]_])atall(?![[:alnum:]_])", tab, perl = TRUE))
})

test_that("in-TAB Substitute executes as backsolve with a message", {
  sub_model <- write_modified_model(
    model_file,
    paste(condense_graft, "Substitute tva using E_tva ;", sep = "\n")
  )
  expect_snapshot(model <- ems_model(sub_model, closure_file))
  expect_identical(
    model$condense[model$type == "Variable" & model$name %in% "tva"],
    "backsolve"
  )
})

test_that("ignore_condense disables in-TAB condensation statements", {
  omit_model <- write_modified_model(model_file, "Omit atall avaall ;")
  model <- ems_model(omit_model, closure_file, ignore_condense = TRUE)
  expect_true(all(is.na(model$condense)))
  expect_true(any(grepl("atall\\(", model$tab[model$type == "Equation"])))
})

test_that("ems_model rejects invalid variable names in backsolve", {
  expect_snapshot_error(ems_model(model_file, closure_file, backsolve = "not_a_var"))
})

test_that("ems_model rejects invalid equation names in backsolve", {
  expect_snapshot_error(
    ems_model(model_file, closure_file, backsolve = c(qgdp = "E_not_real"))
  )
})

test_that("ems_model rejects unresolvable backsolve entries", {
  # pop has no E_pop defining equation
  expect_snapshot_error(ems_model(model_file, closure_file, backsolve = "pop"))
})

test_that("ems_model rejects conflicting condensation actions", {
  bs_model <- write_modified_model(model_file, condense_graft)
  expect_snapshot_error(
    ems_model(bs_model, closure_file, omit = "tva", backsolve = "tva")
  )
})

test_that("ems_model rejects a reused backsolve equation", {
  bs_model <- write_modified_model(model_file, condense_graft)
  expect_snapshot_error(
    ems_model(bs_model, closure_file, backsolve = c(tva = "E_tva", tvb = "E_tva"))
  )
})

test_that("backsolve rule violations abort (GEMPACK 14.1.10)", {
  rule_graft <- paste(
    "Variable (all,r,REG)(all,t,ALLTIME) tvr(r,t) # rule test var #;",
    "Variable (all,t,ALLTIME) tvc2(t) # rule test var #;",
    "Variable (all,t,ALLTIME) tvc3(t) # rule test var #;",
    "Variable (all,c,COMM)(all,t,ALLTIME) tvm(c,t) # rule test var #;",
    "Variable (all,r,REG)(all,s,REG)(all,t,ALLTIME) tvrr(r,s,t) # rule test var #;",
    "Equation E_tr1 (all,t,ALLTIME) tvr(\"usa\",t) = walras_sup(t);",
    "Equation E_tr2 (all,t,ALLTIME) tvc2(t) = sum{r,REG, tvr(r,t)};",
    "Equation E_tr3 (all,r,REG)(all,t,ALLTIME) tvc3(t) = pop(r,t);",
    "Equation E_tr4 (all,m,MARG)(all,t,ALLTIME) tvm(m,t) = walras_sup(t);",
    "Equation E_tr5 (all,r,REG)(all,t,ALLTIME) tvrr(r,r,t) = pop(r,t);",
    "Equation E_tr6 (all,r,REG)(all,t,ALLTIME) tvr(r,t+1) = pop(r,t);",
    "Equation E_tr7 (all,r,REG)(all,s,REG)(all,t,ALLTIME) tvrr(r,s,t) = tvrr(s,r,t) + pop(r,t);",
    "Equation E_trc (all,r,REG)(all,t,ALLTIME) tvr(r,t) = tvr(r,t) + pop(r,t);",
    sep = "\n"
  )
  rule_model <- write_modified_model(model_file, rule_graft)

  # rule 1: element argument
  expect_snapshot_error(ems_model(rule_model, closure_file, backsolve = c(tvr = "E_tr1")))
  # rule 2: SUM index as argument
  expect_snapshot_error(ems_model(rule_model, closure_file, backsolve = c(tvr = "E_tr2")))
  # rule 3: equation ALL index absent from the occurrence
  expect_snapshot_error(ems_model(rule_model, closure_file, backsolve = c(tvc3 = "E_tr3")))
  # rule 4: quantifier ranges over a subset of the declared set
  expect_snapshot_error(ems_model(rule_model, closure_file, backsolve = c(tvm = "E_tr4")))
  # rule 5: repeated index within one occurrence
  expect_snapshot_error(ems_model(rule_model, closure_file, backsolve = c(tvrr = "E_tr5")))
  # rule 6: lead/lag offset argument
  expect_snapshot_error(ems_model(rule_model, closure_file, backsolve = c(tvr = "E_tr6")))
  # rule 7: two occurrences with different index patterns
  expect_snapshot_error(ems_model(rule_model, closure_file, backsolve = c(tvrr = "E_tr7")))
  # occurrences cancel: no expression obtainable
  expect_snapshot_error(ems_model(rule_model, closure_file, backsolve = c(tvr = "E_trc")))
})

test_that("backsolved variables must be endogenous in the closure", {
  endo_graft <- paste(
    "Variable (all,r,REG)(all,t,ALLTIME) tvz(r,t) # closure test var #;",
    "Equation E_tvp (all,r,REG)(all,t,ALLTIME) pop(r,t) = tvz(r,t);",
    sep = "\n"
  )
  endo_model <- write_modified_model(model_file, endo_graft)
  expect_snapshot_error(
    ems_model(endo_model, closure_file, backsolve = c(pop = "E_tvp"))
  )
})

test_that("omitted variables must be exogenous in the closure", {
  expect_snapshot_error(ems_model(model_file, closure_file, omit = "qgdp"))
})

test_that("swaps and shocks on condensed variables abort", {
  model <- ems_model(model_file, closure_file, omit = "atall")
  nest_temp("condensed_guard", write_dir)
  expect_snapshot_error(
    ems_deploy(dat, model, swap_in = "atall", swap_out = "pop")
  )
  expect_snapshot_error(
    ems_deploy(dat, model, shock = ems_uniform_shock("atall", 1))
  )
})

test_that("GTAP standard condensation condenses cleanly", {
  # gtapv7.sti (corpus 12102.zip) standard condensation, minus the four
  # regional-aggregate CNT* variables absent from the teems GTAPv7 variant
  std_omit <- c(
    "atall", "avaall", "tfe", "tfd", "tfm", "tgd", "tgm",
    "tpdall", "tpmall", "tid", "tim"
  )
  std_backsolve <- c(
    "pfactreal", "CNTqpm", "CNTqfd", "CNTqfm", "qfe", "CNTqim", "pfd",
    "qia", "qtmfsd", "qfd", "CNTqfe", "c2_cr", "ptrans", "atmfsd", "afa",
    "qca", "qfa", "pca", "qfm", "compvalad", "qgd", "pfa", "pes", "pgd",
    "ps", "CNTtech_ava", "CNTendw", "qint", "CNTqe", "pmds", "pcif",
    "qpd", "CNTqpd", "CNTqid", "c3_cr", "CNTqms", "qim", "CNTtech_ao",
    "CNTqca", "qxs", "c1_cr", "CNTqfeer", "qmw", "ppm", "qpm",
    "CNTtech_ams", "CNTqgd", "pfm", "qva", "pfob", "pint", "pfe",
    "CNTalleffcr", "CNTtech_af", "CNTtech_aint", "CNTtech_afe", "afe",
    "qid", "pva", "qgm", "CNTqo", "CNTtech_atmfsd", "CNTqgm", "ppd",
    "pgm", "CNTqxs", "qpev", "aint"
  )

  GTAPv7 <- ems_example("GTAPv7", write_dir)
  suppressWarnings(
    model <- ems_model(GTAPv7[["model_file"]], GTAPv7[["closure_file"]],
      omit = std_omit, backsolve = std_backsolve
    )
  )

  var_flags <- model[model$type == "Variable" & !is.na(model$condense), ]
  expect_identical(sum(var_flags$condense == "omit"), length(std_omit))
  expect_identical(sum(var_flags$condense == "backsolve"), length(std_backsolve))

  # no equation other than the retained defining equation references a
  # backsolved variable; retained equations reference survivors only
  eqs <- model[model$type == "Equation", ]
  for (v in std_backsolve) {
    def_eq <- model$condense_eq[model$type == "Variable" & model$name %in% v]
    hit <- grepl(paste0("(?<![[:alnum:]_])", v, "(?![[:alnum:]_])"), eqs$tab,
      perl = TRUE, ignore.case = TRUE
    )
    expect_identical(setdiff(eqs$name[hit], def_eq), character(0))
  }

  # deployed TAB carries one Backsolve statement per substitution
  tab <- teems:::.finalize_tab(model)
  expect_identical(
    sum(grepl("^Backsolve ", strsplit(tab, "\n")[[1]])),
    length(std_backsolve)
  )
})

unlink(tools::R_user_dir("teems", "cache"), recursive = TRUE)
test_that("ems_model rejects a non-logical auto_omit", {
  expect_snapshot_error(ems_model(model_file, closure_file, auto_omit = NA))
})

library(usethis)

fun <- list.files("./data-raw/R", full.names = TRUE)
lapply(fun, source)

# databases
db_version <- c("GTAPv9", "GTAPv10", "GTAPv11", "GTAPv12")
vetted_db_versions <- c("GTAPv9A", "GTAPv10A", "GTAPv11a", "GTAPv11c", "GTAPv12", "GTAPv12a")
data_format <- c("GTAPv6", "GTAPv7")

# models
dir <- list.dirs(
  "../teems-models",
  recursive = FALSE
)

dir <- dir[basename(dir) != ".git"]
file.copy(dir, "./inst/models/", overwrite = TRUE, recursive = TRUE)

tab_qual <- c(
  "nonzero_by_zero",
  "zero_by_zero",
  "\\bchange\\b",
  "linear",
  "orig_level",
  "ge -?[.[:digit:]]",
  "gt -?[.[:digit:]]",
  "le -?[.[:digit:]]",
  "lt -?[.[:digit:]]",
  "integer",
  "parameter",
  "initial",
  "\\breal\\b",
  "levels"
)

supported_state <- c(
  "File", "Coefficient", "Read", "Update", "Set", "Subset",
  "Formula", "Assertion", "Variable", "Equation", "Write",
  "Zerodivide", "Omit", "Substitute", "Backsolve", "Postsim",
  "Mapping"
)

# vocabularies mirrored from teems-solver src (keep in sync; row
# inventory in dev/validation_table.md):
# reserved identifier names: tab_parse.c names_validate (manual 11.2.1)
tab_reserved_words <- c(
  "sum", "if", "abs", "max", "min", "sqrt", "exp", "loge", "log10",
  "id01", "id0v", "round", "trunc0", "truncb", "prod", "maxs", "mins",
  "random", "normal", "cumnormal", "lognormal", "cumlognormal",
  "gperf", "gperfc", "ras_matrix", "all"
)
# variable/coefficient qualifier accept-lists: tab_parse.c
# tab_qualifiers_parse (manual 10.3/10.4); *_prefixes match
# "token=value" forms; bounds (ge/gt/le/lt <number>) are handled
# separately
tab_var_qualifiers <- c("change", "percent_change", "linear", "levels")
tab_var_qualifier_prefixes <- c("orig_level=", "vpqtype=")
tab_coef_qualifiers <- c("real", "integer", "parameter", "non_parameter")
# Default-statement value vocabulary: cmf_io.c tab_defaults_validate
# (manual 10.19); values outside these lists are individually
# diagnosed (bound defaults, levels, add_homotopy) or unknown
tab_default_values <- list(
  coefficient = c("parameter", "non_parameter"),
  variable = c("linear", "levels", "change", "percent_change"),
  formula = c("initial", "always"),
  equation = c("linear", "not_add_homotopy")
)

ignored_state <- character(0)

invalid_state <- c(
  "Loop", "Display", "Break", "Cycle", "Complementarity", "Transfer"
)

# mappings
mapping_files <- list.files(
  "../teems-mappings",
  pattern = "\\.csv",
  recursive = TRUE,
  full.names = TRUE
)

mappings <- process_mappings(
  mapping_files = mapping_files,
  db_version = db_version,
  data_format = data_format
)

# parameters
GTAPv6_weights <- list(
  ESBD = c("VDPA", "VIPA", "VDGA", "VIGA", "VDFA", "VIFA"),
  ESBM = c("VIPA", "VIGA", "VIFA"),
  ESBT = c("VDFM", "VIFM", "VFM", "FTRV", "-FBEP", "-ISEP"),
  ESBV = "EVFA",
  INCP = c("VDPA", "VIPA"),
  SUBP = c("VDPA", "VIPA")
)

GTAPv7_weights <- list(
  ESBD = c("VDPP", "VMPP", "VMGP", "VDGP", "VDFP", "VMFP"),
  ESBM = c("VMPP", "VMGP", "VMFP"),
  ESBT = c("VDFB", "VMFB", "EVFB", "FTRV", "-FBEP", "-ISEP"),
  ESBV = "EVFP",
  INCP = c("VDPP", "VMPP"),
  SUBP = c("VDPP", "VMPP"),
  ESBC = c("VDFB", "VMFB")
)

param_weights <- list(GTAPv6 = GTAPv6_weights, GTAPv7 = GTAPv7_weights)
set_conversion <- data.frame(
  GTAPv6name = c(
    "REG", "TRAD_COMM", "MARG_COMM", "NMRG_COMM", "CGDS_COMM", "PROD_COMM",
    "ENDW_COMM", "DEMD_COMM", "ENDWS_COMM", "ENDWM_COMM", "ENDWC_COMM",
    "NSAV_COMM", NA, NA
  ),
  GTAPv6header = c("H1", "H2", "MARG", NA, "H9", "H5", "H6", "H4", "H7", "H8", NA, "H3", NA, NA),
  GTAPv6description = c(
    "regions in the model", "traded commodities", "margin commodities",
    "non-margin commodities", "capital goods commodities", "produced commodities",
    "endowment commodities", "demanded commodities", "sluggish endowment commodities",
    "mobile endowment commodities", "capital endowment commodity",
    "non-savings commodities", NA, NA
  ),
  GTAPv7name = c(
    "REG", "COMM", "MARG", "NMRG", NA, "ACTS", "ENDW", "DEMD", "ENDWS",
    "ENDWM", "ENDWC", NA, "ENDWF", "ENDWMS"
  ),
  GTAPv7header = c("REG", "COMM", "MARG", NA, NA, "ACTS", "ENDW", NA, "ENDS", "ENDM", NA, NA, NA, NA),
  GTAPv7description = c(
    "regions", "commodities", "margin commodities", "non-margin commodities",
    NA, "Activities", "endowments", "commodities and endowments",
    "sluggish endowment", "mobile endowments", "capital endowment",
    NA, "Sector-specific endowment", "mobile and sluggish endowments"
  ),
  stringsAsFactors = FALSE
)

coeff_conversion <- data.frame(
  GTAPv6header = c(
    "ADRV", "EVFA", "EVOA", "FBEP", "FTRV", "ISEP", "MFRV", "OSEP", "POP", "PURV",
    "SAVE", "TFRV", "TVOM", "VDEP", "VDFA", "VDFM", "VDGA", "VDGM", "VDPA", "VDPM",
    "VFM", "VIFA", "VIFM", "VIGA", "VIGM", "VIMS", "VIPA", "VIPM", "VIWS", "VKB",
    "VRRV", "VST", "VTSS", "VTWR", "VXMD", "VXWD", "XTRV",
    NA, NA, NA, NA, NA, NA, NA,
    "ESBD", "ESBM", "ESBT", "ESBV", "ETRE", "INCP", "SUBP", "RDLT", "RFLX", "SLUG",
    NA, NA, NA, NA, NA, NA
  ),
  GTAPv6description = c(
    "anti-dumping duty",
    "primary factor purchases, at agents' prices",
    "primary factor sales, at agents' prices",
    "gross factor-based subsidies",
    "gross factor employment tax revenue",
    "net intermediate input subsidies",
    "export tax equivalent of MFA quota premia",
    "net ordinary output subsidy",
    "population",
    "export tax equivalent of price undertakings",
    "net saving, by region",
    "ordinary import duty",
    "sales of domestic product, at market prices",
    "capital depreciation",
    "domestic purchases, by firms, at agents' prices",
    "domestic purchases, by firms, at market prices",
    "domestic purchases, by government, at agents' prices",
    "domestic purchases, by government, at market prices",
    "domestic purchases, by households, at agents' prices",
    "domestic purchases, by households, at market prices",
    "primary factor purchases, by firms, at market prices",
    "import purchases, by firms, at agents' prices",
    "import purchases, by firms, at market prices",
    "import purchases, by government, at agents' prices",
    "import purchases, by government, at market prices",
    "imports, at market prices",
    "import purchases, by households, at agents' prices",
    "import purchases, by households, at market prices",
    "imports, at world prices",
    "capital stock",
    "export tax equivalent of voluntary export restraints",
    "margin exports",
    "Import tariff Rev by type of tariffs paid",
    "margins by margin commodity",
    "non-margin exports, at market prices",
    "non-margin exports, at world prices",
    "ordinary export tax",
    NA, NA, NA, NA, NA, NA, NA,
    "Armington CES for domestic/imported allocation",
    "Armington CES for regional allocation of imports",
    "Elasticity of intermediate input substitution",
    "CES between primary factors in production",
    "CET between sectors for sluggish primary factors",
    "CDE expansion parameter",
    "CDE substitution parameter",
    "Investment allocation binary coefficient",
    "Expected rate of return flexibility parameter",
    "Binary parameter for factor mobility: 1=sluggish 0=mobile",
    NA, NA, NA, NA, NA, NA
  ),
  GTAPv7header = c(
    "ADRV", "EVFP", "EVOS", "FBEP", "FTRV", "ISEP", "MFRV", NA, "POP", "PURV",
    "SAVE", "TFRV", "VOSB", "VDEP", "VDFP", "VDFB", "VDGP", "VDGB", "VDPP", "VDPB",
    "EVFB", "VMFP", "VMFB", "VMGP", "VMGB", "VMSB", "VMPP", "VMPB", "VCIF", "VKB",
    "VRRV", "VST", "VMTS", "VTWR", "VXSB", "VFOB", "XTRV",
    "VDIB", "VDIP", "VMIB", "VMIP", "CSEP", "MAKS", "MAKB",
    "ESBD", "ESBM", "ESBT", "ESBV", "ETRE", "INCP", "SUBP", "RDLT", "RFLX", NA,
    "ESBG", "ETRQ", "ESBS", "ESBC", "ESBQ", "ESBI"
  ),
  GTAPv7description = c(
    "anti-dumping duty",
    "primary factor purchases, at producer prices",
    "primary factor sales, at supply (post-income tax) prices",
    "gross factor-based subsidies",
    "gross factor employment tax revenue",
    "net investment input subsidies",
    "export tax equivalent of MFA quota premia",
    "net ordinary output subsidy",
    "population",
    "export tax equivalent of price undertakings",
    "net saving, by region",
    "ordinary import duty",
    "sales of domestic product, at basic prices",
    "capital depreciation",
    "domestic purchases, by firms, at producer prices",
    "domestic purchases, by firms, at basic prices",
    "domestic purchases, by government, at producer prices",
    "domestic purchases, by government, at basic prices",
    "domestic purchases, by households, at producer prices",
    "domestic purchases, by households, at basic prices",
    "primary factor purchases, by firms, at basic prices",
    "import purchases, by firms, at producer prices",
    "import purchases, by firms, at basic prices",
    "import purchases, by government, at producer prices",
    "import purchases, by government, at basic prices",
    "imports, at basic prices",
    "import purchases, by households, at producer prices",
    "import purchases, by households, at basic prices",
    "imports, at CIF prices",
    "capital stock",
    "export tax equivalent of voluntary export restraints",
    "margin exports",
    "Import tariff Rev by type of tariffs paid",
    "margins by margin commodity",
    "non-margin exports, at basic prices",
    "non-margin exports, at FOB prices",
    "ordinary export tax",
    "domestic purchases, by investment, at basic prices",
    "domestic purchases, by investment, at producer prices",
    "import purchases, by investment, at basic prices",
    "import purchases, by investment, at producer prices",
    "net intermediate input subsidies",
    "multi-production ('make') matrix at supply prices",
    "multi-production ('make') matrix at basic prices",
    "Armington CES for domestic/imported allocation",
    "Armington CES for regional allocation of imports",
    "CES between primary factors and intermediate inputs",
    "CES between primary factors in production",
    "CET between sectors for sluggish primary factors",
    "CDE expansion parameter",
    "CDE substitution parameter",
    "Investment allocation binary coefficient",
    "Expected rate of return flexibility parameter",
    NA,
    "CES elasticity of substitution for government demands",
    "CET elasticity of transformation for commodities produced by",
    "CES elasticity of substitution for international transport margin services",
    "CES elasticity of substitution for intermediate inputs",
    "1/CES elasticity for commodity sourcing",
    "Investment expenditure CES elasticity"
  ),
  GTAPv6set = c(
    rep(NA, 44),
    "TRAD_COMM", "TRAD_COMM", "PROD_COMM", "PROD_COMM", "ENDWS_COMM",
    "TRAD_COMM, REG", "TRAD_COMM, REG", NA, "REG", NA,
    NA, NA, NA, NA, NA, NA
  ),
  GTAPv7set = c(
    rep(NA, 44),
    "COMM, REG", "COMM, REG", "ACTS, REG", "ACTS, REG", "ENDW, REG",
    "COMM, REG", "COMM, REG", NA, "REG", NA,
    "REG", "ACTS, REG", "MARG", "ACTS, REG", "COMM, REG", "REG"
  ),
  data_type = c(rep("dat", 44), rep("par", 16)),
  stringsAsFactors = FALSE
)

coeff_conversion$GTAPv6set <- strsplit(coeff_conversion$GTAPv6set, ", ")
coeff_conversion$GTAPv7set <- strsplit(coeff_conversion$GTAPv7set, ", ")

# messages (definitions in data-raw/R/msg_*.R) -------------------------
cls_err <- build_cls_err()
compose_err <- build_compose_err()
convert_wrn <- build_convert_wrn()
data_err <- build_data_err()
data_info <- build_data_info()
data_wrn <- build_data_wrn()
deploy_err <- build_deploy_err()
exp_err <- build_exp_err()
gen_err <- build_gen_err()
gen_info <- build_gen_info()
model_err <- build_model_err()
model_info <- build_model_info()
model_wrn <- build_model_wrn()
probe_err <- build_probe_err()
probe_info <- build_probe_info()
shk_err <- build_shk_err()
solve_err <- build_solve_err()
solve_info <- build_solve_info()
solve_wrn <- build_solve_wrn()
swap_err <- build_swap_err()
solver_error_map <- build_solver_error_map()

usethis::use_data(
  vetted_db_versions,
  mappings,
  tab_qual,
  supported_state,
  ignored_state,
  invalid_state,
  tab_reserved_words,
  tab_var_qualifiers,
  tab_var_qualifier_prefixes,
  tab_coef_qualifiers,
  tab_default_values,
  param_weights,
  set_conversion,
  coeff_conversion,
  cls_err,
  compose_err,
  convert_wrn,
  data_err,
  data_info,
  data_wrn,
  deploy_err,
  exp_err,
  gen_err,
  gen_info,
  model_err,
  model_info,
  model_wrn,
  probe_err,
  probe_info,
  shk_err,
  solve_err,
  solve_info,
  solve_wrn,
  swap_err,
  solver_error_map,
  overwrite = TRUE,
  internal = TRUE,
  compress = "gzip"
)

# teems (development version)
* Coefficients return through the solver's binary coefficient dump (`sol.cof`/`sol.cbin`) instead of per-coefficient CSV files: `ems_compose()` reads them selectively and exactly (the CSVs carried six fixed decimals); `ems_deploy(write_coefficients = TRUE)` restores the CSV `Write` pairs
* `ems_solve()` checks the solver's exit status in addition to scanning its log
* `ems_model(auto_omit = TRUE)` omits unshocked, wholly exogenous variables at deploy time
* `ems_solve()` reports when a condensed deployment meets a bordered method, or is intertemporal
* `ems_probe()` returns a condensation verdict read from the measured block structure
* Deploy metadata records the condensation state and the exogenous/variable element counts

# teems 0.1.1
* `ems_example()` example typo fixed
* verbatim tests for all exported function examples added

# teems 0.1.0
* `write_dir` removed from `ems_deploy()`, default write directory now tempdir() and can be overridden via `tempdir` arg in `ems_option_set()`
* Explicit `path` first arg for `ems_example()`, with no default
* `ems_example()` now accepts outputs from `GTAP_convert()`
* `GTAP_convert()` inputs now har-specific
* Expanded and more descriptive examples

# teems 0.0.5
* Officially submitted to CRAN
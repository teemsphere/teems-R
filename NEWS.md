# teems 0.0.0.98

## Bug fixes
* Messy closure files now correctly parsed, omission via ! added
* Tablo statement check logic updated to fix file, outFile confusion
* stderr now properly captured in outfile and console output
* Directory creation with ems_deploy() `write_dir` now safely nested within additional folder
* Disparate header/coefficient situation with intertemporal headers fixed
* Variable/coefficient set parsing error on messy tabs fixed

## Other changes
* Can now accommodate coefficients, variables, and sets of any combination of lower and upper case
* File (new) are automatically dropped from Tablo declarations
* Write declarations automatically dropped from Tablo declarations
* Binary switches to select set elements now flagged at processing
* Coefficient and variable set order in declaration can now differ from actual set order
* ems_get_option() `full_exclude` now also used to exclude on Tablo files, removing e.g., DVER
* Tablo statement syntax now case insensitive
* Gaps in set specification (all,r,  REG) now handled
* NEWS added and to be maintain by release
* Intertemporal header checks now conducted on `n_timestep_header` and `timestep_header` (see ?ems_get_option)

# teems 0.0.0.97 (initial beta release)
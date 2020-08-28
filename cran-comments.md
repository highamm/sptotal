## Patch Notes

- August 28, 2020: updated `slmfit` to work with the `tibble` object.

## Resubmission Comments

- We have added the author, year, and DOI number for the relevant methods used in the package in the DESCRIPTION field of the DESCRIPTION file.

- We have removed some files in the /inst folder that were unnecessary to the package. Files removed from the /inst folder include `function_coordinates.R` and `OR_biomass_data_example.R`, both of which were listed under author "Paco." None  of the code in these files was used in constructing the package. Therefore, the files were dropped.

- One of the other files removed from the /inst folder was the `US_lakes_example_code.R` file, which had been writing to the user's directory.

- We have reset the user's `par()` in lines 85 and 100 of the vignette code to the user's original `options()`. 

## Test Environments

- local test on Mac OS X (`R` 4.0.0)
- Ubuntu 16.04.6 on Travis CI (`R` 4.0.0)
- win-builder, both the development and release versions of `R`
- R-hub `check_for_cran()` tests

## Check Results for R CMD

- There were no ERRORs or WARNINGs.
- There is one NOTE that this is the first time the `sptotal` package is being submitted to CRAN.

## Downstream dependencies

- There are currently no downstream dependencies for this package.

Thank you!



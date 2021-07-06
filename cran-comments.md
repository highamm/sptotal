## Summary of New Changes

* the `stratafit()` and `predict.stratafit()` functions have been added so that FPBK can be performed easily for different strata, allowing each stratum to have different fixed effects and different covariance parameters for each stratum. 

* `check.variogram()`, `get.predinfo()`, and `get.predplot()` have been deprecated to `plot.slmfit()`, `print.predict.slmfit()`, and `plot.predict.slmfit()` generics, respectively.

* functionality for `sp` and `sf` point geometry objects to be used as a `data` argument in `slmfit()` has been added.

## Test Environments

- local test on Mac OS X (`R` 4.1.0)
- Ubuntu 16.04.6 on Travis CI (`R` 4.1.0)
- win-builder, both the development and release versions of `R` with `devtools::check_win_release()` and `devtools::check_win_devel()`

## Check Results for R CMD

- There were no ERRORs, WARNINGs, or NOTEs.

## Downstream dependencies

- There are currently no downstream dependencies for this package.

Thank you!



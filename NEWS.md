# sptotal 1.0.0

## Major Changes

* deprecated `check.variogram()` to `plot.slmfit()` generic.

* deprecated `get.predinfo()` to `print.predict.slmfit()` generic.

* deprecated `get.predplot()` for `plot.predict.slmfit()` generic.

* added the `stratafit()`, `predict.stratafit()`, and related `summary()` and `print()` generics to easily perform FPBK for different strata, allowing both different fixed effects and different covariance parameters for each stratum.

* added functionality for `sp` and `sf` point geometry objects to be used as a `data` argument in `slmfit()`.

* added variogram function `sv()` to allow for more options in the empirical variogram.

* added normalized residuals as an option to the `residuals.slmfit()` generic.

* added fitted value extraction via the `fitted.slmfit()` generic.

# sptotal 0.1.0

## Bug Fixes and Improvements

* removed unnecessary dependencies on `matrixcalc` and `mvtnorm` packages.

* added error message for collinear predictors.

* added `print()` generics for `predict.slmfit()` and `slmfit()`.

* changed UTM to TM in package functions and documentation.

# sptotal 0.0.2

## Bug Fixes and Improvements

* Fixed an issue with using a `tibble` object instead of a `data.frame` object.

* Updated error message for collinear predictors.

* Changed a warning about converting characters to factors to a message.

* Removed unnecessary package dependencies with `sp`, `matrixcalc` and `mvtnorm` and moved `tibble` from Imports to Suggests.

# sptotal 0.0.1

* This is the first release of sptotal.

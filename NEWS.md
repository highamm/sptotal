# sptotal 0.1.0

## Bug Fixes Improvements

* created plot generics and deprecated the old plotting functions.

* changed UTM to TM in package functions and documentation.

* added the `stratafit()`, `predict.stratafit()`, and related `summary()` and `print()` generics to easily perform FPBK for different strata, allowing both different fixed effects and different covariance parameters for each stratum.

# sptotal 0.0.2

## Bug Fixes and Improvements

* Fixed an issue with using a `tibble` object instead of a `data.frame` object.

* Updated error message for collinear predictors.

* Changed a warning about converting characters to factors to a message.

* Removed unnecessary package dependencies with `sp`, `matrixcalc` and `mvtnorm` and moved `tibble` from Imports to Suggests.

# sptotal 0.0.1

* This is the first release of sptotal.

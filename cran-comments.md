## Summary of New Changes

* two bug fixes, one for normalized residual computation and one for fixed effect standard error estimation.

## Test Environments

- local test on Mac OS X (`R` 4.2.1).
- R Hub with `devtools::check_rhub()` on December 10.
- win-builder, both the development and release versions of `R` with `devtools::check_win_release()` and `devtools::check_win_devel()` on December 10.

## Check Results for R CMD

- There is one NOTE that there is a new maintainer for the package. Matt Higham is still the maintainer: I just corrected to the proper `person("First Name", "Last Name", ...)` order in the `DESCRIPTION` file.
- There is an additional NOTE under the Fedora Linux build on R Hub that "Skipping checking HTML validation: no command 'tidy' found." After searching online, I could not find anything I could do about this NOTE because I cannot update tidy on the external Fedora Linux server.

## Downstream dependencies

- There are currently no downstream dependencies for this package.

Thank you!



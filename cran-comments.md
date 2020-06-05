Check through all output and help files and "change" counts to "response"

Jay Comments

\code{\link{function}}
 to link to other functions

Add tests for spherical, Gaussian
Add test for ML
Add test for covestimates and estmethod = "None"
 
HELP FILE COMMENTS



7. Actually, ORbiomass is a SpatialPointsDataFrame object from sp package.  In the description, it says the response variable is density, but there is no "density" in the @data object.  The response variable is BIOT_ha, and the other two are predictor variables.  It is a complete census of 2788 samples, so can be used to test methods by creating missing data, as was done for simdata.  Also, the coordinates are contained in a slot called coords, and they are labelled "LON" and "LAT", but they are clearly not longitude and latitude. Maybe Bryce can check on this.  Also, we need to be VERY CAREFUL to give credit for data.  People can be very touchy about that.  Again, maybe Bryce can help (through Temesgen, probably).  You will see that I have cleaned up the help files for the AKmoose and USlakes data sets, and added their source.

## Test Environments

- local test on Mac OS X (`R` 4.0.0)
- Ubuntu 16.04.6 on Travis CI (`R` 4.0.0)
- win-builder, both the development and release versions of `R`

## Check Results for R CMD

- There were no ERRORs or WARNINGs.
- There is one NOTE that this is the first time the `sptotal` package is being submitted to CRAN.

## Downstream dependencies

- There are currently no downstream dependencies for this package.

Thank you!



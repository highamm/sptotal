Jay Comments

\code{\link{function}}
 to link to other functions
 
HELP FILE COMMENTS



7. Actually, ORbiomass is a SpatialPointsDataFrame object from sp package.  In the description, it says the response variable is density, but there is no "density" in the @data object.  The response variable is BIOT_ha, and the other two are predictor variables.  It is a complete census of 2788 samples, so can be used to test methods by creating missing data, as was done for simdata.  Also, the coordinates are contained in a slot called coords, and they are labelled "LON" and "LAT", but they are clearly not longitude and latitude. Maybe Bryce can check on this.  Also, we need to be VERY CAREFUL to give credit for data.  People can be very touchy about that.  Again, maybe Bryce can help (through Temesgen, probably).  You will see that I have cleaned up the help files for the AKmoose and USlakes data sets, and added their source.

VIGNETTE COMMENTS

1. Show some totals from the simulated data, so we know the "truth."

We have simulated the data for the whole population. This is convenient, because we know the true means and totals.

> mean(simdata[,'Z'])
[1] 12.08581
> sum(simdata[,'Z'])
[1] 4834.326
> sum(simdata[,'wts2']*simdata[,'Z'])
[1] 273.3751

However, we will now sample from this population to provide a more realistic setting where we can measure...

2.    Also, is estimating the mean, the Total Observed is not as meaningful as Mean Observed.  Maybe you should give both.

3. Make a connection back to the "truth," so the reader can verify it is working.

We predict a total of 4817 units in this simulated region with 90% confidence bounds of (4779, 4856). The confidence interval is fairly small because we simulated data that were highly correlated, increasing precision in prediction for unobserved sites.  You can see that the prediction of the total is close to the true value of 4834.326, and the true value is within the confidence interval.

4. I can't see any variation in colors in the open circles. It would be better to choose a different shape, and make it solid.  Also, they should all be bigger.  There is too much white space between the dots.

5. I think that we should get away from using "counts" as a default.  Two of our other data sets, forest biomass and lake disolved organic content, are not counts.  We want this package to be useful for something other than moose surveys.  Please check throughout, and change the labeling of the color ramp in the default predplot.

6. For the Section "Prediction for a Quantity Other Than the Total", show the output, and compare it to the truth.  The ability to do small area estimation is very important, and needs to be featured.

7. First line in  Section "Moose Abundance from Aerial Surveys", TM is not defined.

8. For the box,
xy <- LLtoUTM(mean(centroids$x), centroids$y, centroids$x)$xy
We need to tell the user's a little bit about the arguments, especially the first one.  TM is based on minimizing distortion from a central meridian, so using the mean of longitude is setting that as the central meridian.

9. I feel like the user would like a map of raw data as part of the vignette, to give them a sense of the data. Maybe right before slmfit_out_moose,

plot(moose_df[is.na(moose_df$total),c('x','y')], pch = 19)
text(moose_df[!is.na(moose_df$total),c('x','y')],
labels = moose_df[!is.na(moose_df$total),'total'], cex = .8)

10. Again, I can't see any variation in the open circles when using
get.predplot(pred_moose)

12. We don't have to highlight every function in the vignette, but residuals() is a pretty import one.  It might be useful to add one or both of these sets,

hist(residuals(slmfit_out1))
hist(residuals(slmfit_out1, cross.validation = TRUE))

hist(residuals(slmfit_out_moose))
hist(residuals(slmfit_out_moose, cross.validation = TRUE))



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



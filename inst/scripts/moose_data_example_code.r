#' Fits a Spatial Linear Model
#'
#' Estimates regression coefficients and spatial autocorrelation
#' parameters.
#'
#' @param formula is an R linear model formula specifying density as the
#' response variable as well as covariates for predicting densities on the unsampled sites.
#' @param data is the data set with the response column of densities, the covariates to
#' be used for the block kriging, and the spatial coordinates for all of the sites.
#' @param xcoordcol is the name of the column in the data frame with x coordinates or longitudinal coordinates
#' @param ycoordcol is the name of the column in the data frame with y coordinates or latitudinal coordinates
#' @param CorModel is the covariance structure. By default, \code{CorModel} is
#' Exponential but other options include the Spherical and Gaussian.
#' @param coordtype specifies whether spatial coordinates are in latitude, longitude (\code{LatLon}) form or UTM (\code{UTM}) form.
#' @return a list with \itemize{
#'   \item the spatial covariance estimates
#'   \item the regression coefficient estimates
#'   \item a list containing \enumerate{
#'        \item formula
#'        \item data
#'        \item xcoordcol
#'        \item ycoordcol
#'        \item CorModel
#'        \item Inverted covariance matrix on the sampled sites
#'        \item Covariance matrix on all sites
#'        }
#' }
#' @import stats
#' @export slmfit

plot.sptotalPredOut = function(predictions, nbreaks = 4, 
  breakMethod = 'quantile', legend.cex = 1, ...)
{
  # use layout to create partitioned plot so we can add color legend to left
  layout(matrix(1:2, nrow = 1), width = c(3,1))
  # create breaks according to user-specified breakMethod
  if(breakMethod == 'quantile') {
    probs = (1:nbreaks)/(nbreaks + 1)
    brks = c(min(predictions$Pred_df$preddensity) - 1e-10,
      quantile(predictions$Pred_df$preddensity, probs = probs),
      max(predictions$Pred_df$preddensity) + 1e-10)
  }
  if(breakMethod == 'even') {
    rang = max(predictions$Pred_df$preddensity) + 1e-10 -
    min(predictions$Pred_df$preddensity) - 1e-10
    brks = c(min(predictions$Pred_df$preddensity) - 1e-10,
      min(predictions$Pred_df$preddensity) - 1e-10 + 
        rang*(1:nbreaks)/(nbreaks + 1),
      max(predictions$Pred_df$preddensity) + 1e-10)
  }
  # cut predictions at breakpoints to create a vector of factors and labels
  cuts = cut(predictions$Pred_df$preddensity, breaks = brks)
  # create a color palette
  palette = viridis(length(levels(cuts)))
  # create plot of predictions colored by their prediction values
  par(mar = c(5,5,1,1))
  plot(predictions$Pred_df$xcoords, predictions$Pred_df$ycoords, pch = 19,
    col = palette[as.integer(cuts)], xlab = 'xcoords',
    ylab = 'ycoords', ...)
  par(mar = c(5,0,1,1))
  # add the legend embedded in a second invisible plot
  plot(c(0,1),c(0,1), type = 'n', xlab = '', ylab = '', xaxt = 'n', 
    yaxt = 'n', bty='n')
  legend(.1, .9, legend = levels(cuts), col = palette,
    pch = rep(19, times = length(palette)), cex = legend.cex)
}

plot.sptotalPredOut(predout1, nbreaks = 20, breakMethod = 'even', cex = 2,
  legend.cex = 1.4)

library(splmm)
d2 = d1
d2L = d2[d2$STRAT=='L',]
d2M = d2[d2$STRAT=='M',]
coordinates(d2) <- ~ x + y
coordinates(d2L) <- ~ x + y
coordinates(d2M) <- ~ x + y

splmmout1 = splmm(TOTAL ~ 1, d2) 
splmmout2 = splmm(TOTAL ~ STRAT, d2) 
splmmout3 = splmm(TOTAL ~ 1, d2L) 
splmmout4 = splmm(TOTAL ~ 1, d2M) 

slmfit_out1$SpatialParmEsts
splmmout1$theta
slmfit_out2$SpatialParmEsts
splmmout2$theta
slmfit_out3$SpatialParmEsts
splmmout3$theta
slmfit_out4$SpatialParmEsts
splmmout4$theta

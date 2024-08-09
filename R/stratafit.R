#' Fits a Separate Spatial Linear Model for Each Stratum
#'
#' Estimates regression coefficients and spatial autocorrelation
#' parameters, given spatial coordinates, a model formula, and a
#' stratification variable. Arguments are the same here as they are
#' for \code{\link{slmfit}()}, with an extra argument for
#' \code{stratacol}, the name of the stratification column. Note that
#' stratum can either by incorporated as a covariate in
#' \code{\link{slmfit}()}, in which case the errors have the same
#' spatial covariance, or, models with differing spatial covariances
#' for the errors can be fit to each level of stratum, as is done here
#' in \code{stratafit()}.
#'
#' @param formula is an \code{R} linear model formula specifying the
#' response variable as well as covariates for predicting the
#' response on the unsampled sites.
#' @param data is the data set with the response column, the covariates to
#' be used for the block kriging, and the spatial coordinates for
#' all of the sites.
#' @param xcoordcol is the name of the column in the data frame with
#' x coordinates or longitudinal coordinates
#' @param ycoordcol is the name of the column in the data frame with
#' y coordinates or latitudinal coordinates
#' @param stratacol is the name of the stratification column
#' @param areacol is the name of the column with the areas of the sites.
#' By default, we assume that all sites have equal area, in which
#' case a vector of 1's is used as the areas.
#' @param CorModel is the covariance structure. By default, \code{CorModel} is
#' Exponential but other options include the Spherical and Gaussian.
#' @param estmethod is either the default \code{"REML"} for restricted
#' maximum likelihood to estimate the covariance parameters and
#' regression coefficients or \code{"ML"} to estimate the covariance
#' parameters and regression coefficients.
#' @return a list of class \code{slmfit} with \itemize{
#'   \item the spatial covariance estimates
#'   \item the regression coefficient estimates
#'   \item the covariance matrix of the fixed effects
#'   \item minus two times the log-likelihood of the model
#'   \item the names of the predictors
#'   \item the sample size
#'   \item the name of the covariance model used
#'   \item a vector of residuals
#'   \item the design matrix
#'   \item a vector of the sampled densities
#'   \item a list containing \enumerate{
#'        \item formula, the model formula
#'        \item data, the data set input as the \code{data} argument
#'        \item xcoordcol, the name of the x-coordinate column
#'        \item ycoordcol, the name of the y-coordinate column
#'        \item estmethod, either REML or ML
#'        \item CorModel, the correlation model used
#'        \item estimated covariance matrix of all sites
#'        \item Inverted covariance matrix on the sampled sites
#'        \item the vector of areas.
#'        }
#' }
#' @examples
#' data(exampledataset) ## load a toy data set
#' exampledataset$strata <- c(rep("A", 19), rep("B", 21))
#' strataobj <- stratafit(formula = counts ~ pred1 + pred2,
#'  data = exampledataset, stratacol = "strata",
#' xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
#' summary(strataobj)
#' @export stratafit

stratafit <- function(formula, data, xcoordcol, ycoordcol,
                      stratacol = NULL, areacol = NULL,
                       CorModel = "Exponential", estmethod = "REML") {

  both_df <- split(data, data[[stratacol]])
  slm_outs <- lapply(both_df, FUN = slmfit,
                     formula = formula,
                     xcoordcol = xcoordcol, ycoordcol = ycoordcol,
                     areacol = areacol,
                     CorModel = CorModel, estmethod = estmethod)
  class(slm_outs) <- "stratafit"
  return(slm_outs)
}

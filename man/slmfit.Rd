% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/slmfit.R
\name{slmfit}
\alias{slmfit}
\title{Fits a Spatial Linear Model}
\usage{
slmfit(
  formula,
  data,
  xcoordcol,
  ycoordcol,
  areacol = NULL,
  stratacol = NULL,
  CorModel = "Exponential",
  estmethod = "REML",
  covestimates = c(NA, NA, NA)
)
}
\arguments{
\item{formula}{is an \code{R} linear model formula specifying the
response variable as well as covariates for predicting the
response on the unsampled sites.}

\item{data}{is a data frame or tibble with the response column,
the covariates to be used for the block kriging, and the
spatial coordinates for all of the sites. Alternatively, data can be
an \code{sp} Spatial Points Data Frame or \code{sf} object with
POINT geometry.}

\item{xcoordcol}{is the name of the column in the data frame with
x coordinates or longitudinal coordinates}

\item{ycoordcol}{is the name of the column in the data frame with
y coordinates or latitudinal coordinates}

\item{areacol}{is the name of the column with the areas of the sites.
By default, we assume that all sites have equal area, in which
case a vector of 1's is used as the areas.}

\item{stratacol}{is the name of the the column with the
stratification variable, if strata are to be fit separately,
with different covariance parameter estimates.}

\item{CorModel}{is the covariance structure. By default,
\code{CorModel} is Exponential but other options include the
Spherical and Gaussian.}

\item{estmethod}{is either the default \code{"REML"} for restricted
maximum likelihood to estimate the covariance parameters and
regression coefficients or \code{"ML"} to estimate the covariance
parameters and regression coefficients. This argument can also be set to
\code{"None"}, in which case \code{covestimates} must be provided.}

\item{covestimates}{is an optional vector of covariance
parameter estimates (nugget, partial sill, range). If these are
given and \code{estmethod = "None"}, the the provided vector are
treated as the estimators to create the covariance structure.}
}
\value{
a list of class \code{slmfit} with \itemize{
  \item the spatial covariance estimates
  \item the regression coefficient estimates
  \item the covariance matrix of the fixed effects
  \item minus two times the log-likelihood of the model
  \item the names of the predictors
  \item the sample size
  \item the name of the covariance model used
  \item a vector of residuals
  \item the design matrix
  \item a vector of the sampled densities
  \item a list containing \enumerate{
       \item formula, the model formula
       \item data, the data set input as the \code{data} argument
       \item xcoordcol, the name of the x-coordinate column
       \item ycoordcol, the name of the y-coordinate column
       \item estmethod, either REML or ML
       \item CorModel, the correlation model used
       \item estimated covariance matrix of all sites
       \item Inverted covariance matrix on the sampled sites
       \item the vector of areas.
       }
}
}
\description{
Estimates regression coefficients and spatial autocorrelation
parameters, given spatial coordinates and a model formula.
}
\examples{
data(exampledataset) ## load a toy data set
slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
summary(slmobj)

data(exampledataset) ## load a toy data set
exampledataset$strata <- c(rep("A", 19), rep("B", 21))
strataobj <- slmfit(formula = counts ~ pred1 + pred2,
  data = exampledataset, stratacol = "strata",
xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
summary(strataobj)

}

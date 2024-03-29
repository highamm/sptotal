% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/AIC.slmfit.R
\name{AIC.slmfit}
\alias{AIC.slmfit}
\title{Extract the AIC from a \code{\link{slmfit}} object for comparing models.}
\usage{
\method{AIC}{slmfit}(object, ...)
}
\arguments{
\item{object}{a \code{\link{slmfit}} object}

\item{...}{further arguments passed to or from other methods.}
}
\value{
The AIC value of the slmfit object. Here,
AIC is computed as 2 times the negative log-likelihood plus
2 times the number of model parameters for Maximum Likelihood (ML)
and 2 times the negative log-likelihood for REML. For REML,
AIC should only be used to compare two models with the same
covariates but different spatial covariance structures.
}
\description{
Extract the AIC from a \code{\link{slmfit}} object for comparing models.
}
\examples{
data(exampledataset) ## load a toy data set
slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
AIC(slmobj)
}

#' Data Set with Uncorrelated Poisson Counts.
#'
#' A data set that can be used with the \code{FPBKForestPack} package. In
#' this example, the counts are uncorrelated, the covariates are
#' generated as uniform random variables, and the sites fall on a
#' regular grid.
#'
#' @format A data frame with 40  rows and 7 variables:
#' \describe{
#'   \item{counts}{counts, with NA values for unsampled sites}
#'   \item{pred1}{a possible predictor}
#'   \item{pred2}{a second possible predictor}
#'   \item{xcoords}{coordinates on the x-axis}
#'   \item{ycoords}{coordinates on the y-axis}
#'   \item{dummyvar}{an extra variable}
#'   \item{areavar}{Variable for the area of each plot}
#'   ...
#' }
"exampledataset"

#' Data Set with Uncorrelated Poisson Counts.
#'
#' A toy data set that can be used with the \code{sptotal} package. In
#' this example, the true counts are actually uncorrelated, the covariates are
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


#' Data Set with Alaska Moose Observations.
#'
#' A data set that can be used with the \code{sptotal} package. In
#' this example, the counts are of moose on 860 sites of equal area.
#'
#' @format A spatial polygons (\code{sp}) object inclding:
#' \describe{
#'   \item{CENTRLAT}{The latitude of the centroid for each site}
#'   \item{CENTRLON}{The latitude of the centroid for each site}
#'   \item{STRAT}{A stratification variable}
#'   \item{TOTAL}{The total moose count on each site}
#'   ...
#' }
"AKmoose"

#' Simulated Spatially Autocorrelated Data.
#'
#' A data set that can be used with the \code{sptotal} package. In
#' this example, the response variable is a density.
#'
#' @format A data frame object inclding:
#' \describe{
#'   \item{x}{The x-coordinate of the centroid for each site}
#'   \item{y}{The y-coordinate of the centroid for each site}
#'   \item{X1}{Independent Variable to be used as a predictor}
#'   \item{X2}{Independent Variable to be used as a predictor}
#'   \item{X3}{Independent Variable to be used as a predictor}
#'   \item{X4}{Independent Variable to be used as a predictor}
#'   \item{X5}{Independent Variable to be used as a predictor}
#'   \item{X6}{Spatially Correlated Random Variable}
#'   \item{X7}{Spatially Correlated Random Variable}
#'   \item{F1}{Factor Variable to be used as a predictor}
#'   \item{F2}{Factor Variable to be used as a predictor}
#'   \item{Z}{The total moose count on each site}
#'   \item{wts1}{Possible Weighting Column to be used as the prediction weights}
#'   \item{wts2}{Possible Weighting Column to be used as the prediction weights}

#'   ...
#' }
"simdata"


#' Oregon Biomass Data
#'
#' A data set that can be used with the \code{sptotal} package. In
#' this example, the response variable is a density.
#'
#' @format A data frame object inclding:
#' \describe{
#'   \item{coordinates}{The spatial cooredinates for the centroid for each site}
#'   \item{BIOT_ha}{Biomass on Each Site}
#'   \item{precip_mean_30yr}{Average precipitation for 30 years}
#'   \item{temp_mean_30yer}{Average temperature for 30 years}
#'   ...
#' }
"ORbiomass"


#' U.S. Lakes Data
#'
#' A data set that can be used with the \code{sptotal} package. In
#' this example, the lakes are located across the United States.
#'
#' @format A data frame object inclding:
#' \describe{
#'   \item{XCOORD}{The x spatial cooredinate for the centroid for each site}
#'   \item{YCOORD}{The y spatial cooredinate for the centroid for each site}
#'   \item{DOC_RESULT}{Disolved Organic Carbon}
#'   \item{ELEVATION}{Variable that is a candidate for prediction}
#'   \item{FCIBIG_LIT}{Variable that is a candidate for prediction}
#'   \item{RVFCGNDBARE_RIP}{Variable that is a candidate for prediction}
#'   \item{RVFCGNDWOODY_RIP}{Variable that is a candidate for prediction}
#'   \item{RVFPUNDWOODY_RIP}{Variable that is a candidate for prediction}
#'   \item{UID}{Lake Identification Number}

#'   ...
#' }
"USlakes"

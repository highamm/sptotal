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


#' Data Set with Alaska Moose Counts.
#'
#' A data set that can be used with the \code{sptotal} package. In
#' this example, the counts are of moose on 860 sites of equal area.
#'
#' @format A dataframe object. The data frame \code{AKmoose_df}
#' contains 860 rows and 8 columns:
#' \describe{
#'   \item{elev_mean}{The mean elevation for each sitefor each site}
#'   \item{strat}{A stratification variable (either L or M)}
#'   \item{surveyed}{Assigned a 1 if the site was surveyed and a 0 otherwise}
#'   \item{total}{The total moose count on each site (\code{NA}
#'   if the site was not surveyed)}
#'   \item{x}{the x-coordinate centroid of the site (TM)}
#'   \item{y}{the y-coordinate centroid of the site (TM)}
#'   \item{lon}{the longitudinal centroid of the site}
#'   \item{lat}{the latitudinal centroid of the site}
#'
#' }
#' @source \href{http://www.adfg.alaska.gov/index.cfm?adfg=hunting.main}{Alaska
#' Department of Fish and Game, Division of Wildlife Conservation}
#' has released this data set under the CC0 (creative commons) license.
#' To the extent possible under law, Alaska Department of Fish and Game,
#' Division of Wildlife Conservation waives all copyright and related or
#' neighboring rights to An Alaskan GSPE (Geospatial Population Estimator)
#' Survey of Moose, AKmoose_df.rda. This work is published from: United States.
#' @examples
#' data(AKmoose_df)
#' names(AKmoose_df)
#' summary(AKmoose_df)
"AKmoose_df"

#' Simulated Spatially Autocorrelated Data.
#'
#' A simulated data set that can be used with the \code{sptotal} package.
#'
#' @format A data frame object including:
#' \describe{
#'   \item{x}{The x-coordinate for each site}
#'   \item{y}{The y-coordinate for each site}
#'   \item{X1}{Simulated independent variable to be used as a predictor}
#'   \item{X2}{Simulated independent variable to be used as a predictor}
#'   \item{X3}{Simulated independent variable to be used as a predictor}
#'   \item{X4}{Simulated independent variable to be used as a predictor}
#'   \item{X5}{Simulated independent variable to be used as a predictor}
#'   \item{X6}{Simulated spatially correlated random variable
#'   to be used as a predictor}
#'   \item{X7}{Simulated spatially correlated random variable to be
#'   used as a predictor}
#'   \item{F1}{Simulated factor variable to be used as a predictor}
#'   \item{F2}{Simulated factor variable to be used as a predictor}
#'   \item{Z}{The simulated response variable.}
#'   \item{wts1}{Prediction weights if estimating an overall mean}
#'   \item{wts2}{Prediction weights for estimating a total over a
#'   subset of 25 contiguous plots}
#' }
#' @examples
#' data(simdata)
#' names(simdata)
#' summary(simdata)
"simdata"

#' Dissolved Organic Carbon in U.S. Lakes
#'
#' These data contain dissolved organic carbon (DOC) in
#' National Lakes Data from the U.S. Environmental Protection Agency
#'
#' @format A data frame with 1206 rows and 9 variables:
#' \describe{
#'   \item{XCOORD}{x-coordinate from US Contiguous Albers Equal
#'   Area Conic projection}
#'   \item{YCOORD}{y-coordinate from US Contiguous Albers Equal
#'   Area Conic projection}
#'   \item{DOC_RESULT}{Analyte value, in mg/L, for Dissolved Organic Carbon}
#'   \item{ELEVATION}{Elevation at lake coordinates
#'   (LAT_DD_N83, LON_DD_N83) from NHD Digital Elevation Map layer}
#'   \item{FCIBIG_LIT}{Fish cover: index of fish cover due to
#'   large structures in the littoral zone}
#'   \item{RVFCGNDBARE_RIP}{riparian zone and vegetation:
#'   fraction of ground lacking cover in the riparian zone}
#'   \item{RVFCGNDWOODY_RIP}{riparian zone and vegetation:
#'   fraction of ground cover by woody vegetation in the riparian zone}
#'   \item{RVFPUNDWOODY_RIP}{riparian zone and vegetation:
#'   fraction of understory with nonwoody cover present in the
#'   riparian zone}
#'   \item{UID}{A unique lake identifier in the EPA lake survey databases}
#' }
#' @source \href{https://www.epa.gov/national-aquatic-resource-surveys/data-national-aquatic-resource-surveys}{National
#' Aquatic Resource Surveys} webpage.
#' We combined \href{https://www.epa.gov/sites/production/files/2016-12/nla2012_wide_siteinfo_08232016.csv}{site data},
#' \href{https://www.epa.gov/sites/production/files/2016-12/nla2012_waterchem_wide.csv}{DOC data},
#' and \href{https://www.epa.gov/sites/production/files/2016-12/nla2012_wide_phabmet_10202016.csv}{habitat metrics}
#' to create a data set of 1206 lakes in the conterminous United States.
#' @examples
#' data(USlakes)
#' names(USlakes)
#' summary(USlakes)
"USlakes"

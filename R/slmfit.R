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

slmfit <- function(formula, data, xcoordcol, ycoordcol,
  CorModel = "Exponential",
  coordtype = "LatLon") {

  ## ASSUME that coordinates are lat/lon. Convert these to UTM
  if (coordtype != "LatLon" & coordtype != "UTM") {
    stop("coordtype must be a character string LatLon or UTM")
  } else if (coordtype == "LatLon") {
    xcoordsUTM <- LLtoUTM(cm = base::mean(data[ ,xcoordcol]),
      lat = data[ ,ycoordcol], lon = data[ ,xcoordcol])$xy[ ,1]
    ycoordsUTM <- LLtoUTM(cm = base::mean(data[ ,xcoordcol]),
      lat = data[ ,ycoordcol], lon = data[ ,xcoordcol])$xy[ ,2]
  } else if (coordtype == "UTM") {
    xcoordsUTM <- data[ ,xcoordcol]
    ycoordsUTM <- data[ ,ycoordcol]
  }


  ## divide data set into sampled sites and unsampled sites based
  ## on whether the response variable has a number (for sampled sites)
  ## or NA (for unsampled sites)


  fullmf <- stats::model.frame(formula, na.action =
      stats::na.pass, data = data)
  yvar <- stats::model.response(fullmf, "numeric")
  density <- yvar

  ind.sa <- !is.na(yvar)
  ind.un <- is.na(yvar)
  data.sa <- data[ind.sa, ]
  data.un <- data[ind.un, ]


  ## display some warnings if the user, for example tries to input the
  ## vector of xcoordinates as the input instead of the name of the column
  if(is.character(xcoordcol) == FALSE) {
    stop("xcoords must be a string giving the
      name of the column in the data set
      with the x coordinates")
  }
  if(is.character(ycoordcol) == FALSE) {
    stop("ycoords must be a string giving the
      name of the column in the data set
      with the y coordinates")
  }


  ## create the design matrix for unsampled sites, for all of the sites,  and for the sampled sites, respectively.

  m.un <- stats::model.frame(formula, data.un, na.action =
      stats::na.pass)
  Xu <- stats::model.matrix(formula, m.un)
  X <- stats::model.matrix(formula, fullmf)

  ## sampled response values and design matrix
  m.sa <- stats::model.frame(formula, data.sa, na.action =
      stats::na.omit)
  z.sa <- stats::model.response(m.sa)
  Xs <- stats::model.matrix(formula, m.sa)
  z.density <- z.sa
  n <- nrow(Xs)

  prednames <- colnames(Xs)

  ## x and y coordinates for sampled and unsampled sites
  x.sa <- xcoordsUTM[ind.sa]
  y.sa <- ycoordsUTM[ind.sa]
  x.un <- xcoordsUTM[ind.un]
  y.un <- ycoordsUTM[ind.un]

  ## number of sites that were sampled
  n.sa <- nrow(Xs)
  ## number of sites that were not sampled
  n.un <- nrow(Xu)


  ## estimate the spatial parameters, the covariance matrix, and
  ## the inverse of the covariance matrix
  spat.est <- estcovparm(response = density,
    designmatrix = as.matrix(X),
    xcoordsvec = xcoordsUTM,
    ycoordsvec = ycoordsUTM, CorModel = CorModel)
  parms.est <- spat.est[[1]]
  Sigma <- spat.est[[2]]
  nugget.effect <- parms.est[1]; parsil.effect <- parms.est[2]
  range.effect <- parms.est[3]

  ## used in the Kriging formulas
  Sigma.us <- Sigma[ind.un, ind.sa]
  Sigma.su <- t(Sigma.us)
  Sigma.ss <- Sigma[ind.sa, ind.sa]

  ## give warning if covariance matrix cannot be inverted
  if(abs(det(Sigma.ss)) <= .Machine$double.eps) {
    warning("Covariance matrix is compulationally singular and
      cannot be inverted")
  }

  Sigma.ssi <- solve(Sigma.ss, tol = .Machine$double.eps)

  ## the generalized least squares regression coefficient estimates
  betahat <- solve((t(Xs) %*% Sigma.ssi %*% Xs)) %*%
    (t(Xs) %*% Sigma.ssi %*% as.matrix(z.density))

  ## estimator for the mean vector
  muhats <- Xs %*% betahat
  muhatu <- Xu %*% betahat

  resids <- z.sa - muhats

  muhat <- rep(NA, nrow(data))
  muhat[ind.sa == TRUE] <- muhats
  muhat[ind.sa == FALSE] <- muhatu


  ## returns a list with the following components:
  ## 1.) A vector of the estimated regression coefficients
  ## 2.) A vector of the estimated covariance parameters
  ## 3.) A list with the information needed by FPBKpred

  covparms <- as.vector(c(nugget.effect, parsil.effect, range.effect))
  betahatest <- as.vector(betahat)
  covest <- solve((t(Xs) %*% Sigma.ssi %*% Xs))

  names(covparms) <- c("Nugget", "Partial Sill", "Range")

  FPBKpredobj <- list(formula, data, xcoordsUTM, ycoordsUTM,
    CorModel, Sigma, Sigma.ssi)
  names(FPBKpredobj) <- c("formula", "data", "xcoordsUTM",
    "ycoordsUTM", "correlationmod", "covmat", "covmatsampi")
  obj <- list(covparms, betahatest, covest, prednames,
    n, CorModel, resids, Xs, z.sa, FPBKpredobj)

  names(obj) <- c("SpatialParmEsts", "CoefficientEsts",
    "BetaCov", "PredictorNames", "SampSize", "CovarianceMod",
    "resids", "DesignMat", "Density",
    "FPBKpredobj")

  class(obj) <- "slmfit"
  return(obj)

  }

##counts <- 1:10
##pred1 <- runif(10, 0, 1)
##pred2 <- runif(10, 0, 2)
##data <- data.frame(cbind(counts, pred1, pred2))
##formula <- counts ~ pred1 + pred2

##slm_info <- slmfit(counts ~ pred1 + pred2, data = exampledataset,
##xcoordcol = "xcoords", ycoordcol = "ycoords",  coordtype = "UTM")
##summary.slmfit(object = slm_info)

##print.summary.slmfit(x = summary.slmfit(object = slm_info))
##summary(slm_info)
##print(slm_info)

##pred_info <- FPBKpred(slmfitobj = slm_info, FPBKcol = NULL)
##str(slm_info)

##FPBKoutput(pred_info = pred_info, get_variogram = TRUE,
##  get_sampdetails = TRUE,
##  get_krigmap = FALSE, get_report = TRUE, conf_level = c(0.80, 0.90,
##    0.95))


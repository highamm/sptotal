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
#' @param areacol is the name of the column with the areas of the sites. By default, we assume that all sites have equal area, in which
#' case a vector of 1's is used as the areas.
#' @param CorModel is the covariance structure. By default, \code{CorModel} is
#' Exponential but other options include the Spherical and Gaussian.
#' @param estmethod is either the default \code{"REML"} for restricted
#' maximum likelihood to estimate the covariance parameters and
#' regression coefficients or \code{"ML"} to estimate the covariance
#' parameters and regression coefficients.
#' @param covestimates is an optional vector of covariance parameter estimates (nugget, partial sill, range). If these are given and \code{estmethod = "None"}, the the provided vector are treated as the estimators to create the covariance structure.
#' @return a list with \itemize{
#'   \item the spatial covariance estimates
#'   \item the regression coefficient estimates
#'   \item the covariance matrix of the fixed effects
#'   \item minus two times the log-likeihood of the model
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
#' slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
#' xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
#' summary(slmobj)
#' @import stats
#' @export slmfit


slmfit <- function(formula, data, xcoordcol, ycoordcol, areacol = NULL,
  CorModel = "Exponential", estmethod = "REML",
  covestimates = c(NA, NA, NA)) {

  ## make sure estmethod is either REML, ML, or None

  if (estmethod != "REML" & estmethod != "ML" &
      estmethod != "None") {
    stop("estmethod must be either 'REML' for restricted maximum
      likelihood, 'ML' for maximum likelihood, or 'None' with
      covariance parameters specified in the covestimates
      argument.")
  }
  ## make sure CorModel is one of the options we have set-up
  if (CorModel != "Exponential" & CorModel != "Spherical" &
      CorModel != "Gaussian") {
    stop("'CorModel' must be either 'Exponential', 'Spherical', or
      'Gaussian'")
  }

  ## display error message if estmethod is set to None and the user
  ## does not input covariance estimates

  if (estmethod == "None" & sum(is.na(covestimates) > 0) > 0) {
    stop("If 'estmethod' is set to None, then 'covestimates' must
      be a vector without missing values
      with the estimated (nugget, partial sill, range)")
  }

  if (estmethod == "None" & length(covestimates) != 3) {
    stop("If 'estmethod' is set to None, then 'covestimates' must
      be a vector of length 3 with the estimated
      (nugget, partial sill, range)")
  }

  if (estmethod == "None" & sum(covestimates < 0) > 0) {
    stop("'covestimates' must be a vector of positive values with
      the (nugget, partial sill, range) specified.")
  }

  ## display some warnings if the user, for example tries to input the
  ## vector of xcoordinates as the input instead of the name of the column
  if (is.character(xcoordcol) == FALSE) {
    stop("xcoords must be a string giving the
      name of the column in the data set
      with the x coordinates")
  }
  if (is.character(ycoordcol) == FALSE) {
    stop("ycoords must be a string giving the
      name of the column in the data set
      with the y coordinates")
  }

  if (sum(names(data) == xcoordcol) == 0 |
      sum(names(data) == ycoordcol) == 0) {
    stop("xcoordcol and ycoordcol must be the names of the columns
      in the data set (in quotes) that specify the x and y coordinates.")
  }



  ## convert all character predictor variables into factors,
  ## with a warning message.
  datapredsonly <- data.frame(data[ ,all.vars(formula)[-1]])
  colnames(datapredsonly) <- all.vars(formula)[-1]
  predictormatch <- match(names(data), names(datapredsonly))

  if (ncol(datapredsonly) >= 1) {

  if (sum(sapply(datapredsonly, is.character)) > 0) {
    warning("At least one predictor variable is a character, which has been converted into a factor.")

  data[ ,sapply(data, is.character) & is.na(predictormatch) == FALSE] <- factor(data[ ,which(sapply(data, is.character))])

  }


  ## check to make sure number of factor levels is somewhat small.
  ## If not, return a warning.

  if (sum(sapply(datapredsonly, is.factor)) >= 1) { ## check to
    ## see if  there are any factors
  if (max(sapply(datapredsonly[ ,sapply(datapredsonly, is.factor)], nlevels)) > 20) {
    warning("At least one predictor variable has more than 20 factor levels.")
  }
  }

  }


  Xall <- model.matrix(formula, model.frame(formula, data,
    na.action = stats::na.pass))

  missingind <- base::apply(is.na(Xall), MARGIN = 1, FUN = sum)
  nmissing <- sum(missingind >= 1)

  datanomiss <- data[missingind == 0, ]

  ## give a warning if some of the predictors have missing values.
  if (nmissing >= 1) {
    warning(paste("There were", nmissing, "sites with predictors with missing values. These will be removed from the data set and further analysis will be completed without these observations."))
  }

  ## display error if any values in the area col are missing
  if (is.null(areacol) == FALSE) {
    if (is.numeric(datanomiss[ ,areacol]) == FALSE |
        sum(is.na(datanomiss[ ,areacol])) > 0) {
      stop("'areacol' must specify the name of the column in the data set with the areas for each site. This column must be numeric
        without any missing values.")
    }
  }



  ## ASSUME that coordinates are TM

    xcoordsUTM <- datanomiss[ ,xcoordcol]
    ycoordsUTM <- datanomiss[ ,ycoordcol]


  ## create the design matrix for unsampled sites, for all of the sites, and for the sampled sites, respectively.


    if (is.null(areacol) == TRUE) {
      areavar <- rep(1, nrow(datanomiss))
    } else {
      areavar <- datanomiss[ ,areacol]
    }

  fullmf <- stats::model.frame(formula, na.action =
      stats::na.pass, data = datanomiss)

  yvar <- stats::model.response(fullmf, "numeric")
  density <- yvar / areavar

  if (is.numeric(yvar) == FALSE) {
    stop("Check to make sure response variable is numeric, not a factor or character.")
  }

  ## remove any rows with missing values in any of the predictors
  formula.onlypreds <- formula[-2]

  X <- model.matrix(formula.onlypreds,
    model.frame(formula.onlypreds, datanomiss,
      na.action = stats::na.omit))

  ## divide data set into sampled sites and unsampled sites based
  ## on whether the response variable has a number (for sampled sites)
  ## or NA (for unsampled sites)

  ind.sa <- !is.na(yvar)
  ind.un <- is.na(yvar)
  data.sa <- datanomiss[ind.sa, ]
  data.un <- datanomiss[ind.un, ]

  m.un <- stats::model.frame(formula, data.un, na.action =
      stats::na.pass)


  Xu <- model.matrix(formula.onlypreds,
    model.frame(formula.onlypreds, data.un,
    na.action = stats::na.omit))


  ## sampled response values and design matrix
  m.sa <- stats::model.frame(formula, data.sa, na.action =
      stats::na.omit)
  z.sa <- stats::model.response(m.sa)
  Xs <- stats::model.matrix(formula, m.sa)
  z.density <- z.sa / areavar[ind.sa]
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
    ycoordsvec = ycoordsUTM, CorModel = CorModel,
    estmethod = estmethod,
    covestimates = covestimates)

  parms.est <- spat.est$parms.est
  Sigma <- spat.est$Sigma
  min2loglik <- spat.est$min2loglik

  nugget.effect <- parms.est[1]; parsil.effect <- parms.est[2]
  range.effect <- parms.est[3]


  Sigma.ssi <- solve(spat.est$qrV) / (nugget.effect + parsil.effect)

  ## the generalized least squares regression coefficient estimates

  betahat <- spat.est$b.hat

  ## estimator for the mean vector
  muhats <- Xs %*% betahat
  muhatu <- Xu %*% betahat

  resids <- z.density - muhats

  muhat <- rep(NA, nrow(datanomiss))
  muhat[ind.sa == TRUE] <- muhats
  muhat[ind.sa == FALSE] <- muhatu


  ## returns a list with the following components:
  ## 1.) A vector of the estimated regression coefficients
  ## 2.) A vector of the estimated covariance parameters
  ## 3.) A list with the information needed by FPBKpred

  covparms <- as.vector(c(nugget.effect, parsil.effect, range.effect))
  betahatest <- as.vector(betahat)
  covest <- spat.est$covb


  names(covparms) <- c("Nugget", "Partial Sill", "Range")

  FPBKpredobj <- list(formula, datanomiss, xcoordsUTM, ycoordsUTM,
    estmethod, CorModel, Sigma, Sigma.ssi, areavar)
  names(FPBKpredobj) <- c("formula", "data", "xcoordsUTM",
    "ycoordsUTM",
    "estmethod","correlationmod", "covmat", "covmatsampi", "areavar")
  obj <- list(covparms, betahatest, covest, min2loglik, prednames,
    n, CorModel, resids, Xs, z.density, FPBKpredobj)

  names(obj) <- c("SpatialParmEsts", "CoefficientEsts",
    "BetaCov", "minus2loglike", "PredictorNames", "SampSize",
    "CovarianceMod",
    "resids", "DesignMat", "Density",
    "FPBKpredobj")

  class(obj) <- "slmfit"
  return(obj)

  }


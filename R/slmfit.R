#' Fits a Spatial Linear Model
#'
#' Estimates regression coefficients and spatial autocorrelation
#' parameters, given spatial coordinates and a model formula.
#'
#' @param formula is an \code{R} linear model formula specifying the
#' response variable as well as covariates for predicting the
#' response on the unsampled sites.
#' @param data is a data frame or tibble with the response column,
#' the covariates to be used for the block kriging, and the
#' spatial coordinates for all of the sites. Alternatively, data can be
#' an \code{sp} Spatial Points Data Frame or \code{sf} object with
#' POINT geometry.
#' @param xcoordcol is the name of the column in the data frame with
#' x coordinates or longitudinal coordinates
#' @param ycoordcol is the name of the column in the data frame with
#' y coordinates or latitudinal coordinates
#' @param areacol is the name of the column with the areas of the sites.
#' By default, we assume that all sites have equal area, in which
#' case a vector of 1's is used as the areas.
#' @param stratacol is the name of the the column with the
#' stratification variable, if strata are to be fit separately,
#' with different covariance parameter estimates.
#' @param CorModel is the covariance structure. By default,
#' \code{CorModel} is Exponential but other options include the
#' Spherical and Gaussian.
#' @param estmethod is either the default \code{"REML"} for restricted
#' maximum likelihood to estimate the covariance parameters and
#' regression coefficients or \code{"ML"} to estimate the covariance
#' parameters and regression coefficients. This argument can also be set to
#' \code{"None"}, in which case \code{covestimates} must be provided.
#' @param covestimates is an optional vector of covariance
#' parameter estimates (nugget, partial sill, range). If these are
#' given and \code{estmethod = "None"}, the the provided vector are
#' treated as the estimators to create the covariance structure.
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
#' slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
#' xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
#' summary(slmobj)
#'
#' data(exampledataset) ## load a toy data set
#' exampledataset$strata <- c(rep("A", 19), rep("B", 21))
#' strataobj <- slmfit(formula = counts ~ pred1 + pred2,
#'   data = exampledataset, stratacol = "strata",
#' xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
#' summary(strataobj)
#'
#' @importFrom stats model.matrix
#' @importFrom stats model.frame
#' @export slmfit

slmfit <- function(formula, data, xcoordcol, ycoordcol,
                   areacol = NULL, stratacol = NULL,
                   CorModel = "Exponential", estmethod = "REML",
                   covestimates = c(NA, NA, NA)) {

  ## convert sp to data frame (point geometry)
  ### see if data has SpatialPointsDataFrame class (a point geometry)
  if (inherits(data, "SpatialPointsDataFrame")) {
    ### take coordinate slot in data
    coords <- data@coords
    ### take data slot in data and redefine data (as a data frame)
    data <- data@data
    ### take the xcoordinate from coords and name it xcoord
    data$xcoord <- coords[, 1]
    ### take the ycoordinate from coords and name it ycoord
    data$ycoord <- coords[, 2]
    ### name xcoordcol "xcoord" to be used later
    xcoordcol <- "xcoord"
    ### name ycoordcol "ycoord" to be used later
    ycoordcol <- "ycoord"
  }

  ## convert sf to data frame (point geometry)
  ### see if data has sf class
  if (inherits(data, "sf")) {
    ### find the sf geometry column location
    geometry_column <- which(colnames(data) == attr(data, "sf_column"))
    ### see if the geometry column has sfc_point class (a point geometry)
    if (inherits(data[[geometry_column]], "sfc_POINT")) {
      ### save the coordinates
      #### (structured as xcoord1, ycoord1, xcoord2, ycoord2, ... , ycoordn)
      coords <- unlist(data[[geometry_column]])
      ### redefine data as a data frame
      data <- as.data.frame(data)
      ### drop the geometry column
      data <- data[, -geometry_column, drop = FALSE]
      ### take the xcoordinate from coords and name it xcoord
      data$xcoord <- coords[seq(from = 1, to = length(coords), by = 2)]
      ### take the ycoordinate from coords and name it ycoord
      data$ycoord <- coords[seq(from = 2, to = length(coords), by = 2)]
      ### name xcoordcol "xcoord" to be used later
      xcoordcol <- "xcoord"
      ### name ycoordcol "ycoord" to be used later
      ycoordcol <- "ycoord"
    } else {
      ### return an error if all geometries are not POINT (user must use
      ### sf::st_cast() or an equivalent)
      stop("All geometries in the sf object must be POINT geometries")
    }
  }

  data <- as.data.frame(data)

  ## fit strata separately (with separate mean and covariance
  ## parameter estimates) if stratacol is specified.
  if (is.null(stratacol) == FALSE) {

    ## check that strata wasn't specified in both formula
    ## and as stratification column

    datapredsonly <- data.frame(data[ ,all.vars(formula)[-1]])
    colnames(datapredsonly) <- all.vars(formula)[-1]

    if(stratacol %in% colnames(datapredsonly) == TRUE) {
      stop("The stratification variable cannot be specified in both
           the stratacol argument and in the model formula. Use the
           model formula if assuming strata have the same
           covariance parameters and use the stratacol argument
           if allowing strata to have different covariance
           parameter estimates.")
    }

    stratafit(formula = formula, data = data,
              xcoordcol = xcoordcol, ycoordcol = ycoordcol,
              stratacol = stratacol, areacol = areacol,
              CorModel = CorModel, estmethod = estmethod)

  } else {

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


      ## check to make sure number of factor levels is somewhat small.
      ## If not, return a warning.

      if (sum(vapply(datapredsonly, is.factor, numeric(1))) >= 1) {
        ## check to see if there are any factors
        if (max(vapply(datapredsonly[ ,vapply(datapredsonly,
                                              is.factor,
                                              numeric(1))], nlevels,
                       numeric(1))) > 20) {
          warning("At least one predictor variable has more
                  than 20 factor levels.")
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
      warning(paste("There were",
      nmissing,
      "sites with predictors
      with missing values. These will be removed from
      the data set and further analysis will be
      completed without these observations."))
    }

    ## display error if any values in the area col are missing
    if (is.null(areacol) == FALSE) {
      if (is.numeric(datanomiss[[areacol]]) == FALSE |
          sum(is.na(datanomiss[[areacol]])) > 0) {
        stop("'areacol' must specify the name of the column in the
        data set with the areas for each site. This column must be
        numeric without any missing values.")
      }
    }



    ## ASSUME that coordinates are TM

    xcoordsTM <- datanomiss[[xcoordcol]]
    ycoordsTM <- datanomiss[[ycoordcol]]


    ## create the design matrix for unsampled sites, for all of the
    ## sites, and for the sampled sites, respectively.


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
      stop("Check to make sure response variable is numeric, not a
           factor or character.")
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

    m.un <- model.frame(formula, data.un, na.action =
                          stats::na.pass)


    Xu <- model.matrix(formula.onlypreds,
                       model.frame(formula.onlypreds, data.un,
                                   na.action = stats::na.omit))


    ## sampled response values and design matrix
    m.sa <- stats::model.frame(formula, data.sa, na.action =
                                 stats::na.omit)
    z.sa <- stats::model.response(m.sa)
    Xs <- model.matrix(formula, m.sa)

    if (abs(det(t(Xs) %*% Xs)) < 1e-10) {
      stop("There are collinearity issues in the predictors.
           Remove collinear predictors and re-fit the model.")
    }

    z.density <- z.sa / areavar[ind.sa]
    n <- nrow(Xs)


    prednames <- colnames(Xs)

    ## x and y coordinates for sampled and unsampled sites
    x.sa <- xcoordsTM[ind.sa]
    y.sa <- ycoordsTM[ind.sa]
    x.un <- xcoordsTM[ind.un]
    y.un <- ycoordsTM[ind.un]

    ## number of sites that were sampled
    n.sa <- nrow(Xs)
    ## number of sites that were not sampled
    n.un <- nrow(Xu)


    ## estimate the spatial parameters, the covariance matrix, and
    ## the inverse of the covariance matrix

    spat.est <- estcovparm(response = density,
                           designmatrix = as.matrix(X),
                           xcoordsvec = xcoordsTM,
                           ycoordsvec = ycoordsTM,
                           CorModel = CorModel,
                           estmethod = estmethod,
                           covestimates = covestimates)

    parms.est <- spat.est$parms.est
    Sigma <- spat.est$Sigma
    Sigma.ss <- Sigma[ind.sa, ind.sa]
    min2loglik <- spat.est$min2loglik

    nugget.effect <- parms.est[1]; parsil.effect <- parms.est[2]
    range.effect <- parms.est[3]


    Sigma.ssi <- solve(spat.est$qrV) / (nugget.effect + parsil.effect)

    ## the generalized least squares regression coefficient estimates

    betahat <- spat.est$b.hat

    ## estimator for the mean vector
    muhats <- Xs %*% betahat
    muhatu <- Xu %*% betahat

    ## could change to density - muhat if we want residuals to have
    ## missing NA by default.
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

    FPBKpredobj <- list(formula, datanomiss, xcoordsTM, ycoordsTM,
                        estmethod, CorModel, Sigma,
                        Sigma.ssi, areavar, Sigma.ss)
    names(FPBKpredobj) <- c("formula", "data", "xcoordsTM",
                            "ycoordsTM",
                            "estmethod","correlationmod",
                            "covmat", "covmatsampi", "areavar",
                            "covmatsamp")
    obj <- list(covparms, betahatest, covest, min2loglik, prednames,
                n, CorModel, resids, Xs, z.density, FPBKpredobj, muhats)

    names(obj) <- c("SpatialParmEsts", "CoefficientEsts",
                    "BetaCov", "minus2loglike", "PredictorNames",
                    "SampSize",
                    "CovarianceMod",
                    "resids", "DesignMat", "Density",
                    "FPBKpredobj", "FittedValues")

    class(obj) <- "slmfit"
    return(obj)
  }
}

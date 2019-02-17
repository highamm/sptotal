#' Perform Finite Population Block Kriging
#'
#' Takes sample data and uses FPBK to predict the counts on the unsampled sites.
#' The column with the counts should have numeric values for the observed counts
#' on the sampled sites and `NA` for any site that was not sampled.
#'
#' @param formula is an R linear model formula specifying density as the
#' response variable as well as covariates for predicting densities on the unsampled sites.
#' @param data is the data set with the response column of counts, the covariates to
#' be used for the block kriging, and the spatial coordinates for all of the sites.
#' @param xcoordcol is the name of the column in the data frame with x coordinates or longitudinal coordinates
#' @param ycoordcol is the name of the column in the data frame with y coordinates or latitudinal coordinates
#' @param CorModel is the covariance structure. By default, \code{CorModel} is
#' Exponential but other options include the Spherical and Gaussian.
#' @param FPBKcol is a vector in the data set that contains the weights for
#' prediction. The default setting predicts the population total
#' @param coordtype specifies whether spatial coordinates are in latitude, longitude (\code{LatLon}) form or UTM (\code{UTM}) form.
#' @return a list with \itemize{
#'   \item the estimated population total
#'   \item the estimated prediction variance
#'   \item a data frame containing \enumerate{
#'        \item x-coordinates
#'        \item y-coordinates
#'        \item density predictions
#'        \item count predictions
#'        \item indicator variable for whether or not the each site was sampled
#'        \item estimated mean for each site
#'        }
#'    \item vector with estimated covariance parameters
#' }
#' @import stats
#' @export FPBKpred


FPBKpred <- function(formula, data, xcoordcol, ycoordcol,
  CorModel = "Exponential", FPBKcol = NULL,
  coordtype = "LatLon") {

  ## if FPBKcol is left out, we are predicting the population total.
  ## Otherwise, FPBKcol is the name of the column in the data set
  ## with the weights for the sites that we are predicting (eg. a vector
  ## of 1's and 0's for predicting the total of the sites marked with 1's)


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


   if (is.null(FPBKcol) == TRUE) {
    predwts <- rep(1, nrow(data))
  } else if (is.character(FPBKcol) == TRUE) {
    predwts <- data[ ,FPBKcol]
  } else{
    stop("FPBKcol must be a character specifying the name of the
      column of
      prediction weights in the data set")
  }

  ## divide data set into sampled sites and unsampled sites based
  ## on whether the response variable has a number (for sampled sites)
  ## or NA (for unsampled sites)

  ## want to krig the density now, not the count


  fullmf <- stats::model.frame(formula, na.action =
      stats::na.pass, data = data)
  yvar <- stats::model.response(fullmf, "numeric")
  density <- yvar

##response.col <- as.character(attr(stats::terms(formula, data = data), "variables"))[2]
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


  ## create the design matrix for unsampled sites, for all of the sites,
  ## and for the sampled sites, respectively.
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

  ## x and y coordinates for sampled and unsampled sites
  x.sa <- xcoordsUTM[ind.sa]
  y.sa <- ycoordsUTM[ind.sa]
  x.un <- xcoordsUTM[ind.un]
  y.un <- ycoordsUTM[ind.un]

  ## number of sites that were sampled
  n.sa <- nrow(Xs)
  ## number of sites that were not sampled
  n.un <- nrow(Xu)

  ## double check that this is what we would want when areas
  ## are not equal to 1
  B <- predwts
  Bs <- B[ind.sa]
  Bu <- B[ind.un]


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

  muhat <- rep(NA, nrow(data))
  muhat[ind.sa == TRUE] <- muhats
  muhat[ind.sa == FALSE] <- muhatu

  ## matrices used in the kriging equations
  ## notation follow Ver Hoef (2008)
  Cmat <- Sigma.ss %*% as.matrix(Bs) +
    Sigma.su %*% as.matrix(Bu)
  Dmat <- t(X) %*% matrix(B) - t(Xs) %*% Sigma.ssi %*% Cmat
  Vmat <- solve(t(Xs) %*% Sigma.ssi %*% Xs)

  ## the predicted values for the sites that were not sampled
  zhatu <- Sigma.us %*% Sigma.ssi %*% (z.density -
      muhats) + muhatu

  ## creating a column in the outgoing data set for predicted counts as
  ## well as a column indicating whether or not the observation was sampled
  ## or predicted
  preddensity <- density
  preddensity[is.na(preddensity) == TRUE] <- zhatu

  preds <- preddensity

  sampind <- rep(1, length(yvar))
  sampind[is.na(yvar) == TRUE] <- 0



  ## the FPBK predictor
  FPBKpredictor <- (t(B) %*% preddensity)

  ## the prediction variance for the FPBK predictor
  ## if detectionest is left as the default, we assume
  ## perfect detection with no variability in the detection estimate

  pred.var.obs <- (t(as.matrix(B)) %*% Sigma %*%
    as.matrix(B) -
    t(Cmat) %*% Sigma.ssi %*% Cmat +
    t(Dmat) %*% Vmat %*% Dmat)


  ## returns a list with 3 components:
  ## 1.) the kriging predictor and prediction variance
  ## 2.) a matrix with x and y coordinates, kriged predctions, and
  ## indicators for whether sites were sampled or not
  ## 3.) a vector of the estimated spatial parameters
  df_out <- data.frame(cbind(xcoordsUTM, ycoordsUTM,
    preddensity, sampind, muhat))
  colnames(df_out) <- c("xcoords", "ycoords",
    "preddensity",
    "sampind", "muhat")
  obj <- list(FPBKpredictor, pred.var.obs,
    df_out,
    as.vector(c(nugget.effect, parsil.effect, range.effect)))

  names(obj) <- c("FPBK_Prediction", "PredVar",
    "Pred_df", "SpatialParms")




  return(obj)

}


# # code used to generate the example data set
#  counts <- rpois(40, 20)
#  counts[c(2, 7, 19, 20, 24)] <- NA
#  pred1 <- runif(40, 0, 1); pred2 <- rnorm(40, 0, 1)
#  xcoordinit <- 1:7; ycoordinit <- 1:7
#  grids <- expand.grid(xcoordinit, ycoordinit)[1:40, ]
#  xcoords <- grids$Var1; ycoords <- grids$Var2
#  dummyvar <- runif(40, 0, 1)
#  areavar <- sample(c(0.5, 1), size = 40, replace = TRUE)
# exampledataset <- as.data.frame(cbind(counts, pred1, pred2, xcoords, ycoords, dummyvar, areavar))
# #devtools::use_data(exampledataset, overwrite = TRUE)
#
# data <- exampledataset
# FPBKcol <- NULL
# xcoordcol <- "xcoords"; ycoordcol <- "ycoords"
# coordtype <- "UTM"
# formula <- counts ~ poly(pred1, 2) + pred2
# formula <- counts ~ pred1 + pred2
# formula <- counts ~ 1

##FPBKpred(formula = formula, data = data, xcoordcol = xcoordcol,
##  ycoordcol = ycoordcol, CorModel = "Gaussian",
##  coordtype = "UTM", FPBKcol = NULL)[[1]]

##pred_info <- FPBKpred(counts ~ pred1 + pred2, data = exampledataset,
##  xcoordcol = "xcoords", ycoordcol = "ycoords",  coordtype = "UTM", areacol = "areavar")
##FPBKoutput(pred_info = pred_info, get_variogram = TRUE,
##  get_krigmap = FALSE, get_report = TRUE, conf_level = c(0.80, 0.90,
##    0.95))

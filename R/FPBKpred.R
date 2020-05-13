#' Perform Finite Population Block Kriging
#'
#' Takes the output of \code{slmfit} and uses FPBK to predict the counts on the unsampled sites.
#' The column with the counts should have numeric values for the observed counts
#' on the sampled sites and `NA` for any site that was not sampled.
#'
#'
#' @param object is an object generated from \code{slmfit}
#' @param FPBKcol is the name of the column that contains the weights for
#' @param ... Additional arguments
#' prediction. The default setting predicts the population total
#' @return a list with \itemize{
#'   \item the estimated population total
#'   \item the estimated prediction variance
#'   \item a data frame containing \enumerate{
#'        \item x-coordinates
#'        \item y-coordinates
#'        \item density predictions
#'        \item indicator variable for whether or not the each site was sampled
#'        \item estimated mean for each site
#'        }
#'    \item vector with estimated covariance parameters
#' }
#' @import stats
#' @export


predict.slmfit <- function(object, FPBKcol = NULL, ...) {

  ## check to make sure object is of class `slmfit`

  if (class(object) != "slmfit") {
    stop("object must be of class 'slmfit' generated from the 'slmfit' function")
  }
  ## if FPBKcol is left out, we are predicting the population total.
  ## Otherwise, FPBKcol is the name of the column in the data set
  ## with the weights for the sites that we are predicting (eg. a vector
  ## of 1's and 0's for predicting the total of the sites marked with 1's)


  formula <- object$FPBKpredobj$formula
  data <- object$FPBKpredobj$data
  xcoordsUTM <- object$FPBKpredobj$xcoordsUTM
  ycoordsUTM <- object$FPBKpredobj$ycoordsUTM
  covparmests <- object$SpatialParmEsts
  areavar <- object$FPBKpredobj$areavar

  if (is.null(FPBKcol) == FALSE) {
    if (sum(names(data) == FPBKcol) == 0) {
    stop("FPBKcol must be the name of the column (in quotes) in the data used in 'slmfit' that specifies the column with the prediction weights. ")
    }
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






  fullmf <- stats::model.frame(formula, na.action =
      stats::na.pass, data = data)
  yvar <- stats::model.response(fullmf, "numeric")
  density <- yvar / areavar

  ind.sa <- !is.na(yvar)
  ind.un <- is.na(yvar)

  ## make sure that some of the response values are missing
  if (sum(ind.un) == 0) {
    stop("None of the values for the response variable are missing (NA). Therefore, prediction cannot be performed for any values of the response.")
  }

  data.sa <- data[ind.sa, ]
  data.un <- data[ind.un, ]

  B <- predwts
  Bs <- B[ind.sa]
  Bu <- B[ind.un]

  m.un <- stats::model.frame(formula, data.un, na.action =
      stats::na.pass)
  Xu <- stats::model.matrix(formula, m.un)
  X <- stats::model.matrix(formula, fullmf)

  ## sampled response values and design matrix
  m.sa <- stats::model.frame(formula, data.sa, na.action =
      stats::na.omit)
  z.sa <- stats::model.response(m.sa)
  Xs <- stats::model.matrix(formula, m.sa)
  z.density <- z.sa / areavar[ind.sa]


  Sigma <- object$FPBKpredobj$covmat

  ## used in the Kriging formulas
  Sigma.us <- Sigma[ind.un, ind.sa]
  Sigma.su <- t(Sigma.us)
  Sigma.ss <- Sigma[ind.sa, ind.sa]
  Sigma.uu <- Sigma[ind.un, ind.un]

       ## give warning if covariance matrix cannot be inverted
      # if(abs(det(Sigma.ss)) <= 1e-21) {
      #   warning("Covariance matrix is compulationally singular and
      #     cannot be inverted")
      # }

  Sigma.ssi <- object$FPBKpredobj$covmatsampi

  ## the generalized least squares regression coefficient estimates
  betahat <- object$CoefficientEsts

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
  zhatucount <- zhatu * areavar[ind.un]


  ## creating a column in the outgoing data set for predicted counts as
  ## well as a column indicating whether or not the observation was sampled
  ## or predicted
  preddensity <- density
  preddensity[is.na(preddensity) == TRUE] <- zhatu

  pred.persite <- preddensity * areavar


  sampind <- rep(1, length(yvar))
  sampind[is.na(yvar) == TRUE] <- 0

  ## adding the site-by-site predictions

  W <- t(Xu) - t(Xs) %*% Sigma.ssi %*% Sigma.su
  sitecov <- Sigma.uu - Sigma.us %*% Sigma.ssi %*% Sigma.su +
    t(W) %*% Vmat %*% W
  sitevar <- diag(sitecov)

  densvar <- rep(NA, nrow(data))
  densvar[sampind == 1] <- 0
  densvar[sampind == 0] <- sitevar

  countcov <- areavar[ind.un] *
    sitevar * areavar[ind.un]
  countcovnodet <- countcov

  countvar <- rep(NA, nrow(data))
  countvar[sampind == 1] <- 0
  countvar[sampind == 0] <- countcov

  ## the FPBK predictor
  FPBKpredictor <- (t(B) %*% preddensity) ## density
  FPBKpredictorcount <- (t(B) %*% pred.persite) ## count

  ## the prediction variance for the FPBK predictor
  ## if detectionest is left as the default, we assume
  ## perfect detection with no variability in the detection estimate

  pred.var.obs <- (t(as.matrix(B)) %*% Sigma %*%
    as.matrix(B) -
    t(Cmat) %*% Sigma.ssi %*% Cmat +
    t(Dmat) %*% Vmat %*% Dmat) ## density
  pred.var.count <- (t(as.matrix(B * areavar)) %*% Sigma %*%
      as.matrix(B * areavar) -
      t(Cmat) %*% Sigma.ssi %*% Cmat +
      t(Dmat) %*% Vmat %*% Dmat)


  ## returns a list with 3 components:
  ## 1.) the kriging predictor and prediction variance
  ## 2.) a matrix with x and y coordinates, kriged predctions, and
  ## indicators for whether sites were sampled or not
  ## 3.) a vector of the estimated spatial parameters




  df_out <- data.frame(cbind(data, xcoordsUTM, ycoordsUTM,
    preddensity, densvar, sampind, muhat, areavar))

  # data <- data.frame(y = 1:10, x = 2:11)
  #
  # fullmf <- stats::model.frame(formula, na.action =
  #   stats::na.pass, data = data)

  colnames(df_out) <- c(colnames(data), "xcoordsUTM_", "ycoordsUTM_",
    paste(base::all.vars(formula)[1], "_pred",
      sep = ""),
    paste(base::all.vars(formula)[1], "_predvar",
      sep = ""),
    paste(base::all.vars(formula)[1], "_sampind",
      sep = ""),
    paste(base::all.vars(formula)[1], "_muhat",
      sep = ""),
    paste(base::all.vars(formula)[1], "_areas",
      sep = ""))

  obj <- list(FPBKpredictorcount, pred.var.count,
    df_out,
    as.vector(covparmests),
    formula = formula)

  names(obj) <- c("FPBK_Prediction", "PredVar",
    "Pred_df", "SpatialParms", "formula")

  class(obj) <- "sptotalPredOut"

  return(obj)

}





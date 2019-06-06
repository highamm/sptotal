#' Covariance Parameter Estimation Function.
#'
#' The primary purpose of \code{m2LL.FPBK.nodet} is to estimate the spatial
#' covariance parameters using REML.
#'
#' @param theta is the parameter vector of (nugget, partialsill, range)
#' @param zcol is the response vector of densities
#' @param XDesign is the design matrix containing the covariates used to predict animal or plant abundance (including a column of 1's for the intercept).
#' @param xcoord is a vector of the x spatial coordinates (in UTM)
#' @param ycoord is a vector of the y spatial coordinates (in UTM)
#' @param CorModel is the geostatistical spatial correlation model to be used. See the \code{corModels} documentation for possible models to use.
#' @param estmethod is either "REML" for restricted maximum likelihood or "ML" for maximum likelihood.

#' @return A numeric output of minus 2 times the restricted log likelihood to be minimized by `optim` to obtain spatial parameter estimates.
#' @importFrom stats optim
#' @importFrom stats glm
#' @importFrom stats rbinom

## split into different functions for different covariance matrix structures

m2LL.FPBK.nodet <- function(theta, zcol, XDesign, xcoord, ycoord,
  CorModel, estmethod) {
  ## Exponential

  n <- length(zcol)
  p <- length(XDesign[1, ])

  ## we can use profiled likelihood to optimize likelihood,
  ## proportion of nugget to nugget + partial sill (overall variance)
  nug_prop <- as.numeric(exp(theta[1]) / (1 + exp(theta[1])))
  range <- as.numeric(exp(theta[2]))

  ## construct the distance matrix
  DM <- matrix(0, n, n)
  DM[lower.tri(DM)] <- stats::dist(as.matrix(cbind(xcoord,ycoord)))
  Dismat <- DM + t(DM)

  ## construct spatial autocorrelation matrix using exponential covariance structure
  if (CorModel == "Exponential") {
    Sigmat <- (1 - nug_prop) * corModelExponential(Dismat, range)
    Cmat.nodet <- diag(nug_prop, nrow = nrow(Sigmat)) + Sigmat
  } else if (CorModel == "Gaussian") {
    Sigmat <- (1 - nug_prop) * (corModelGaussian(Dismat, range))
    Cmat.nodet <- diag(nug_prop, nrow = nrow(Sigmat)) + Sigmat
  } else if (CorModel == "Spherical") {
    Sigmat <- (1 - nug_prop) * corModelSpherical(Dismat, range)
    Cmat.nodet <- diag(nug_prop, nrow = nrow(Sigmat)) +
      Sigmat
  }

  ## use QR decomposition, it is more stable and faster
  ## ViX is the same as the slower method of directly calculating
  ## solve(Cmat.nodet) %*% XDesign (can verify using algebra)

  if (nug_prop < 0.001) {
    Cmat.nodet <- Cmat.nodet + diag(1e-6, nrow = nrow(Cmat.nodet))
  }

  qrV <- qr(Cmat.nodet)
  ViX <- solve(qrV, XDesign)

  covbi <- crossprod(XDesign, ViX) ## Computationally more efficient than covbi <- t(X) %*% ViX

  covb <- mginv(covbi, tol = 1e-21)

  ## again, instead of solve(Cmat.nodet) %*% zcol
  ## use qr decomposition as a faster method
  b.hat <- covb %*% t(XDesign) %*% solve(qrV, zcol)
  ## b.hat <- covb %*% t(XDesign) %*% Ci %*% zcol
  r <- zcol - XDesign %*% b.hat

  np <- n

  if (estmethod == "REML") {
    np <- n - p
  }
    ## this part is in common to both REML and ML for given np
    ## log is taken in the first term here because we are
    ## profiling the variance term.
  LLcommon <- np * log(crossprod(r, solve(qrV, r))) +
    sum(log(abs(diag(qr.R(qrV))))) + ##log(det(Cmat.nodet))
    np * (1 + log(2 * pi / np))

  if(estmethod == "REML") {
    ## add log(det(t(X)V(theta)^-1 X)) to REML
     LLcommon <- LLcommon + sum(log(svd(covbi)$d))
  }
  return(LLcommon)
}


#  theta <- c(2, 1, 0.5)
#  zcol <- runif(40, 0, 15);
#  pred1 <- runif(40, 0, 1); pred2 <- rnorm(40, 0, 1)
#  XDesign <- as.matrix(cbind(rep(1, 40), pred1, pred2))
#  xcoord <- runif(40, 0, 1); ycoord <- runif(40, 0, 1)
#
# m2LL.FPBK.nodet(theta = theta, zcol = zcol, XDesign = XDesign,
#   xcoord = xcoord, ycoord = ycoord, CorModel = "Exponential")
# m2LL_jay(theta = theta, zcol = zcol, XDesign = XDesign,
#   xcoord = xcoord, ycoord = ycoord, CorModel = "Exponential",
#   estmethod = "REML")

## OLD ML CODE

# m2LL.FPBK.nodet.ML <- function(theta, zcol, XDesign, xcoord, ycoord,
#   CorModel)
# {
#   n <- length(zcol)
#   p <- length(XDesign[1,])
#   nugget <- as.numeric(exp(theta[1]))
#   parsil <- as.numeric(exp(theta[2]))
#   range <- as.numeric(exp(theta[3]))
#   beta <- matrix(as.numeric(theta[4:length(theta)]))
#
#   DM <- matrix(0, n, n)
#   DM[lower.tri(DM)] <- stats::dist(as.matrix(cbind(xcoord, ycoord)))
#   Dismat <- DM + t(DM)
#
#   if (CorModel == "Exponential") {
#     Sigmat <- parsil * corModelExponential(Dismat, range)
#     Cmat.nodet <- diag(nugget, nrow = nrow(Sigmat)) + Sigmat
#   } else if (CorModel == "Gaussian") {
#     Sigmat <- parsil * (corModelGaussian(Dismat, range))
#     Cmat.nodet <- diag(nugget, nrow = nrow(Sigmat)) + Sigmat
#   } else if (CorModel == "Spherical") {
#     Sigmat <- parsil * corModelSpherical(Dismat, range)
#     Cmat.nodet <- diag(nugget, nrow = nrow(Sigmat)) +
#       Sigmat
#   }
#
#   Ci <- mginv(Cmat.nodet)
#
#   minus2loglik <- log(det(Cmat.nodet)) +
#     (t(as.matrix(zcol) - as.matrix(XDesign %*% beta))) %*%
#     Ci %*%
#     (as.matrix(zcol) - as.matrix(XDesign %*% beta)) +
#     n * log(2 * pi)
#
#   return(as.numeric(minus2loglik))
# }


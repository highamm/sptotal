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

#' @return A numeric output of minus 2 times the restricted log likelihood to be minimized by `optim` to obtain spatial parameter estimates.
#' @importFrom stats optim
#' @importFrom stats glm
#' @importFrom stats rbinom
#' @export m2LL.FPBK.nodet

## split into different functions for different covariance matrix structures

m2LL.FPBK.nodet <- function(theta, zcol, XDesign, xcoord, ycoord,
  CorModel) {
  ## Exponential

  n <- length(zcol)
  p <- length(XDesign[1,])
  nugget <- as.numeric(exp(theta[1]))
  parsil <- as.numeric(exp(theta[2]))
  range <- as.numeric(exp(theta[3]))

  ## construct the distance matrix
  DM <- matrix(0, n, n)
  DM[lower.tri(DM)] <- stats::dist(as.matrix(cbind(xcoord,ycoord)))
  Dismat <- DM + t(DM)

  ## construct spatial autocorrelation matrix using exponential covariance structure
  if (CorModel == "Exponential") {
    Sigmat <- parsil * corModelExponential(Dismat, range)
    Cmat.nodet <- diag(nugget, nrow = nrow(Sigmat)) + Sigmat
  } else if (CorModel == "Gaussian") {
    Sigmat <- parsil * (corModelGaussian(Dismat, range))
    Cmat.nodet <- diag(nugget, nrow = nrow(Sigmat)) + Sigmat
  } else if (CorModel == "Spherical") {
    Sigmat <- parsil * corModelSpherical(Dismat, range)
    Cmat.nodet <- diag(nugget, nrow = nrow(Sigmat)) +
       Sigmat
  }

  ## using REML to obtain the restricted maximum likelihood
  Ci <- mginv(Cmat.nodet, tol = 1e-21)
  covbi <- t(XDesign) %*% Ci %*% XDesign
  covb <- mginv(covbi, tol = 1e-21)
  b.hat <- covb %*% t(XDesign) %*% Ci %*% zcol
  r <- zcol - XDesign %*% b.hat

  return(t(r) %*% Ci %*% r + sum(log(svd(Cmat.nodet)$d)) + sum(log(svd(covbi)$d)))
  }

#' Estimation Function
#'
#' The primary purpose of \code{m2LL.FPBK.nodet} is to estimate the spatial
#' covariance parameters using REML.
#'
#' @param theta is the parameter vector of (nugget, partialsill, range)
#' @param zcol is the response vector of counts
#' @param XDesign is the design matrix containing the covariates used to predict animal or plant abundance (including a column of 1's for the intercept).
#' @param xcoord is a vector of the x spatial coordinates (in UTM)
#' @param ycoord is a vector of the y spatial coordinates (in UTM)

#' @return A numeric output of minus 2 times the restricted log likelihood to be minimized by `optim` to obtain spatial parameter estimates.
#' @importFrom stats optim
#' @importFrom stats glm
#' @importFrom stats rbinom
#' @export m2LL.FPBK.nodet.sph

m2LL.FPBK.nodet.sph <- function(theta, zcol, XDesign, xcoord, ycoord) {

    ## Spherical covariance matrix

    n <- length(zcol)
    p <- length(XDesign[1, ])
    nugget <- as.numeric(exp(theta[1]))
    parsil <- as.numeric(exp(theta[2]))
    range <- as.numeric(exp(theta[3]))

    ## construct the distance matrix
    DM <- matrix(0, n, n)
    DM[lower.tri(DM)] <- stats::dist(as.matrix(cbind(xcoord,ycoord)))
    Dismat <- DM + t(DM)

    ## construct spatial autocorrelation matrix using spherical covariance structure
    cormatSpher <- 1 - (3 / 2) * (Dismat / range) +
      (1 / 2) * (Dismat / range) ^ 3
    cormatSpher[Dismat > range] <- 0
    Cmat.nodet <- diag(nugget, nrow = nrow(Dismat)) +
      parsil * cormatSpher

    Ci <- mginv(Cmat.nodet, tol = 1e-21)
    covbi <- t(XDesign) %*% Ci %*% XDesign
    covb <- mginv(covbi, tol = 1e-21)
    b.hat <- covb %*% t(XDesign) %*% Ci %*% zcol
    r <- zcol - XDesign %*% b.hat
    return(t(r) %*% Ci %*% r + sum(log(svd(Cmat.nodet)$d)) + sum(log(svd(covbi)$d)))
}


# theta <- c(2, 1, 0.5)
# zcol <- runif(40, 0, 15);
# pred1 <- runif(40, 0, 1); pred2 <- rnorm(40, 0, 1)
# XDesign <- as.matrix(cbind(rep(1, 40), pred1, pred2))
# xcoord <- runif(40, 0, 1); ycoord <- runif(40, 0, 1)

##m2LL.FPBK.nodet(theta = theta, zcol = zcol, XDesign = XDesign,
##  xcoord = xcoords, ycoord = ycoords)


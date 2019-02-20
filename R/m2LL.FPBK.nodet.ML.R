#' Covariance Parameter Estimation Function.
#'
#' The primary purpose of \code{m2LL.FPBK.nodet.ML} is to estimate the spatial
#' covariance parameters using Maximum Likelihood
#'
#' @param theta is the parameter vector of (nugget, partialsill,
#'  range, regression_coefficients)
#' @param zcol is the response vector of densities
#' @param XDesign is the design matrix containing the covariates used to predict animal or plant abundance (including a column of 1's for the intercept).
#' @param xcoord is a vector of the x spatial coordinates (in UTM)
#' @param ycoord is a vector of the y spatial coordinates (in UTM)
#' @param CorModel is the geostatistical spatial correlation model to be used. See the \code{corModels} documentation for possible models to use.

#' @return A numeric output of the negative of the log likelihood to be minimized by `optim` to obtain spatial parameter estimates.
#' @importFrom stats optim
#' @importFrom stats glm
#' @importFrom stats rbinom
#' @export m2LL.FPBK.nodet.ML

m2LL.FPBK.nodet.ML <- function(theta, zcol, XDesign, xcoord, ycoord,
  CorModel)
{
  n <- length(zcol)
  p <- length(XDesign[1,])
  nugget <- as.numeric(exp(theta[1]))
  parsil <- as.numeric(exp(theta[2]))
  range <- as.numeric(exp(theta[3]))
  beta <- matrix(as.numeric(theta[4:length(theta)]))

  DM <- matrix(0, n, n)
  DM[lower.tri(DM)] <- stats::dist(as.matrix(cbind(xcoord, ycoord)))
  Dismat <- DM + t(DM)

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

  Ci <- mginv(Cmat.nodet)
  minusloglik <- (1 / 2) * log(det(2 * pi * Cmat.nodet)) +
    (1 / 2) * (t(as.matrix(zcol) - as.matrix(XDesign %*% beta))) %*%
    Ci %*%
    (as.matrix(zcol) - as.matrix(XDesign %*% beta))
  return(as.numeric(minusloglik))
}

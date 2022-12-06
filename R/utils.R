#' Estimate Covariance Parameters
#'
#' Used to estimate spatial covariance parameters for a few different spatial models.
#' Estimated parameters can then be used in \code{predict.slmfit()} to predict values at unobserved locations.
#'
#' The function is a helper function used internally in \code{predict.slmfit()}.
#'
#' @param response a vector of a response variable, possibly with
#' missing values.
#' @param designmatrix is the matrix of covariates used to regress
#' the response on.
#' @param xcoordsvec is a vector of x coordinates
#' @param ycoordsvec is a vector of y coordinates
#' @param CorModel is the covariance structure. By default,
#'  \code{CorModel} is \code{"Exponential"} but other options are
#'  \code{"Spherical"} and \code{"Gaussian"}.
#' @param estmethod is either the default \code{"REML"} for restricted
#' maximum likelihood to estimate the covariance parameters and
#' regression coefficients or \code{"ML"} to estimate the covariance
#' parameters and regression coefficients.
#' @param covestimates is an optional vector of covariance parameter estimates (nugget, partial sill, range). If these are given and \code{estmethod = "None"}, the the provided vector are treated as the estimators to create the covariance structure.
#' @keywords internal
#' @return a list with \itemize{
#'    \item \code{parms.est}, a vector of estimated covariance parameters
#'    \item \code{Sigma}, the fitted covariance matrix for all of the sites
#'    \item \code{qrV}, the qr decomposition
#'    \item \code{b.hat}, the vector of estimated fixed effect coefficients
#'    \item \code{covbi}, the inverse of the covariance matrix for the fixed effects
#'    \item \code{covb}, the covariance matrix for the fixed effects
#'    \item \code{min2loglik}, minus two times the loglikelihood
#' }


estcovparm <- function(response, designmatrix, xcoordsvec, ycoordsvec,
  CorModel = "Exponential", estmethod = "REML",
  covestimates = c(NA, NA, NA)) {

  ## only estimate parameters using sampled sites only

  ind.sa <- !is.na(response)
  designmatrixsa <- as.matrix(designmatrix[ind.sa, ])


  names.theta <- c("nugget", "parsil", "range")

  ## eventually will expand this section to include other covariance types
  if(CorModel == "Cauchy" || CorModel == "BesselK"){
    names.theta <- c(names.theta, "extrap")
  }

  nparm <- length(names.theta)

  n <- length(response[ind.sa])
  p <- ncol(designmatrix)


  ## distance matrix for all of the sites
  distmatall <- matrix(0, nrow = nrow(designmatrix),
    ncol = nrow(designmatrix))
  distmatall[lower.tri(distmatall)] <- stats::dist(as.matrix(cbind(xcoordsvec, ycoordsvec)))
  distmatall <- distmatall + t(distmatall)

  ## constructing the distance matrix between sampled sites only
  sampdistmat <- matrix(0, n, n)
  sampdistmat[lower.tri(sampdistmat)] <- stats::dist(as.matrix(cbind(xcoordsvec[ind.sa], ycoordsvec[ind.sa])))
  distmat <- sampdistmat + t(sampdistmat)

  if (estmethod == "None") {

    possible_nug_prop <- covestimates[1] /
      (covestimates[1] + covestimates[2])
    possible.theta1 <- log(possible_nug_prop /
        (1 - possible_nug_prop))
    possible.range <- covestimates[3]
    possible.theta2 <- log(possible.range)
    theta <- c(possible.theta1, possible.theta2)

    m2loglik <- m2LL.FPBK.nodet(theta = theta,
      zcol = response[ind.sa],
      XDesign = as.matrix(designmatrixsa),
      distmat = distmat,
      CorModel = CorModel,
      estmethod = "ML")

    loglik <- -m2loglik

    nug_prop <- possible_nug_prop
    range.effect <- possible.range
  } else {

    possible_nug_prop <- c(0.25, 0.5, 0.75)
    possible.theta1 <- log(possible_nug_prop /
        (1 - possible_nug_prop))
    possible.range <- c(max(distmat), max(distmat) / 2, max(distmat) / 4)
    possible.theta2 <- log(possible.range)
    theta <- expand.grid(possible.theta1, possible.theta2)

    m2loglik <- rep(NA, nrow(theta))


    for (i in 1:nrow(theta)) {
      m2loglik[i] <- m2LL.FPBK.nodet(theta = theta[i, ],
        zcol = response[ind.sa],
        XDesign = as.matrix(designmatrixsa),
        distmat = distmat,
        CorModel = CorModel,
        estmethod = estmethod)
    }

    max.lik.obs <- which(m2loglik == min(m2loglik))

    ## optimize using Nelder-Mead
    parmest <- optim(theta[max.lik.obs, ], m2LL.FPBK.nodet,
      zcol = response[ind.sa],
      XDesign = as.matrix(designmatrixsa),
      distmat = distmat,
      method = "Nelder-Mead",
      CorModel = CorModel, estmethod = estmethod)

    ## extract the covariance parameter estimates. When we deal with covariance
    ## functions with more than 3 parameters, this section will need to be modified
    min2loglik <- parmest$value
    loglik <- -min2loglik

    # nugget.effect <- exp(parmest$par[1])
    # parsil.effect <- exp(parmest$par[2])
    # range.effect <- exp(parmest$par[3])
    # parms.est <- c(nugget.effect, parsil.effect, range.effect)

    nug_prop <- exp(parmest$par[1]) / (1 + exp(parmest$par[1]))
    range.effect <- exp(parmest$par[2])

  }
  ## ALL THINGS PERTAINING TO ML BEFORE JAY'S UPDATES
  # possible.thetarest <- as.vector(solve(t(as.matrix(designmatrixsa)) %*%
  #   as.matrix(designmatrixsa)) %*% t(as.matrix(designmatrixsa)) %*%
  #   response[ind.sa])
  #
  #
  # theta <- expand.grid(possible.theta1, possible.theta2,
  #   possible.theta3)
  # possible.thetaresttab <- matrix(possible.thetarest,
  #   nrow = nrow(theta), ncol = length(possible.thetarest),
  #   byrow = TRUE)
  #
  # theta <- cbind(theta, possible.thetaresttab)
  #
  #
  # m2loglik <- rep(NA, nrow(theta))
  #
  # for (i in 1:nrow(theta)) {
  #   m2loglik[i] <- m2LL.FPBK.nodet.ML(theta = theta[i, ],
  #     zcol = response[ind.sa],
  #     XDesign = as.matrix(designmatrixsa),
  #     xcoord = xcoordsvec[ind.sa], ycoord = ycoordsvec[ind.sa],
  #     CorModel = CorModel)
  # }
  #
  # max.lik.obs <- which(m2loglik == min(m2loglik))
  #
  # ## optimize using Nelder-Mead
  # parmest <- optim(theta[max.lik.obs, ], m2LL.FPBK.nodet.ML,
  #   zcol = response[ind.sa],
  #   XDesign = as.matrix(designmatrixsa),
  #   xcoord = xcoordsvec[ind.sa], ycoord = ycoordsvec[ind.sa],
  #   method = "Nelder-Mead",
  #   CorModel = CorModel)
  #
  # ## extract the covariance parameter estimates. When we deal with covariance
  # ## functions with more than 3 parameters, this section will need to be modified
  # minloglik <- parmest$value
  # loglik <- -minloglik
  #
  # nugget.effect <- exp(parmest$par[1])
  # parsil.effect <- exp(parmest$par[2])
  # range.effect <- exp(parmest$par[3])
  # regcoefs <- parmest$par[4:length(theta)]
  # parms.est <- c(nugget.effect, parsil.effect, range.effect,
  #   regcoefs)



  #get overall variance parameter
  if (CorModel == "Exponential") {
    Sigma <- (1 - nug_prop) *
      corModelExponential(distmatall, range.effect) +
      diag(nug_prop, nrow = nrow(distmatall))
  } else if (CorModel == "Gaussian") {
    Sigma <- (1 - nug_prop) *
      (corModelGaussian(distmatall, range.effect)) +
      diag(nug_prop, nrow = nrow(distmatall))
  } else if (CorModel == "Spherical") {
    Sigma <-  (1 - nug_prop) *
      corModelSpherical(distmatall, range.effect) +
      diag(nug_prop, nrow = nrow(distmatall))
  }

  ## diagonalization to stabilize the resulting covariance matrix
  if (nug_prop < 0.0001) {
    Sigma <- Sigma + diag(1e-6, nrow = nrow(Sigma))
  }

  # #get Sigma for sampled sites only
  Sigma_samp = Sigma[ind.sa, ind.sa]


  qrV <- qr(Sigma_samp)
  ViX <- solve(qrV, as.matrix(designmatrixsa))
  covbi <- crossprod(as.matrix(designmatrixsa), ViX)
  covb <- mginv(covbi, tol = 1e-21)
  b.hat <- covb %*% t(as.matrix(designmatrixsa)) %*%
    solve(qrV, response[ind.sa])
  r <- response[ind.sa] - as.matrix(designmatrixsa) %*% b.hat

  sill <- as.numeric(crossprod(r, solve(qrV, r))) / n

  if (estmethod == "None") {
    sill <- covestimates[1] + covestimates[2]
  }

  if(estmethod == "ML" | estmethod == "None") {
    min2loglik <- n * log(sill) + sum(log(abs(diag(qr.R(qrV))))) +
      as.numeric(crossprod(r, solve(qrV, r))) / sill + n * log(2 * pi)
  }

  if(estmethod == "REML") {

    sill = n * sill/(n - p)
    min2loglik = (n - p) * log(sill) +
      sum(log(abs(diag(qr.R(qrV))))) +
      as.numeric(crossprod(r, solve(qrV,r))) / sill +
      (n - p) * log(2 * pi) +
      sum(log(svd(covbi)$d))
  }

  # scale profiled quantities by the sill
  covbi <- sill * covbi
  covb <- sill * covb
  Sigma <- sill * Sigma
  parms.est <- c(nug_prop * sill, (1 - nug_prop) * sill,
    range.effect)

  return(list(parms.est = parms.est, Sigma = Sigma, qrV = qrV,
    b.hat = b.hat, covbi = covbi, covb = covb,
    min2loglik = min2loglik))
}

#' Covariance Parameter Estimation Function.
#'
#' The primary purpose of \code{m2LL.FPBK.nodet()} is to estimate the spatial
#' covariance parameters using REML. This is a helper function to \code{slmfit()}.
#'
#' @param theta is the parameter vector of (nugget, partialsill, range)
#' @param zcol is the response vector of densities
#' @param XDesign is the design matrix containing the covariates used to predict animal or plant abundance (including a column of 1's for the intercept).
#' @param distmat is the distance matrix of the sampled sites
#' @param CorModel is the geostatistical spatial correlation model to be used. See the \code{corModels} documentation for possible models to use.
#' @param estmethod is either "REML" for restricted maximum likelihood or "ML" for maximum likelihood.

#' @return A numeric output of minus 2 times the restricted log likelihood to be minimized by `optim` to obtain spatial parameter estimates.
#' @importFrom stats optim
#' @importFrom stats glm
#' @importFrom stats rbinom

m2LL.FPBK.nodet <- function(theta, zcol, XDesign, distmat,
  CorModel, estmethod) {
  ## Exponential

  n <- length(zcol)
  p <- length(XDesign[1, ])

  ## we can use profiled likelihood to optimize likelihood,
  ## proportion of nugget to nugget + partial sill (overall variance)
  nug_prop <- as.numeric(exp(theta[1]) / (1 + exp(theta[1])))
  range <- as.numeric(exp(theta[2]))

  Dismat <- distmat

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

  ## OLD ML CODE: this code is slower but a bit easier to follow.

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

  return(LLcommon)

}

#' Constructing the generalized inverse of a matrix
#'
#' Computes the generalized inverse of a matrix X. This function is used in
#' the \code{m2LL.FPBK.nodet} functions in order
#' to estimate the spatial covariance parameters
#'
#' @param X The matrix to be inverted
#' @param tol The tolerance of the estimation
#'
#' @return The generalized inverse matrix

mginv <- function(X, tol = sqrt(.Machine$double.eps)) {
  dnx <- dimnames(X)
  if(is.null(dnx)) dnx <- vector("list", 2)
  s <- svd(X)
  nz <- s$d > tol * s$d[1]
  structure(
    if(any(nz)) s$v[, nz] %*% (t(s$u[, nz])/s$d[nz]) else X,
    dimnames = dnx[2:1])
}

#' Spatial Correlation Models
#'
#' Note that, currently, only three of these models are implemented
#' in the \code{sptotal} package: \code{corModelExponential()},
#'  \code{corModelGaussian()}, and \code{corModelSpherical()}.
#'
#' @param distance.matrix The distance matrix for sampled sites
#' @param range The range that determines how quickly covariance
#' among sites tapers
#' @return Correlation Matrix


#' @describeIn corModelExponential Exponential Correlation Structure
corModelExponential <- function(distance.matrix, range) {
  exp(-distance.matrix / range)
}

corModelExpRadon2 <- function(distance.matrix) {
  (1 + distance.matrix)*exp(-distance.matrix)
}

corModelExpRadon4 <- function(distance.matrix) {
  (1 + distance.matrix + distance.matrix^2/3)*exp(-distance.matrix)
}

#' @describeIn corModelExponential Gaussian Correlation Structure
corModelGaussian <- function(distance.matrix, range) {
  exp(-distance.matrix^2 / range)
}

corModelStable <- function(distance.matrix, extrap) {
  exp(-distance.matrix^extrap)
}

corModelRationalQuad <- function(distance.matrix) {
  1/(1+distance.matrix^2)
}

corModelCauchyGrav <- function(distance.matrix) {
  1/sqrt(1+distance.matrix^2)
}

corModelCauchyMag <- function(distance.matrix) {
  1/(sqrt(1+distance.matrix^2))^3
}

corModelCauchy <- function(distance.matrix, range, extrap) {
  1/(1+distance.matrix^2)^extrap
}

corModelCircular <- function(distance.matrix) {
  d <- distance.matrix
  d[distance.matrix > 1] <- 0
  CovMat <- 2*(acos(d) - d*sqrt(1 - d^2))/pi
  CovMat[distance.matrix >= 1] <- 0
  CovMat
}

#' @describeIn corModelExponential Spherical Correlation Structure

corModelSpherical <- function(distance.matrix, range) {
  CovMat <- (1 - 1.5 * (distance.matrix / range) +
      0.5 * (distance.matrix / range) ^ 3)
  CovMat[distance.matrix / range > 1] <- 0
  CovMat
}

corModelCubic <- function(distance.matrix) {
  CovMat <- (1 - 7*distance.matrix^2 + 35*distance.matrix^3/4 - 7*distance.matrix^5/2
    + 3*distance.matrix^7/4)
  CovMat[distance.matrix > 1] <- 0
  CovMat
}

corModelPenta <- function(distance.matrix) {
  CovMat <- (1 - 22*distance.matrix^2/3 + 33*distance.matrix^4 - 77*distance.matrix^5/2
    + 33*distance.matrix^7/2 - 11*distance.matrix^9/2 + 5*distance.matrix^11/6)
  CovMat[distance.matrix > 1] <- 0
  CovMat
}

corModelCardinalSine <- function(distance.matrix) {
  d <- distance.matrix
  d[distance.matrix == 0] <- 1
  CorMat <- sin(d)/d
  CorMat[distance.matrix == 0] <- 1
  CorMat
}

corModelBesselJ <- function(distance.matrix, extrap) {
  d <- distance.matrix
  d[distance.matrix == 0] <- 1
  extrap <- extrap + 2.0
  CorMat <- d^(1-extrap/2)*besselJ(d, extrap/2 - 1)*2^(extrap/2 - 1)*gamma(extrap/2)
  CorMat[distance.matrix == 0] <- 1
  CorMat
}

corModelBesselK <- function(distance.matrix, extrap) {
  d <- distance.matrix
  d[distance.matrix == 0] <- 1
  CorMat <- d^extrap*besselK(d, extrap)/(2^(extrap - 1)*gamma(extrap))
  CorMat[distance.matrix == 0] <- 1
  CorMat
}

#' simulate completely spatially random point patterns.
#'
#' simulates a completely spatially random point patterns. This function is
#' only used in simulating data sets.
#'
#' @param npoints number of points to add that are completely spatially random (CSR), default = 100
#' @param lower_x_lim left limit of boundary, default = 0
#' @param upper_x_lim right limit of boundary, default = 1
#' @param lower_y_lim lower limit of boundary, default = 0
#' @param upper_y_lim upper limit of boundary, default = 1
#'
#' @return data.frame of two columns, x-coordinate in the first, and y-coordinate in the second.
#'
#' @author Jay Ver Hoef

pointSimCSR <- function(npoints = 100, lower_x_lim = 0, upper_x_lim = 1,
  lower_y_lim = 0, upper_y_lim = 1)
{
  x_range <- upper_x_lim - lower_x_lim
  y_range <- upper_y_lim - lower_y_lim
  tibble::data_frame(x=lower_x_lim + runif(npoints)*x_range,
    y=lower_y_lim + runif(npoints)*y_range)
}

#' Creates a systematic grid of points.
#'
#' Creates a systematic grid of points. This function is only used
#' in simulating data sets.
#'
#' @param lower_x_lim the lower limit for x-coordinate, default is 0
#' @param upper_x_lim the upper limit for x-coordinate, default is 1
#' @param lower_y_lim the lower limit for y-coordinate, default is 0
#' @param upper_y_lim the upper limit for y-coordinate, default is 1
#' @param ncol the number of cols in the systematic grid, default is 10
#' @param nrow the number of rows in the systematic grid, default is 10

#' @return A data.frame with x- and y-coordinates of simulated locations
#' @author Jay Ver Hoef
#' @export

pointSimSyst <- function (nrow = 10, ncol = 10, lower_x_lim = 0, upper_x_lim = 1,
  lower_y_lim = 0, upper_y_lim = 1)
{
  x_range <- upper_x_lim - lower_x_lim
  y_range <- upper_y_lim - lower_y_lim
  y_mat <- lower_y_lim + y_range * (nrow - matrix(rep(1:nrow,
    times = ncol), nrow = nrow, ncol = ncol))/(nrow) + y_range/(2 *
        nrow)
  x_mat <- lower_x_lim + x_range * (t(matrix(rep(1:ncol, times = nrow),
    nrow = ncol, ncol = nrow)) - 1)/(ncol) + x_range/(2 *
        ncol)
  data.frame(x = matrix(x_mat, ncol = 1), y = matrix(y_mat,
    ncol = 1))
}





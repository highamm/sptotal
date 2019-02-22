#' Estimate Covariance Parameters
#'
#' Used to estimate spatial covariance parameters for a few different spatial models.
#' Estimated parameters can then be used in \code{FPBKpred} to predict unobserved values.
#'
#' The function is used internally in \code{FPBKpred}.
#'
#' @param response a vector of a response variable, possibly with
#' missing values.
#' @param designmatrix is the matrix of covariates used to regress
#' the response on.
#' @param xcoordsvec is a vector of x coordinates
#' @param ycoordsvec is a vector of y coordinates
#' @param CorModel is the covariance structure. By default, \code{covstruct} is
#' @param estmethod is either the default \code{"REML"} for restricted
#' maximum likelihood to estimate the covariance parameters and
#' regression coefficients or \code{"ML"} to estimate the covariance
#' parameters and regression coefficients.
#' Exponential but other options include the Spherical and the Gaussian.
#' @return a list with \itemize{
#'    \item a vector of estimated covariance parameters
#'    \item the fitted covariance matrix for all of the sites
#' }
#' @export

estcovparm_jay <- function(response, designmatrix, xcoordsvec, ycoordsvec,
  CorModel = "Exponential", estmethod = "REML") {

  ## only estimate parameters using sampled sites only

  ind.sa <- !is.na(response)
  designmatrixsa <- designmatrix[ind.sa, ]


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

  ## perform a grid search on log scale to find an appropriate
  ## starting value for optim. The grid search covers a variety
  ## of nugget to partial sill ratios as well as a few different
  ## range parameters spanning the maximum distance of the distance matrix

  possible_nug_prop <- c(.25, .5, .75)
  possible.theta1 <- log(possible_nug_prop/(1 - possible_nug_prop))
  possible.range <- c(max(distmat), max(distmat) / 2, max(distmat) / 4)
  possible.theta2 <- log(possible.range)
  theta <- expand.grid(possible.theta1, possible.theta2)
 
  m2loglik <- rep(NA, nrow(theta))

    for (i in 1:nrow(theta)) {
      m2loglik[i] <- m2LL_jay(theta = theta[i, ],
        zcol = response[ind.sa],
        XDesign = as.matrix(designmatrixsa),
        xcoord = xcoordsvec[ind.sa], ycoord = ycoordsvec[ind.sa],
        CorModel = CorModel,
        estmethod = estmethod)
    }

    max.lik.obs <- which(m2loglik == min(m2loglik))

    ## optimize using Nelder-Mead
    parmest <- optim(theta[max.lik.obs, ], m2LL_jay,
      zcol = response[ind.sa],
      XDesign = as.matrix(designmatrixsa),
      xcoord = xcoordsvec[ind.sa], ycoord = ycoordsvec[ind.sa],
      method = "Nelder-Mead",
      CorModel = CorModel, estmethod = estmethod)

    ## extract the covariance parameter estimates. When we deal with covariance
    ## functions with more than 3 parameters, this section will need to be modified
    min2loglik <- parmest$value
    
    nug_prop <- exp(parmest$par[1])/(1 + exp(parmest$par[1]))
    range.effect <- exp(parmest$par[2])
    
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
    if (nug_prop < 0.001) {
      Sigma <- Sigma + diag(0.1, nrow = nrow(Sigma))
    }
    # get Sigma for sampled sites only
    Sigma_samp = Sigma[ind.sa, ind.sa]
    
    ## use QR decomposition, it is more stable and faster
    qrV <- qr(Sigma_samp)
    ViX <- solve(qrV,as.matrix(designmatrixsa))
    covbi <- crossprod(as.matrix(designmatrixsa),ViX) 
    covb <- mginv(covbi, tol = 1e-21)
    b.hat <- covb %*% t(as.matrix(designmatrixsa)) %*% 
      solve(qrV,response[ind.sa])
    r <- response[ind.sa] - as.matrix(designmatrixsa) %*% b.hat
    
    # the estimated sill
    sill = as.numeric(crossprod(r, solve(qrV,r)))/n
    if(estmethod == 'REML') sill = n*sill/(n - p)
    
    Sigma = sill*Sigma

    parms.est <- c(nug_prop*sill, (1-nug_prop)*sill, range.effect)

  return(list(parms.est = parms.est, Sigma = Sigma, qrV = qrV, 
    b.hat = b.hat, covbi = covbi, covb = covb, min2loglik = min2loglik))
}


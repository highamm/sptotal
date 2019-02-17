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
#' Exponential but other options include the Spherical and the Gaussian.
#' @return a list with \itemize{
#'    \item a vector of estimated covariance parameters
#'    \item the fitted covariance matrix for all of the sites
#' }
#' @export estcovparm

estcovparm <- function(response, designmatrix, xcoordsvec, ycoordsvec,
  CorModel = "Exponential") {

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

  possible.nugget <- c(stats::var(response[ind.sa]),
    stats::var(response[ind.sa]) / 2,
    stats::var(response[ind.sa]) / 4)
  possible.theta1 <- log(possible.nugget)
  possible.parsil <- c(stats::var(response[ind.sa]),
    stats::var(response[ind.sa]) / 2,
    stats::var(response[ind.sa]) / 4)
  possible.theta2 <- log(possible.parsil)
  possible.range <- c(max(distmat), max(distmat) / 2, max(distmat) / 4)
  possible.theta3 <- log(possible.range)

  theta <- expand.grid(possible.theta1, possible.theta2, possible.theta3)



  m2loglik <- rep(NA, nrow(theta))

    for (i in 1:nrow(theta)) {
      m2loglik[i] <- m2LL.FPBK.nodet(theta = theta[i, ],
        zcol = response[ind.sa],
        XDesign = as.matrix(designmatrixsa),
        xcoord = xcoordsvec[ind.sa], ycoord = ycoordsvec[ind.sa],
        CorModel = CorModel)
    }

    max.lik.obs <- which(m2loglik == min(m2loglik))

    ## optimize using Nelder-Mead
    parmest <- optim(theta[max.lik.obs, ], m2LL.FPBK.nodet,
      zcol = response[ind.sa],
      XDesign = as.matrix(designmatrixsa),
      xcoord = xcoordsvec[ind.sa], ycoord = ycoordsvec[ind.sa],
      method = "Nelder-Mead",
      CorModel = CorModel)

    ## extract the covariance parameter estimates. When we deal with covariance
    ## functions with more than 3 parameters, this section will need to be modified
    min2loglik <- parmest$value

    nugget.effect <- exp(parmest$par[1])
    parsil.effect <- exp(parmest$par[2])
    range.effect <- exp(parmest$par[3])
    parms.est <- c(nugget.effect, parsil.effect, range.effect)

    if (CorModel == "Exponential") {
      Sigma <- parsil.effect *
        corModelExponential(distmatall, range.effect) +
        diag(nugget.effect, nrow = nrow(distmatall)) 
    } else if (CorModel == "Gaussian") {
      Sigma <- parsil.effect * 
        (corModelGaussian(distmatall, range.effect)) + 
        diag(nugget.effect, nrow = nrow(distmatall))
    } else if (CorModel == "Spherical") {
      Sigma <-  parsil.effect *
        corModelSpherical(distmatall, range.effect) +
        diag(nugget.effect, nrow = nrow(distmatall)) 
    }

    ## diagonalization to stabilize the resulting covariance matrix
    if (nugget.effect / parsil.effect < 0.001) {
      Sigma <- Sigma + diag(0.1, nrow = nrow(Sigma))
    }
    
   # Sigmai <- solve(Sigma)
    ## spherical code

  # for (i in 1:nrow(theta)) {
  #   m2loglik[i] <- m2LL.FPBK.nodet.sph(theta = theta[i, ], zcol = yvar[ind.sa],
  #     XDesign = XDesign,
  #     xcoord = data.sa$xcoordsUTM, ycoord = data.sa$ycoordsUTM)
  # }
  # 
  # max.lik.obs <- which(m2loglik == min(m2loglik))
  # 
  # ## optimize using Nelder-Mead
  # parmest <- optim(theta[max.lik.obs, ], m2LL.FPBK.nodet.sph,
  #   zcol = data.sa$counts,
  #   XDesign = XDesign,
  #   xcoord = data.sa$xcoordsUTM, ycoord = data.sa$ycoordsUTM,
  #   method = "Nelder-Mead")
  # 
  # min2loglik <- parmest$value
  # 
  # nugget.effect <- exp(parmest$par[1])
  # parsil.effect <- exp(parmest$par[2])
  # range.effect <- exp(parmest$par[3])
  # parms.est <- c(nugget.effect, parsil.effect, range.effect)
  # 
  # cormatSpher <- 1 - (3 / 2) * (distmat / range.effect) +
  #   (1 / 2) * (distmat / range.effect) ^ 3
  # cormatSpher[distmat > range.effect] <- 0
  # Sigma <- diag(nugget.effect, nrow = nrow(distmat)) +
  #   parsil.effect * cormatSpher
  # 
  # Sigmai <- solve(Sigma)


  return(list(parms.est, Sigma))
}

# counts <- c(1, NA, NA, NA, 3, 1:13, 21, 30)
# pred1 <- runif(20, 0, 1); pred2 <- rnorm(20, 0, 1)
# xcoords <- runif(20, 0, 1); ycoords <- runif(20, 0, 1)
# dummyvar <- runif(20, 0, 1)
# CorModel = "Gaussian"
# xcoordssamp <- xcoords[is.na(counts) == FALSE]
# ycoordssamp <- ycoords[is.na(counts) == FALSE]
# data <- as.data.frame(cbind(counts, pred1, pred2, xcoords, ycoords, dummyvar))
# 
# Xdesigntest <- model.matrix(formula)
# formula <- counts ~ pred1 + pred2

##estcovparm(response = counts, designmatrix = Xdesigntest,
## xcoordsvec = xcoords,
##  ycoordsvec = ycoords, CorModel = "Gaussian")[[3]]

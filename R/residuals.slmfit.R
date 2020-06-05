#' Extract Model Residuals from an \code{slmfit} object.
#'
#' @param object a \code{slmfit} object generated from the \code{\link{slmfit}()} function.
#' @param cross.validation a logical (\code{TRUE} or \code{FALSE}) that
#' indicates whether the residuals computed should be found using
#' leave one out cross-validation. Set to \code{FALSE} by default.
#' @param ... further arguments passed to or from other methods.
#' @return a vector of residuals, consisting of each observed response/density minus the estimated mean, or, in the case of cross-validation, the observed response/density minus the leave-one-out-cross-validation prediction.
#' @examples
#' data(exampledataset) ## load a toy data set
#' slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
#' xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
#' residuals(slmobj)
#' residuals(slmobj, cross.validation = TRUE)
#' @export

residuals.slmfit <- function(object, cross.validation = FALSE, ...) {
  resid.vec <- object$resids

  if(cross.validation == TRUE) {
  z <- object$Density
  X <- as.matrix(object$DesignMat)
  V <- object$FPBKpredobj$covmatsamp
  Vi <- object$FPBKpredobj$covmatsampi
  n <- length(z)
  cdd.out <- matrix(-999.9, nrow = n, ncol = 2)

  for(i in 1:n) {
    Vi.i <- Vi[(1:n) != i, (1:n) != i] -
      matrix(Vi[(1:n) != i,i], ncol = 1) %*%
      matrix(Vi[i, (1:n) != i], nrow = 1) / Vi[i,i]
    c.i <- matrix(V[(1:n) != i,i], ncol = 1)
    xi <- matrix(X[i,], ncol = 1)
    X.i <- X[(1:n) != i,]
    z.i <- matrix(z[(1:n) != i], ncol = 1)
    xxi <- xi - t(X.i) %*% Vi.i %*% c.i
    covb.i <- solve(t(X.i) %*% Vi.i %*% X.i)
    si <- V[i,i] - t(c.i) %*% Vi.i %*% c.i
    lam <- t(c.i + X.i %*% covb.i %*% xxi) %*% Vi.i

    cdd.out[i, 1] <- lam %*% z.i
    ## cdd.out[i, 2] <- sqrt(si + t(xxi) %*% covb.i %*% xxi)

  }
  cdd.out[ ,2] <- z - cdd.out[ ,1]
  cdd.out <- as.data.frame(cdd.out)
  names(cdd.out) <- c("cv.pred","cv.resid")
  resid.vec <- cdd.out[ ,2]
}

  return(resid.vec)
}


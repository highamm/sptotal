#' Computes the Generalized R-squared.
#'
#' @param object is an object of class \code{slmfit}.
#' @return the value for generalized R-squared
#' @import stats
#' @export GR2


GR2 <- function(object) {
 if (class(object) != "slmfit") return("Not a slmfit object")
  W <- object$DesignMat
  Vi <- object$FPBKPredobj$covmatsampi
  z <- object$Density
  betahat <- object$CoefficientEsts
  muhat <- sum(Vi %*% z) / sum(Vi)
  ones <- matrix(1, ncol = 1, nrow = length(z))
  1 - t(z - W %*% betahat) %*% Vi %*% (z - W %*% betahat) /
    t(z - ones %*% muhat) %*% Vi %*% (z - ones %*% muhat)
}


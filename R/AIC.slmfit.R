#' Extract the AIC from a \code{\link{slmfit}} object for comparing models.
#'
#' @param object a \code{\link{slmfit}} object
#' @param ... further arguments passed to or from other methods.
#' @return The AIC value of the slmfit object.  Here, AIC is computed as 2 times the
#' negative log-likelihood plus 2 times the number of model parameters for Maximum Likelihood (ML) and 2 times the negative log-likelihood for REML. For REML, AIC should only be used to compare two models with the same covariates but different spatial covariance structures.
#' @examples
#' data(exampledataset) ## load a toy data set
#' slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
#' xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
#' AIC(slmobj)
#' @export

AIC.slmfit <- function(object, ...) {

  if(object$FPBKpredobj$estmethod == 'ML')
    return(object$minus2loglike + 2 * length(object$SpatialParmEsts) +
      2 * ncol(object$DesignMat))
  if(object$FPBKpredobj$estmethod == 'REML')
    return(object$minus2loglike + 2 * length(object$SpatialParmEsts))

}

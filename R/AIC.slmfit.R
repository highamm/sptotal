#' Extract AIC
#'
#' @param object a slmfit object
#' @param ... additional arguments
#' @return The AIC value of the slmfit object.  Here, AIC is computed as 2 times the
#' negative log-likelihood plus 2 times the number of model parameters.  Thus,
#' lower AIC values are preferred models.
#' @export

AIC.slmfit <- function(object, ...)
{

  if(object$FPBKpredobj$estmethod == 'ML')
    return(object$minus2loglike + 2 * length(object$SpatialParmEsts) +
      2 * ncol(object$DesignMat))
  if(object$FPBKpredobj$estmethod == 'REML')
    return(object$minus2loglike + 2 * length(object$SpatialParmEsts))

}

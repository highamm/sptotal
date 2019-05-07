#' Extract Model Residuals
#'
#' @param object a slmfit object
#' @param ... additional arguments
#' @return a vector of residuals, consisting of each observed count minus the estimated mean.
#' @export

residuals.slmfit <- function(object, ...)
{
  resid.vec <- object$resids
  return(resid.vec)
}

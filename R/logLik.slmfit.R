#' Extract Log-Likelihood
#'
#' @param object is a slmfit object 
#' @return the log-likelihood of the slmfit object
#' @export 

logLik.slmfit <- function(object) 
{

  0.5*(-object$minus2loglike) 

}


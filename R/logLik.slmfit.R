#' Extract Log-Likelihood from a fitted class \code{slmfit} object
#'
#' @param object is a class \code{slmfit} object
#' @param ... additional arguments
#' @return the log-likelihood of the model fit in the \code{slmfit} object
#'
#' @examples
#' data(exampledataset) ## load a toy data set
#' slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
#' xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
#' loglik.slmfit(slmobj)
#' @export

loglik.slmfit <- function(object, ...) {

  if (class(object) != "slmfit") return("Not a slmfit object")
  0.5 * (-object$minus2loglike)

}


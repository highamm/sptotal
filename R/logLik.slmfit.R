#' Extract Log-Likelihood from a fitted \code{slmfit()} object
#'
#' @param object is a slmfit object
#' @param ... additional argurments
#' @return the log-likelihood of the slmfit object
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


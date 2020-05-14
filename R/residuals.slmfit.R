#' Extract Model Residuals from an \code{slmfit()} object.
#'
#' @param object a slmfit object
#' @param ... additional arguments
#' @return a vector of residuals, consisting of each observed count minus the estimated mean.
#' @examples
#' data(exampledataset) ## load a toy data set
#' slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
#' xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
#' residuals(slmobj)
#' @export

residuals.slmfit <- function(object, ...) {
  resid.vec <- object$resids
  return(resid.vec)
}

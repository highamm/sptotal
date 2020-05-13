#' Extract Model Coefficients from a \code{slmfit} object
#'
#' @param object a \code{slmfit} object
#' @param ... additional arguments
#' @return a vector of fitted model coefficients.
#' @examples
#' data(exampledataset) ## load a toy data set
#' slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
#' xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
#' coef(slmobj)
#' @export

coef.slmfit <- function(object, ...)
{
  coef.vec <- object$CoefficientEsts
  return(coef.vec)
}

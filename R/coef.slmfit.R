#' Extract Model Coefficients from a \code{\link{slmfit}} object
#'
#' @param object a \code{\link{slmfit}} object
#' @param ... further arguments passed to or from other methods.
#' @return a vector of fitted model coefficients.
#' @examples
#' data(exampledataset) ## load a toy data set
#' slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
#' xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
#' coef(slmobj)
#' @export

coef.slmfit <- function(object, ...) {
  coef.vec <- object$CoefficientEsts
  names(coef.vec) <- object$PredictorNames
  return(coef.vec)
}

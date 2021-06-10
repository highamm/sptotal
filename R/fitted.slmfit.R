#' Extract Fitted Values from an \code{slmfit} object.
#'
#' @param object a \code{slmfit} object generated from the \code{\link{slmfit}()} function.
#' @param ... further arguments passed to or from other methods.
#'
#' @return a vector of fitted values (estimated means)
#' @export
#'
#' @examples
#' data(exampledataset) ## load a toy data set
#' slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
#' xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
#' fitted(slmobj)
fitted.slmfit <- function(object, ...) {
  return(object$FittedValues)
}

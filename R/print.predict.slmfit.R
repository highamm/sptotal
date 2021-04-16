#' Prints a short summary for the \code{\link{predict.slmfit}()} function.
#'
#' This function uses the object that is output from \code{\link{predict.slmfit}()} of class \code{predict.slmfit}..
#'
#' @param x is a prediction object generated from \code{\link{predict.slmfit}()}
#' @param digits is the number of digits to be displayed in the
#' model output
#' @param ... further arguments passed to or from other methods.
#' @return a list with \itemize{
#'   \item Prediction, the prediction of interest.
#'   \item Standard Error, the standard error for the prediction.
#'   \item a preview of the data set with both sampled and predicted
#'    observations, showing the first 6 and the last 6 observations.
#' }
#' @examples
#' data(exampledataset) ## load a toy data set
#' slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
#' xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
#' print(predict(slmobj), digits = 4)
#' @import stats
#' @export

print.predict.slmfit <- function(x, digits =
                                   max(3L, getOption("digits") - 3L),
                                 ...) {
  cat("Prediction:\n")
  print(x$FPBK_Prediction, digits = digits)

  cat("\n Standard Error:\n")
  print(sqrt(x$PredVar), digits = digits)

  cat("\nPrediction Data Frame Preview: First 6 and Last 6 Observations\n")
  print(base::rbind(utils::head(x$Pred_df), utils::tail(x$Pred_df)))


}

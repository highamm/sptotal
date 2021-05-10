#' Prints a short summary for the \code{\link{predict.stratafit}()} function.
#'
#' This function uses the object that is output from \code{\link{predict.stratafit}()} of class \code{predict.stratafit}..
#'
#' @param x is a prediction object generated from \code{\link{predict.stratafit}()}
#' @param digits is the number of digits to be displayed in the
#' model output
#' @param ... further arguments passed to or from other methods.
#' @return a list with \itemize{
#'   \item a table of predictions, standard errors, and confidence
#'    intervals for each stratum and for the total.
#' }
#' @examples
#' data(exampledataset) ## load a toy data set
#' exampledataset$strata <- c(rep("A", 19), rep("B", 21))
#' strataobj <- stratafit(formula = counts ~ pred1 + pred2,
#'  data = exampledataset, stratacol = "strata",
#' xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
#' predict(strataobj)
#' @import stats
#' @export

print.predict.stratafit <- function(x, digits =
                               max(3L, getOption("digits") - 3L),
                             ...) {

  cat("Note: The full prediction data set can be accessed with `x$Pred_df`,
      where x is the name of of the object generated from
      predict.stratafit() \n")

  cat("\nPrediction and Confidence Intervals:\n")

  print(x[[1]], digits = digits)

}

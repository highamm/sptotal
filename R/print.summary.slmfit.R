#' Prints the summary of a fitted spatial linear model.
#'
#' This function uses the object that is output from \code{\link{summary.slmfit}()}.
#'
#' @param x is an summary object generated from \code{\link{summary.slmfit}()}
#' @param digits is the number of digits to be displayed in the
#' model output
#' @param signif.stars is an option to show which predictors
#' are significant.
#' @param ... further arguments passed to or from other methods.
#' @return a list with \itemize{
#'   \item model formula
#'   \item summary statistics for the residuals.
#'   \item a table of fixed effects estimates and associated standard errors.
#'   \item estimated spatial covariance parameter estimates.
#'   \item generalized r-squared value.
#' }
#' @examples
#' data(exampledataset) ## load a toy data set
#' slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
#' xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
#' print(summary(slmobj))
#' @import stats
#' @export

print.summary.slmfit <- function(x,
  digits = max(3L, getOption("digits") - 3L),
  signif.stars = getOption("show.signif.stars"), ...) {


  cat("\nCall:\n", paste(deparse(x$catCall),
    sep = "\n", collapse = "\n"),
    "\n", sep = "")

  cat("\nResiduals:\n")
  resQ <- c(min(x$Residuals), quantile(x$Residuals,
    p = c(0.25, 0.5, 0.75),
    na.rm = TRUE), max(x$Residuals))
  names(resQ) <- c("Min", "1Q", "Median", "3Q", "Max")
  print(resQ, digits = digits)

  cat("\nCoefficients:\n")
  coefs <- x$FixedEffects
  colnames(coefs) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)")
  printCoefmat(coefs, digits = digits, signif.stars = signif.stars,
    na.print = "NA", ...)

  cat("\nCovariance Parameters:\n")
  print(x$CovarianceParms)

  cat("\nGeneralized R-squared:", x$GeneralizedR2,"\n")
}

#' Prints the fitted coefficient table of a fitted spatial linear model.
#'
#' This function uses the object that is output from \code{\link{slmfit}()} of class \code{slmfit}.
#'
#' @param x is an object generated from \code{\link{slmfit}()}
#' @param digits is the number of digits to be displayed in the
#' model output
#' @param ... further arguments passed to or from other methods.
#' @return a list with \itemize{
#'   \item model formula
#'   \item summary statistics for the residuals.
#'   \item a table of fixed effects estimates and associated standard errors.
#'   \item estimated spatial covariance parameter estimates.
#'   \item generalized r-squared value.
#' }
#' @examples
#' data(exampledataset) ## load a toy data set
#' slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
#' xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
#' print(slmobj)
#' @import stats
#' @export

print.slmfit <- function(x,
                         digits = max(3L, getOption("digits") - 3L),
                         ...) {

  cat("\nCoefficients:\n")
  coefs <- x$CoefficientEsts
  names(coefs) <- x$PredictorNames

  print(coefs, digits = digits)

  cat("\nCovariance Parameters:\n")
  print(x$SpatialParmEsts, digits = digits)
}

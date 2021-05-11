#' Display basic summary information in a tabular form.
#'
#' \code{get.predinfo()} has been depracated. Information given
#' is now provided in the basic print of a
#' \code{\link{predict.slmfit}()} object. Creates a list of tables that shows the prediction, standard error, and
#' confidence interval for the prediction, as well as some summary information
#' about the sample.
#'
#' @param x the output of the \code{\link{predict.slmfit}()} function, of class \code{predict.slmfit}
#' @param conf_level is the confidence level for a normal-based
#' confidence interval (default = 0.90).
#' @return a list of three tables, including \itemize{
#' \item \code{simptab}, which contains the prediction and its standard error,
#' \item \code{confbounds}, which contains a confidence interval for the prediction, and
#' \item \code{outptmat}, a table of sampling information, including the number of sites sampled, the total number of sites, the total observed response, and the
#' observed average density (equal to the average response if all site areas are
#' equal).
#' }
#' @name get.predinfo-deprecated
#' @rdname get.predinfo-deprecated
#' @examples
#' data(exampledataset) ## load a toy data set
#' slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
#' xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
#' predobj <- predict(slmobj)
#' ## Not Run
#' ## get.predinfo(predobj)
#' @export


get.predinfo <- function(x, conf_level = 0.90) {

  .Deprecated("print.predict.slmfit")

  pred.total <- x$FPBK_Prediction
  pred.total.var <- x$PredVar
  formula <- x$formula

  responsevar <- x$Pred_df[ ,base::all.vars(formula)[1]]
  sampind <- x$Pred_df[ ,paste(base::all.vars(formula)[1], "_sampind",
    sep = "")]
  density <- x$Pred_df[ ,paste(base::all.vars(formula)[1], "_pred_density",
    sep = "")]
  density_samp <- density[sampind == 1]
  meandensity <- mean(density_samp)

  simptab <- t(matrix(c(pred.total, sqrt(pred.total.var))))
  colnames(simptab) <- c("Prediction", "SE(Prediction)")

  confbounds <- matrix(c(as.numeric(pred.total) + c(1, -1) *
      stats::qnorm((1 - conf_level) / 2) *
      sqrt(as.numeric(pred.total.var))), nrow = 1)
  labs <- c("Lower Bound", "Upper Bound")
  colnames(confbounds) <- labs

  nsitessampled <- sum(sampind)
  nsitestotal <- nrow(x$Pred_df)
  animalscounted <- sum(responsevar[sampind == 1])

  outptmat <- t(matrix(c(nsitessampled, nsitestotal, animalscounted,
    meandensity)))
  colnames(outptmat) <- c("Numb. Sites Sampled", "Total Numb. Sites",
    "Total Observed", "Average Density")

  tabs <- list(simptab, confbounds, outptmat)
  names(tabs) <- c("Prediction", paste(conf_level * 100,
    "% Confidence_Interval", sep = ""), "Sampling_Information")
  return(tabs)

}

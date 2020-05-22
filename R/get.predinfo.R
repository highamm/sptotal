#' Display basic summary information in a tabular form.
#'
#' Creates a list of tables that shows the prediction, standard error, and
#' confidence interval for the prediction, as well as some summary information
#' about the sample.
#'
#' @param x the output of the \code{predict.slmfit()} function, of class \code{sptotalPredOut}
#' @param conf_level is the confidence level for a normal-based
#' confidence interval (default = 0.90).
#' @return a list of three tables, including \itemize{
#' \item \code{simptab}, which contains the prediction and its standard error,
#' \item \code{confbounds}, which contains a confidence interval for the prediction, and
#' \item \code{outptmat}, a table of sampling information, including the number of sites sampled, the total number of sites, and the total observed count.
#' }
#' data(exampledataset) ## load a toy data set
#' slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
#' xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
#' predobj <- predict.slmfit(slmobj)
#' get.predinfo(predobj)
#' @export


get.predinfo <- function(x, conf_level = 0.90) {

  pred.total <- x$FPBK_Prediction
  pred.total.var <- x$PredVar
  formula <- x$formula

  responsevar <- x$Pred_df[ ,base::all.vars(formula)[1]]
  sampind <- x$Pred_df[ ,paste(base::all.vars(formula)[1], "_sampind",
    sep = "")]

  simptab <- t(matrix(c(pred.total, sqrt(pred.total.var))))
  colnames(simptab) <- c("Prediction", "SE(Prediction)")

  confbounds <- matrix(c(round(as.numeric(pred.total) + c(1, -1) *
      stats::qnorm((1 - conf_level) / 2) *
      sqrt(as.numeric(pred.total.var))),
    as.vector(round(stats::qnorm((1 - conf_level) / 2) * -1 *
        sqrt(as.numeric(pred.total.var)) / pred.total, 2))), nrow = 1)
  labs <- c("Lower Bound", "Upper Bound", "Proportion of Mean")
  colnames(confbounds) <- labs

  nsitessampled <- sum(sampind)
  nsitestotal <- nrow(x$Pred_df)
  animalscounted <- sum(responsevar[sampind == 1])

  outptmat <- t(matrix(c(nsitessampled, nsitestotal, animalscounted)))
  colnames(outptmat) <- c("Numb. Sites Sampled", "Total Numb. Sites",
    "Total Observed")

  tabs <- list(simptab, confbounds, outptmat)
  names(tabs) <- c("Prediction", paste(conf_level * 100, "% Confidence_Interval", sep = ""), "Sampling_Information")
  return(tabs)

}

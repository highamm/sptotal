#' Display basic summary information in a tabular form.
#'
#' Creates a table that shows the prediction, standard error, and
#' confidence interval for the prediction, as well as some summary information
#' about the sample.
#'
#' @param x the output of the prediction function, of class sptotalPredOut
#' @param conf_level is the confidence level for a normal-based
#' confidence interval (default = 0.90)
#' @return a list of tables with summary information about the predictions
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
  ##print(simptab)

  confbounds <- matrix(c(round(as.numeric(pred.total) + c(1, -1) *
      stats::qnorm((1 - conf_level) / 2) *
      sqrt(as.numeric(pred.total.var))),
    as.vector(round(stats::qnorm((1 - conf_level) / 2) * -1 *
        sqrt(as.numeric(pred.total.var)) / pred.total, 2))), nrow = 1)
  labs <- c("Lower Bound", "Upper Bound", "Proportion of Mean")
  colnames(confbounds) <- labs
  ##print(confbounds)

  nsitessampled <- sum(sampind)
  nsitestotal <- nrow(x$Pred_df)
  animalscounted <- sum(responsevar[sampind == 1])

  outptmat <- t(matrix(c(nsitessampled, nsitestotal, animalscounted)))
  colnames(outptmat) <- c("Numb. Sites Sampled", "Total Numb. Sites",
    "Total Observed Count")
  ##print(outptmat)

  tabs <- list(simptab, confbounds, outptmat)
  names(tabs) <- c("Prediction", "Confidence_Interval", "Sampling_Information")
  print(tabs)
  return(tabs)

}

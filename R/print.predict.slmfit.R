#' Prints a short summary for the \code{\link{predict.slmfit}()} function.
#'
#' This function uses the object that is output
#' from \code{\link{predict.slmfit}()} of class \code{predict.slmfit}.
#' @param x is a prediction object generated from \code{\link{predict.slmfit}()}
#' @param digits is the number of digits to be displayed in the
#' model output
#' @param ... further arguments passed to or from other methods.
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

  pred.total <- x$FPBK_Prediction
  pred.total.var <- x$PredVar
  formula <- x$formula
  conf_level <- x$conf_level

  responsevar <- x$Pred_df[ ,base::all.vars(formula)[1]]
  sampind <- x$Pred_df[ ,paste(base::all.vars(formula)[1], "_sampind",
                               sep = "")]
  density <- x$Pred_df[ ,paste(base::all.vars(formula)[1],
                               "_pred_density",
                               sep = "")]
  density_samp <- density[sampind == 1]
  meandensity <- mean(density_samp)

  simptab <- t(matrix(c(pred.total, sqrt(pred.total.var))))
  colnames(simptab) <- c("Prediction", "SE")

  confbounds <- matrix(c(as.numeric(pred.total) + c(1, -1) *
                           stats::qnorm((1 - conf_level) / 2) *
                           sqrt(as.numeric(pred.total.var))),
                       nrow = 1)
  labs <- c(paste(conf_level * 100, "% LB", sep = ""),
            paste(conf_level * 100,
                  "% UB", sep = ""))
  colnames(confbounds) <- labs

  basic_tab <- cbind(simptab, confbounds)
  rownames(basic_tab) <- base::all.vars(formula)[1]

  cat("Prediction Info:\n")
  print(basic_tab, digits = digits)

  nsitessampled <- sum(sampind)
  nsitestotal <- nrow(x$Pred_df)
  animalscounted <- sum(responsevar[sampind == 1])

  outptmat <- t(matrix(c(nsitessampled, nsitestotal, animalscounted,
                         meandensity)))
  colnames(outptmat) <- c("Numb. Sites Sampled", "Total Numb. Sites",
                          "Total Observed", "Average Density")
  rownames(outptmat) <- base::all.vars(formula)[1]

  print(outptmat, digits = digits)

  # cat("\nPrediction Data Frame Preview: First 6 and Last 6 Observations\n")
  # print(base::rbind(utils::head(x$Pred_df), utils::tail(x$Pred_df)))

}

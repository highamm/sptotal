#' Perform Finite Population Block Kriging
#'
#' Uses an object of class \code{stratafit} from the \code{\link{stratafit}()}
#'  function to predict the response on the unsampled sites for separate strata.
#' The column of the data set that has the response should have numeric values for the observed response
#' on the sampled sites and `NA` for any site that was not sampled.
#' Note that there is no \code{newdata} argument to
#' \code{predict.stratafit()}: any point in space for which a prediction
#' is needed should be included in the original data set in \code{\link{stratafit}()}
#' with the response variable as \code{NA}.
#'
#' @param object is an object generated from \code{\link{stratafit}()}
#' @param wtscol is the name of the column that contains the weights for prediction. The default setting predicts the population total
#' @param ... further arguments passed to or from other methods.
#' @return a list with \itemize{
#'   \item the estimated population total
#'   \item the estimated prediction variance
#'   \item a data frame containing \enumerate{
#'        \item x-coordinates
#'        \item y-coordinates
#'        \item density predictions
#'        \item count predictions
#'        \item site-by-site density prediction variances
#'        \item site-by-site count prediction variances
#'        \item indicator variable for whether or not the each site was sampled
#'        \item estimated mean for each site
#'        \item area of each site
#'        }
#'    \item vector with estimated covariance parameters
#'    \item the formula used to fit the model in \code{slmfit()}
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

predict.stratafit <- function(object, wtscol = NULL, ...) {

  predict_outs <- lapply(object, FUN = predict, wtscol = wtscol)

  preds <- lapply(predict_outs, `[[`, 1)
  vars <- lapply(predict_outs, `[[`, 2)
  dfs <- lapply(predict_outs, `[[`, 3)

  pred_out <- do.call("sum", preds)

  ses <- lapply(vars, sqrt)
  totalvar_out <- do.call("sum", vars)
  se_out <- sqrt(totalvar_out)

  df_out <- do.call("rbind", dfs)

  strata_out <- list(preds, pred_out, vars, se_out, df_out)
  total_out <- matrix(c(pred_out, se_out), nrow = 1)

  pred_se_mat <- rbind(cbind(matrix(unlist(preds)),
                             matrix(unlist(ses))), total_out)

  conf_level <- 0.9
  lbs <- matrix(pred_se_mat[ ,1] + 1 *
    stats::qnorm((1 - conf_level) / 2) * pred_se_mat[ ,2])
  ubs <- matrix(pred_se_mat[ ,1] - 1 *
    stats::qnorm((1 - conf_level) / 2) * pred_se_mat[ ,2])
  output_mat <- cbind(pred_se_mat, lbs, ubs)

  rownames(output_mat) <- c(names(dfs), "Total")
  colnames(output_mat) <- c("Prediction", "SE",
                            paste(conf_level * 100, "% LB", sep = ""),
                            paste(conf_level * 100,
                                  "% UB", sep = ""))
  output_obj <- list(output_mat, df_out)
  names(output_obj) <- c("summary_info", "Pred_df")

  class(output_obj) <- "predict.stratafit"

  return(output_obj)

}

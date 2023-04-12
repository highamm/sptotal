#' Summarizes a fitted spatial linear model with a stratification variable..
#'
#' If a model is fitted with \code{\link{stratafit}()}, then
#' this summary function produces summary output for each level
#' of the stratification variable in the same style as the
#' \code{\link{summary.slmfit}()} function.
#'
#' @param object is an object generated from \code{\link{slmfit}()} of
#' class \code{slmfit}.
#' @param ... further arguments passed to or from other methods.
#' @return a list with \itemize{
#'   \item model formula
#'   \item a table of fixed effects estimates and associated standard errors
#'   \item estimated spatial covariance parameter estimates
#'   \item residuals
#'   \item generalized r-squared.
#'        }
#' @examples
#' data(exampledataset) ## load a toy data set
#' exampledataset$strata <- c(rep("A", 25), rep("B", 15))
#' strataobj <- stratafit(formula = counts ~ pred1 + pred2,
#'  data = exampledataset, stratacol = "strata",
#' xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
#' summary(strataobj)
#' @import stats
#' @export

summary.stratafit <- function(object, ...) {

  fixed_ests <- lapply(object, `[[`, 2) ## aren't actually used
  spatial_ests <- lapply(object, `[[`, 1)

  strata_out <- lapply(object, summary)

  return(strata_out)
}

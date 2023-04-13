#' Semi-variogram computation
#'
#' Compute the empirical semivariogram for a data set with specified
#' x and y spatial coordinates and residuals.
#'
#' @param data A data frame or tibble that contains variables for the
#' x coordinates, y coordinates, and residuals.
#' @param xcoordcol is the name of the column in the data frame with
#' x coordinates or longitudinal coordinates
#' @param ycoordcol is the name of the column in the data frame with
#' y coordinates or latitudinal coordinates
#' @param residcol is the name of the column in the data frame with residuals.
#' @param bins Number of equally spaced semivariogram bins. The default is 15.
#' @param cutoff The maximum spatial distance to be considered. The default is
#' half of the maximum observed distance in the data frame.
#' @param ... Additional arguments to be used by \code{stats::dist()} for
#' spatial distance calculations.
#'
#' @return A data frame with the average distance in each bin (dist), the
#' semivariance (gamma), and the number of unique pairs in each bin (np)
#' @export
#'
#' @examples
#' data(exampledataset) ## load a toy data set
#' slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
#' xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
#' sampleddataset <- na.omit(exampledataset)
#' svexample <- data.frame(
#'   xcoords = sampleddataset$xcoords,
#'   ycoords = sampleddataset$ycoords,
#'   resids = residuals(slmobj)
#' )
#' svdata <- sv(svexample, "xcoords", "ycoords", "resids")

sv <- function(data, xcoordcol, ycoordcol, residcol,
               bins = 15, cutoff = NULL, ...) {

  # compute spatial distances
  dists <- as.matrix(dist(cbind(data[[xcoordcol]], data[[ycoordcol]]), ...))
  dists <- dists[upper.tri(dists)]
  if (is.null(cutoff)) {
    cutoff <- max(dists) / 2
  }
  dists_index <- dists <= cutoff
  dists <- dists[dists_index]

  # compute squared differences
  sqrdiffs <- as.matrix(dist(data[[residcol]]))^2
  sqrdiffs <- sqrdiffs[upper.tri(sqrdiffs)]
  sqrdiffs <- sqrdiffs[dists_index]

  # compute semivariogram classes
  dist_classes <- cut(dists, breaks = seq(0, cutoff, length.out = bins + 1))

  # compute squared differences within each class
  gamma <- tapply(sqrdiffs, dist_classes, function(x) mean(x) / 2)

  # compute pairs within each class
  np <- tapply(sqrdiffs, dist_classes, length)

  # compute average distance within each class
  dist <- tapply(dists, dist_classes, mean)

  # return output
  sv_out <- data.frame(dist, gamma, np)
  # remove NA
  sv_out <- stats::na.omit(sv_out)
  return(sv_out)
}

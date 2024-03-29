#' Plot an Empirical Semi-Variogram of Residuals
#'
#' \code{check.variogram()} has been deprecated:
#' use \code{\link{plot.slmfit}()} instead. Plots an empirical
#' semi-variogram of the residuals from the spatial linear model
#' with the fitted parametric model as a curve overtop of the
#' binned points. By default, the empirical semi-variogram only
#' shows distances that are less than or equal to the maximum
#' distance in the data set divided by 2. Therefore, it's possible
#' that the REML-fitted model will not "fit" the points perfectly.
#'
#' @param object is an object of class \code{\link{slmfit}}.
#' @return a plot of the empirical semi-variogram with the fitted
#' model overlayed.
#' @name check.variogram-deprecated
#' @rdname check.variogram-deprecated
#' @importFrom ggplot2 ggplot
#' @importFrom ggplot2 aes
#' @importFrom ggplot2 .data
#' @examples
#' data(exampledataset) ## load a toy data set
#' slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
#' xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
#' ## Not Run
#' ## check.variogram(slmobj)
#' @export

check.variogram <- function(object) {

  .Deprecated("plot.slmfit")

  covmod <- object$CovarianceMod
  parms <- object$SpatialParmEsts ## nugget, psill, range
  residvec <- as.vector(object$resids)

  formula <- object$FPBKpredobj$formula
  response <- all.vars(formula)[1]
  ind.sa <- !is.na(object$FPBKpredobj$data[ ,response]) ## sampled or not?

  xcoords <- object$FPBKpredobj$xcoordsTM[ind.sa]
  ycoords <- object$FPBKpredobj$ycoordsTM[ind.sa]

  df <- data.frame(xcoords = xcoords, ycoords = ycoords,
    resids = residvec)

  ## code for empirical variogram
  g_obj <- gstat::gstat(formula = resids ~ 1,
    locations = ~ xcoords + ycoords,
    data = df)

  ## use h / 2 as cutoff, where h is max distance in data set
  cutoff_point <- max(stats::dist(cbind(xcoords, ycoords))) / 2

  vario_out <- gstat::variogram(g_obj, cutoff = cutoff_point)
  maxy <- max(vario_out$gamma)

  vartab <- cbind(vario_out$dist, vario_out$gamma,
    vario_out$np)
  colnames(vartab) <- c("Distance", "Gamma", "Number of Pairs")
  covparmmat <- t(matrix(parms))
  colnames(covparmmat) <- c("Nugget", "Partial Sill", "Range")
  ## code for fitted variogram

  maxdist <- max(vario_out$dist)
  x.dist.plot <- seq(0, maxdist, length.out = 300)
  nugget <- parms[1]

  if (covmod == "Exponential") {
    v.modfit <- nugget + parms[2] -
      parms[2] * corModelExponential(x.dist.plot, parms[3])
  } else if (covmod == "Gaussian") {
    v.modfit <- nugget + parms[2] -
      parms[2] * corModelGaussian(x.dist.plot, parms[3])
  } else if (covmod == "Spherical") {
    v.modfit <- nugget + parms[2] -
      parms[2] * corModelSpherical(x.dist.plot, parms[3])
  }

  tab2 <- cbind(x.dist.plot, v.modfit)
  df.plot <- as.data.frame(tab2)

  plot_out <- ggplot(data = vario_out,
    aes(x = .data$dist, y = .data$gamma)) +
    ggplot2::geom_point(aes(size = gstat::variogram(g_obj,
                                             cutoff = cutoff_point)$np)) +
    ggplot2::ylim(0, max(c(maxy * (15 / 14), max(df.plot$v.modfit) * (15 / 14)))) +
    ggplot2::geom_line(data = df.plot, aes(x = .data$x.dist.plot,
                                  y = .data$v.modfit)) +
    ggplot2::xlab("Distance (TM)") +
    ggplot2::ylab("Semi-Variance") +
    ggplot2::ggtitle(paste("Empirical Variogram with Fitted",
      covmod, "Model")) +
    ggplot2::scale_size_continuous("Number of Pairs")

  print(plot_out)
}








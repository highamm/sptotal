#' Plot an Empirical Semi-Variogram of Residuals
#'
#' Plots an empirical semi-variogram of the residuals from the spatial linear model
#' with the fitted parametric model as a curve overtop of the
#' binned points. By default, the empirical semi-variogram only shows distances
#' that are less than or equal to the maximum distance in the data
#' set divided by 2. Therefore, it's possible that the REML-fitted model
#' will not "fit" the points perfectly.
#'
#' @param x is an object of class \code{\link{slmfit}}.
#' @param ... further arguments passed to or from other methods.
#' @return a plot of the empirical semi-variogram with the fitted model overlayed.
#' @import stats
#' @import ggplot2
#' @examples
#' data(exampledataset) ## load a toy data set
#' slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
#' xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
#' plot(slmobj)
#'
#' data(exampledataset) ## load a toy data set
#' exampledataset$strata <- c(rep("A", 19), rep("B", 21))
#' strataobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset, stratacol = "strata",
#' xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
#' plot(strataobj[[1]])
#' plot(strataobj[[2]])
#' @export

plot.slmfit <- function(x, ...) {

  if (inherits(x, "slmfit") == FALSE) {
    stop("x must be of class `slmfit`")
  }

  covmod <- x$CovarianceMod
  parms <- x$SpatialParmEsts ## nugget, psill, range
  residvec <- as.vector(x$resids)

  formula <- x$FPBKpredobj$formula
  response <- all.vars(formula)[1]
  ind.sa <- !is.na(x$FPBKpredobj$data[ ,response]) ## sampled or not?

  xcoords <- x$FPBKpredobj$xcoordsTM[ind.sa]
  ycoords <- x$FPBKpredobj$ycoordsTM[ind.sa]

  df <- data.frame(xcoords = xcoords, ycoords = ycoords,
                   resids = residvec)
  vario_out <- sv(df, "xcoords", "ycoords", "resids", ...)
  vartab <- vario_out
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
                     aes_(x = ~dist, y = ~gamma)) +
    geom_point(aes_(size = ~np)) +
    ylim(0, max(c(max(vario_out$gamma) * (15 / 14),
                  max(df.plot$v.modfit) * (15 / 14)))) +
    geom_line(data = df.plot, aes_(x = ~x.dist.plot, y = ~v.modfit)) +
    xlab("Distance (TM)") +
    ylab("Semi-Variance") +
    ggtitle(paste("Empirical Variogram with Fitted",
                  covmod, "Model")) +
    scale_size_continuous("Number of Pairs")

  print(plot_out)

}

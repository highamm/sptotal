#' Creates a default map from predictions
#'
#' Creates a default map from predictions
#'
#' @param x the output of the prediction function, of class sptotalPredOut
#' @param nbreaks number of breaks for coloring predictions.  Default is 4. Note, breaks at endpoints are automatically added, so nbreaks = 4 creates 5 levels.
#' @param breakMethod method to create breaks.  Currently, there are two options, 'quantile' creates breaks at evenly spaced quantiles of the predictions, while 'even' creates breaks evenly between the minimum and maximum prediction values.
#' @param legend.cex expansion factor for legend
#' @param ... are additional arguments to be passed on to \code{plot}
#' @return a plot with predictions
#' @importFrom viridis viridis
#' @import graphics
#' @export

plot.sptotalPredOut = function(x, nbreaks = 4,
  breakMethod = 'quantile', legend.cex = 1, ...)
{
  # name of column with predictions
  pcolname = paste(base::all.vars(x$formula)[1], "_pred",
      sep = "")
  # use layout to create partitioned plot so we can add color legend to left
  layout(matrix(1:2, nrow = 1), widths = c(3,1))
  # create breaks according to user-specified breakMethod
  if(breakMethod == 'quantile') {
    probs = (1:nbreaks)/(nbreaks + 1)
    brks = c(min(x$Pred_df[,pcolname]) - 1e-10,
      quantile(x$Pred_df[,pcolname], probs = probs),
      max(x$Pred_df[,pcolname]) + 1e-10)
  }
  if(breakMethod == 'even') {
    rang = max(x$Pred_df[,pcolname]) + 1e-10 -
    min(x$Pred_df[,pcolname]) - 1e-10
    brks = c(min(x$Pred_df[,pcolname]) - 1e-10,
      min(x$Pred_df[,pcolname]) - 1e-10 +
        rang*(1:nbreaks)/(nbreaks + 1),
      max(x$Pred_df[,pcolname]) + 1e-10)
  }
  # cut predictions at breakpoints to create a vector of factors and labels
  cuts = cut(x$Pred_df[,pcolname], breaks = brks)
  # create a color palette
  palette = viridis(length(levels(cuts)))
  # create plot of predictions colored by their prediction values
  par(mar = c(5,5,1,1))
  plot(x$Pred_df[,'_xcoordsUTM_'], x$Pred_df[,'_ycoordsUTM_'], pch = 19,
    col = palette[as.integer(cuts)], xlab = 'xcoords',
    ylab = 'ycoords', ...)
  par(mar = c(5,0,1,1))
  # add the legend embedded in a second invisible plot
  plot(c(0,1),c(0,1), type = 'n', xlab = '', ylab = '', xaxt = 'n',
    yaxt = 'n', bty='n')
  legend(.1, .9, legend = levels(cuts), col = palette,
    pch = rep(19, times = length(palette)), cex = legend.cex)
}

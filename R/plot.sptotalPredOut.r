#' Creates a default map from predictions
#'
#' Creates a default map from predictions
#'
#' @param predictions the output of the prediction function, of class sptotalPredOut
#' @param nbreaks number of breaks for coloring predictions.  Default is 4. Note, breaks at endpoints are automatically added, so nbreaks = 4 creates 5 levels.
#' @param breakMethod method to create breaks.  Currently, there are two options, 'quantile' creates breaks at evenly spaced quantiles of the predictions, while 'even' creates breaks evenly between the minimum and maximum prediction values.
#' @param legend.cex expansion factor for legend
#' @return a plot
#' @importFrom viridis viridis
#' @export plot.sptotalPredOut

plot.sptotalPredOut = function(predictions, nbreaks = 4, 
  breakMethod = 'quantile', legend.cex = 1, ...)
{
  # use layout to create partitioned plot so we can add color legend to left
  layout(matrix(1:2, nrow = 1), width = c(3,1))
  # create breaks according to user-specified breakMethod
  if(breakMethod == 'quantile') {
    probs = (1:nbreaks)/(nbreaks + 1)
    brks = c(min(predictions$Pred_df$preddensity) - 1e-10,
      quantile(predictions$Pred_df$preddensity, probs = probs),
      max(predictions$Pred_df$preddensity) + 1e-10)
  }
  if(breakMethod == 'even') {
    rang = max(predictions$Pred_df$preddensity) + 1e-10 -
    min(predictions$Pred_df$preddensity) - 1e-10
    brks = c(min(predictions$Pred_df$preddensity) - 1e-10,
      min(predictions$Pred_df$preddensity) - 1e-10 + 
        rang*(1:nbreaks)/(nbreaks + 1),
      max(predictions$Pred_df$preddensity) + 1e-10)
  }
  # cut predictions at breakpoints to create a vector of factors and labels
  cuts = cut(predictions$Pred_df$preddensity, breaks = brks)
  # create a color palette
  palette = viridis(length(levels(cuts)))
  # store current par() values
  par.orig = par()
  # create plot of predictions colored by their prediction values
  par(mar = c(5,5,1,1))
  plot(predictions$Pred_df$xcoords, predictions$Pred_df$ycoords, pch = 19,
    col = palette[as.integer(cuts)], xlab = 'xcoords',
    ylab = 'ycoords', ...)
  par(mar = c(5,0,1,1))
  # add the legend embedded in a second invisible plot
  plot(c(0,1),c(0,1), type = 'n', xlab = '', ylab = '', xaxt = 'n', 
    yaxt = 'n', bty='n')
  legend(.1, .9, legend = levels(cuts), col = palette,
    pch = rep(19, times = length(palette)), cex = legend.cex)
  return(invisible(par(par.orig)))
}

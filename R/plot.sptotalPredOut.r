#' Creates a default map from predictions
#'
#' Creates a default map from predictions. Note that all predictions are stored
#' in a data frame in the output of \code{predict}. Therefore, if a user
#' would like to create his or her own plot, he or she can easily do so using
#' this data frame.
#'
#' @param x the output of the prediction function, of class sptotalPredOut
#' @return a plot with predictions, with points filled by whether or not a site was sampled.
#' @importFrom viridis viridis
#' @import graphics
#' @export

get.predplot = function(x) {

  pred.vals <- data.frame(x$Pred_df)
  formula <- x$formula

  shapevals <- c(1, 16)

  nameresp <- as.vector((noquote(paste(base::all.vars(formula)[1],
    "_pred",
    sep = ""))))
  sampind <- pred.vals[ ,paste(base::all.vars(formula)[1], "_sampind",
    sep = "")]
  pred.vals$sampindfact_ <- factor(sampind)
  preds <- pred.vals[ ,paste(base::all.vars(formula)[1], "_pred",
    sep = "")]
  pcolname <- paste(base::all.vars(formula)[1], "_pred",
    sep = "")

  p3 <- ggplot2::ggplot(data = pred.vals, aes_(x = ~xcoordsUTM_,
    y = ~ycoordsUTM_, shape = ~sampindfact_)) +  ##)) +
    geom_point(aes(colour = preds)) +
    #, ##size = pointsize,
    ## stroke = 3) +
    scale_fill_viridis_c() +
    scale_colour_viridis_c(name = "Counts") +
    theme_bw() +
    scale_shape_manual("Samp Indicator",
      labels = c("Unsampled", "Sampled"), values = shapevals) +
    theme(panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()) +
    xlab("") + ylab("")

  print(p3)
}

  # # name of column with predictions
  # pcolname = paste(base::all.vars(x$formula)[1], "_pred",
  #     sep = "")
  # # use layout to create partitioned plot so we can add color legend to left
  # layout(matrix(1:2, nrow = 1), widths = c(3,1))
  # # create breaks according to user-specified breakMethod
  # if(breakMethod == 'quantile') {
  #   probs = (1:nbreaks)/(nbreaks + 1)
  #   brks = c(min(x$Pred_df[,pcolname]) - 1e-10,
  #     quantile(x$Pred_df[,pcolname], probs = probs),
  #     max(x$Pred_df[,pcolname]) + 1e-10)
  # }
  # if(breakMethod == 'even') {
  #   rang = max(x$Pred_df[,pcolname]) + 1e-10 -
  #   min(x$Pred_df[,pcolname]) - 1e-10
  #   brks = c(min(x$Pred_df[,pcolname]) - 1e-10,
  #     min(x$Pred_df[,pcolname]) - 1e-10 +
  #       rang*(1:nbreaks)/(nbreaks + 1),
  #     max(x$Pred_df[,pcolname]) + 1e-10)
  # }
  # # cut predictions at breakpoints to create a vector of factors and labels
  # cuts = cut(x$Pred_df[,pcolname], breaks = brks)
  # # create a color palette
  # palette = viridis(length(levels(cuts)))
  # # create plot of predictions colored by their prediction values
  # par(mar = c(5,5,1,1))
  # plot(x$Pred_df[,'_xcoordsUTM_'], x$Pred_df[,'_ycoordsUTM_'], pch = 19,
  #   col = palette[as.integer(cuts)], xlab = 'xcoords',
  #   ylab = 'ycoords', ...)
  # par(mar = c(5,0,1,1))
  # # add the legend embedded in a second invisible plot
  # plot(c(0,1),c(0,1), type = 'n', xlab = '', ylab = '', xaxt = 'n',
  #   yaxt = 'n', bty='n')
  # legend(.1, .9, legend = levels(cuts), col = palette,
  #   pch = rep(19, times = length(palette)), cex = legend.cex)


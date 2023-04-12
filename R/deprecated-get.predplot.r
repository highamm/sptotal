#' Create a default map from predictions
#'
#' Creates a default map for the predictions of unobserved sites.
#' Note that all predictions are stored in a data frame in the output of
#' \code{\link{predict.slmfit}()}. Therefore, if a user
#' would like to create his or her own plot, he or she can easily do so using
#' this data frame.
#'
#' @param x the output of the \code{\link{predict.slmfit}()} function,
#' of class \code{predict.slmfit}
#' @return a plot with x-coordinates on the x-axis and y-coordinates on
#' the y-axis that is coloured by predictions, with points with an X
#' denoting that a site was sampled and filled circles denoting
#' unsampled sites.
#' @name get.predplot-deprecated
#' @rdname get.predplot-deprecated
#' @examples
#' data(exampledataset) ## load a toy data set
#' slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
#' xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
#' predobj <- predict(slmobj)
#' ## Not Run
#' ## get.predplot(predobj)
#' @importFrom viridis viridis
#' @import graphics
#' @import ggplot2
#' @export

get.predplot <- function(x) {

  .Deprecated("plot.predict.slmfit")

  pred.vals <- data.frame(x$Pred_df)
  formula <- x$formula

  shapevals <- c(16, 13)

  nameresp <- as.vector((noquote(paste(base::all.vars(formula)[1],
    "_pred_count", sep = ""))))
  sampind <- pred.vals[ ,paste(base::all.vars(formula)[1], "_sampind",
    sep = "")]
  pred.vals$sampindfact_ <- factor(sampind)
  preds <- pred.vals[ ,paste(base::all.vars(formula)[1], "_pred_count",
    sep = "")]
  pcolname <- paste(base::all.vars(formula)[1], "_pred_count",
    sep = "")

  p3 <- ggplot2::ggplot(data = pred.vals, aes(x = .data$xcoordsTM_,
    y = .data$ycoordsTM_, shape = .data$sampindfact_)) +  ##)) +
    geom_point(aes(colour = preds), size = 4, stroke = 2) +
    #, ##size = pointsize,
    ## stroke = 3) +
    scale_fill_viridis_c() +
    scale_colour_viridis_c(name = "Response") +
    theme_bw() +
    scale_shape_manual("Samp Indicator",
      labels = c("Unsampled", "Sampled"), values = shapevals) +
    theme(panel.background = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank()) +
    xlab("") + ylab("")

  print(p3)
}

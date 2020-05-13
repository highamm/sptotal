#' Create maps and summaries from FPBK results.
#'
#' The main purpose of this function is to take the results from FPBK and make
#' readable maps, a fitted variogram plot, and normal-based prediction intervals. The main input for this function is the output from the \code{FPBK.pred} function.
#'
#' @param pred_info is the output from \code{predict.slmfit} in this package.
#' @param conf_level is the desired confidence level for the prediction. If \code{conf_level} is a vector, then confidence intervals for
#' each element of the vector will be produced.
#' @param get_krigmap is an indicator for whether or not a grid of
#' the kriged responses is returned
#' @param get_sampdetails is an indicator for whether or not a summary
#' of the sampled counts should be output. This summary includes
#' the total number of animals or plants sited, the total area
#' surveyed, the number of sampled sites, the total number of sites,
#' etc.
#' @param get_variogram is an indicator for whether or not
#' a variogram of the residuals should be returned
#' @param CorModel Covariance model used, which is required to obtain
#' the appropriate fitted model semi-variogram.
#' @param get_report is an indicator for whether a PDF report of some
#' of the output from \code{get_krigmap}, \code{get_sampdetails},
#' and \code{get_sampdetails} should be produced.
#' @return \itemize{
#'   \item prediction interval
#'   \item a map of the kriged counts (optional)
#'   \item a summary of the sample data (optional)
#'   \item an empirical variogram from \code{gstat} with the fitted variogram model with details of the empirical variogram and spatial parameter estimates (optional)
#' }
#' @import ggplot2
#' @export FPBKoutput

## next steps: write up delta method
## consider scaling the nugget variance for the stratified sites
## make sure maps show counts, not densities

FPBKoutput <- function(pred_info, conf_level = c(0.80,
  0.90, 0.95),
  get_krigmap = FALSE, get_sampdetails = FALSE,
  get_variogram = FALSE, get_report = FALSE,
  CorModel = "Exponential") {

pred.total <- pred_info$FPBK_Prediction
pred.total.var <- pred_info$PredVar
pred.vals <- data.frame(pred_info$Pred_df)
covparmests <- pred_info$SpatialParms
predvalcol <- noquote(paste(base::all.vars(pred_info$formula)[1],
  "_pred", sep = ""))
predvarcol <- noquote(paste(base::all.vars(pred_info$formula)[1], "_predvar",
  sep = ""))
sampindcol <- noquote(paste(base::all.vars(pred_info$formula)[1], "_sampind",
  sep = ""))
muhatcol <- noquote(paste(base::all.vars(pred_info$formula)[1], "_muhat",
  sep = ""))

confbounds <- matrix(NA, nrow = length(conf_level), ncol = 3)

for (k in 1:length(conf_level)){
confbounds[k, ] <- matrix(c(round(as.numeric(pred.total) + c(1, -1) *
    stats::qnorm((1 - conf_level[k]) / 2) *
    sqrt(as.numeric(pred.total.var))),
  as.vector(round(stats::qnorm((1 - conf_level[k]) / 2) * -1 *
      sqrt(as.numeric(pred.total.var)) / pred.total, 2))), nrow = 1)
}

labs <- c("Lower Bound", "Upper Bound", "Proportion of Mean")

rowlabs <- rep(NA, length(conf_level))
for (j in 1:length(conf_level)) {
  rowlabs[j] <- paste(conf_level[j] * 100, "%")
}

colnames(confbounds) <- labs
rownames(confbounds) <- rowlabs
print(confbounds)

basicpred <- t(matrix(round(c(pred.total, sqrt(pred.total.var)))))
colnames(basicpred) <- c("Predicted Total", "SE(Total)")

if (get_sampdetails == TRUE) {
  nsitessampled <- sum(pred.vals[ ,sampindcol])
  nsitestotal <- nrow(pred.vals)
  animalscounted <- sum(pred.vals[ ,predvalcol][pred.vals[ ,sampindcol] == 1])
  outptmat <- t(matrix(c(nsitessampled, nsitestotal, animalscounted)))
  colnames(outptmat) <- c("Numb. Sites Sampled", "Total Numb. Sites",
    "Numb. Units Counted")
  print(outptmat)

  }

if (get_variogram == TRUE) {
  sampled_df <- data.frame(subset(pred.vals, pred.vals[ ,sampindcol] == 1))
  sampled_df$resids <- as.vector(sampled_df[ ,predvalcol] -
      sampled_df[ ,muhatcol])
    ## code for empirical variogram
  g_obj <- gstat::gstat(formula = resids ~ 1,
    locations = ~ `xcoordsUTM_` + `ycoordsUTM_`,
    data = sampled_df)
  vario_out <- gstat::variogram(g_obj)
  maxy <- max(vario_out$gamma)

  vartab <- cbind(vario_out$dist, vario_out$gamma,
    vario_out$np)
  colnames(vartab) <- c("Distance", "Gamma", "Number of Pairs")
  covparmmat <- t(matrix(covparmests))
  colnames(covparmmat) <- c("Nugget", "Partial Sill", "Range")
  ## code for fitted variogram

  maxdist <- max(vario_out$dist)
  x.dist.plot <- seq(0, maxdist, length.out = 300)
  nugget <- covparmests[1]

  if (CorModel == "Exponential") {
  v.modfit <- nugget + covparmests[2] -
    covparmests[2] * corModelExponential(x.dist.plot, covparmests[3])
  } else if (CorModel == "Gaussian") {
  v.modfit <- nugget + covparmests[2] -
      covparmests[2] * corModelGaussian(x.dist.plot, covparmests[3])
  } else if (CorModel == "Spherical") {
  v.modfit <- nugget + covparmests[2] -
    covparmests[2] * corModelSpherical(x.dist.plot, covparmests[3])
  }
  tab2 <- cbind(x.dist.plot, v.modfit)
  df.plot <- as.data.frame(tab2)

  plot_out <- ggplot(data = vario_out,
    aes_(x = ~dist, y = ~gamma)) +
    geom_point(aes_(size = ~gstat::variogram(g_obj)$np)) +
    ylim(0, maxy * (15 / 14)) +
    geom_line(data = df.plot, aes_(x = ~x.dist.plot, y = ~v.modfit)) +
    xlab("Distance (UTM)") +
    ylab("Semi-Variogram") +
    ggtitle(paste("Empirical Variogram with Fitted",
      CorModel, "Model")) +
    scale_size_continuous("Number of Pairs")

  print(plot_out)
} else if (get_variogram == FALSE) {
  plot_out <- NULL

  vartab <- NULL
}

if (get_krigmap == TRUE) {
}

## this needs to be fixed later

spacefun <- function() {
  ## need to make some decisions about how complex the grid/
  ## map should be
  alldata <- data.frame(pred.vals)
  shapevals <- c(16, 15)
  # (p1 <- ggplot(data = alldata, aes_(x = ~xcoords, y = ~ycoords,
  #   colour = ~preds, shape = ~as.factor(sampind))) +
  #   geom_point(size = 4) +
  #   scale_colour_gradient2(low = "blue", mid = "yellow", high = "red",
  #     midpoint = median(alldata$preds)) +
  #     scale_shape_manual(values = shapevals))

  ## make rectangles based on minimum distance
  ## this will only work if data form a grid

  minxdist <- min(dist(alldata$`xcoordsUTM_`)[dist(alldata$`xcoordsUTM_`) != 0])
  minydist <- min(dist(alldata$`ycoordsUTM_`)[dist(alldata$`ycoordsUTM_`) != 0])

  p3 <- ggplot2::ggplot(data = alldata, aes_(x = ~`xcoordsUTM_`, y = ~`ycoordsUTM_`,
    colour = ~counts_pred, shape = ~as.factor(counts_sampind))) +
      geom_rect(aes_(xmin = ~ (`xcoordsUTM_` - minxdist / 2),
        xmax = ~(`xcoordsUTM_` + minxdist / 2),
        ymin = ~(`ycoordsUTM_` - minydist / 2),
        ymax = ~(`ycoordsUTM_` + minydist / 2),
        fill = ~counts_pred)) +
      scale_fill_viridis_c() +
      scale_colour_viridis_c() +
      scale_shape_manual(values = shapevals)

  print(p3)


}
  if(get_report == TRUE) {

    file <- system.file("ReportTest.Rmd", package = "FPBKPack2")

    ## need to think more carefully about where this report
    ## should go.
    dout <- "~/Desktop/"
    if (missing(dout)) {
      dout <- getwd()
    }

    rmarkdown::render(file, envir = list(varplot = plot_out,
      varinfo = vartab,
      predtable = basicpred, conftable = confbounds,
      sumtable = outptmat, covparmests = covparmmat),
      output_dir = dout)
  }


}



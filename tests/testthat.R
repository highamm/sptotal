library(testthat)
library(sptotal)

test_that('m2LL.FPBK.nodet runs for exponential covariance model - REML.', {
  sptotal::m2LL.FPBK.nodet(theta = c(1, 1, 1), zcol=simdata$Z, XDesign=as.matrix(simdata[,c(3:9)]),
                           xcoord=simdata$x, ycoord=simdata$y, CorModel="Exponential",
                           estmethod="REML")
})

test_that('m2LL.FPBK.nodet runs for exponential covariance model - ML.', {
  sptotal::m2LL.FPBK.nodet(theta = c(1, 1, 1), zcol=simdata$Z, XDesign=as.matrix(simdata[,c(3:9)]),
                           xcoord=simdata$x, ycoord=simdata$y, CorModel="Exponential",
                           estmethod="ML")
})

test_that('m2LL.FPBK.nodet runs for spherical covariance model - REML.', {
  sptotal::m2LL.FPBK.nodet(theta = c(1, 1, 1), zcol=simdata$Z, XDesign=as.matrix(simdata[,c(3:9)]),
                           xcoord=simdata$x, ycoord=simdata$y, CorModel="Spherical",
                           estmethod="REML")
})

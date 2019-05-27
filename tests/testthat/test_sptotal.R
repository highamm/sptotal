library(testthat)
library(sptotal)
library(assertthat)

set.seed(12)

## REML ##
test_that('m2LL.FPBK.nodet runs for exponential covariance model - REML.', {
  res <- sptotal::m2LL.FPBK.nodet(theta = c(1, 1, 1), zcol=simdata$Z, XDesign=as.matrix(simdata[,c(3:9)]),
                           xcoord=simdata$x, ycoord=simdata$y, CorModel="Exponential",
                           estmethod="REML")
  res <- round(res, 3)
  expect_equal(res[1,1], 1484.494)
})

test_that('m2LL.FPBK.nodet runs for spherical covariance model - REML.', {
  res <- sptotal::m2LL.FPBK.nodet(theta = c(1, 1, 1), zcol=simdata$Z, XDesign=as.matrix(simdata[,c(3:9)]),
                           xcoord=simdata$x, ycoord=simdata$y, CorModel="Spherical",
                           estmethod="REML")
  res <- round(res, 3)
  expect_equal(res[1,1], 1519.875)
})

## ML ##

test_that('m2LL.FPBK.nodet runs for exponential covariance model - ML.', {
  res <- sptotal::m2LL.FPBK.nodet(theta = c(1, 1, 1), zcol=simdata$Z, XDesign=as.matrix(simdata[,c(3:9)]),
                           xcoord=simdata$x, ycoord=simdata$y, CorModel="Exponential",
                           estmethod="ML")
  res <- round(res, 3)
  expect_equal(res[1,1], 1462.294)
})

test_that('m2LL.FPBK.nodet runs for spherical covariance model - ML.', {
  res <- sptotal::m2LL.FPBK.nodet(theta = c(1, 1, 1), zcol=simdata$Z, XDesign=as.matrix(simdata[,c(3:9)]),
                           xcoord=simdata$x, ycoord=simdata$y, CorModel="Spherical",
                           estmethod="ML")
  res <- round(res, 3)
  expect_equal(res[1,1], 1498.421)
})

test_that('Basic slmfit runs.', {
  # FIXME - bug referenced in issue #5, for now I fit a slightly different model
  #fit <- sptotal::slmfit(Z ~ X1 + X2 + X3 + X4+ X5 + X6 + X7, data=simdata, xcoordcol = "x", ycoordcol="y")
  fit <- sptotal::slmfit(Z ~ X1 + X2 + X3 + X4+ X5 + X6 + X7 + F1, data=simdata, xcoordcol = "x", ycoordcol="y")

  # TODO add more here, but this should be adequate enough to check for a break
  known_coefs <- c(11.69682646, -0.04965179,  0.10825775,  0.19330586,  0.30602021,  0.41637405,  0.44552393,
                   0.08157834,  0.34237992,  0.71481384)
  expect_equal(fit$CoefficientEsts, known_coefs)
})

test_that('If user provides incorrect estmethod, an error is raised.', {
  expect_error(validate_estmethod('foo'))
})

test_that('If user provides incorrect CorMode, an error is raised.', {
  expect_error(validate_cormodel('foo'))
})

test_that('If the user provides None as the estmethod and an improper covestimates vetor, an error is raised.', {
  expect_error(validate_covariance_inputs(estmethod = "None", covestimates = c(NA, NA, NA)))
  expect_error(validate_covariance_inputs(estmethod = "None", covestimates = c(1, NA, NA)))
})

test_that('If column vectors are passed to validate_col_char, errors are raised.', {
  expect_error(validate_col_char(simdata$x, simdata$y))
})

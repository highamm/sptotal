context("test_fpbk") # Workaround for a bug in testthat 2.0
library(testthat)
library(sptotal)

test_that("Sample test data loads", {
  #load(system.file("data/exampledataset.rda", package="FPBKPack2"))
  expect_type(exampledataset, "list")
})

data(exampledataset) ## load a toy data set
slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
xcoordcol = 'xcoords', ycoordcol = 'ycoords', areacol = 'areavar')
predobj <- predict(slmobj)

slmobjnoarea <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
  xcoordcol = 'xcoords', ycoordcol = 'ycoords')
predobjnoarea <- predict(slmobjnoarea)

predobjwts <- predict(slmobj, wtscol = "dummyvar")

test_that("Fixed effects are estimated correctly", {
  expect_equal(slmobj$CoefficientEsts, c(26.1044332, 2.0554473, 0.2140165),
    tolerance = 1)
})

test_that("Areas are incorporated into the estimator", {
  expect_gte(predobj$FPBK_Prediction, predobjnoarea$FPBK_Prediction)
  expect_gte(predobj$PredVar, predobjnoarea$PredVar)
})

test_that("Spatial parameters are incorporated into prediction", {
  expect_equivalent(slmobj$SpatialParmEsts, predobj$SpatialParms)
})

test_that("Predictions in example data set don't change", {
  expect_equivalent(predobj$FPBK_Prediction, 813.1637, tolerance = 10)
  expect_equivalent(predobj$PredVar, 607.1478, tolerance = 10)
  expect_equivalent(predobjwts$FPBK_Prediction, 351.2619, tolerance = 10) ## for the weights
  expect_equivalent(predobjwts$PredVar, 26.29932, tolerance  = 10) ## for the weights
})


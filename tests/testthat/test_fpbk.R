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
test_that("Fixed effects are estimated correctly", {
  expect_equal(slmobj$CoefficientEsts, c(26.1044332, 2.0554473, 0.2140165))
})

test_that("Areas are incorporated into the estimator", {
  expect_gte(predobj$FPBK_Prediction, predobjnoarea$FPBK_Prediction)
  expect_gte(predobj$PredVar, predobjnoarea$PredVar)
})

test_that("Spatial parameters are incorporated into prediction", {
  expect_equivalent(slmobj$SpatialParmEsts, predobj$SpatialParms)
})

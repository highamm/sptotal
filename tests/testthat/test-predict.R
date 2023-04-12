slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
                 xcoordcol = 'xcoords', ycoordcol = 'ycoords',
                 areacol = 'areavar')

slmobj_noarea <- slmfit(formula = counts ~ pred1 + pred2,
                        data = exampledataset,
                        xcoordcol = 'xcoords', ycoordcol = 'ycoords')
predobj <- predict(slmobj)

test_that("Areas are incorporated into the estimator", {
  predobj_noarea <- predict(slmobj_noarea)
  expect_gte(predobj$FPBK_Prediction, predobj_noarea$FPBK_Prediction)
  expect_gte(predobj$PredVar, predobj_noarea$PredVar)
})

test_that("The correct parameters are incorporated into prediction", {
  expect_equal(slmobj$SpatialParmEsts, predobj$SpatialParms,
               ignore_attr = TRUE)
})

predobj_wts <- predict(slmobj, wtscol = "dummyvar")

test_that("Predictions in example data set don't change", {
  expect_equal(predobj$FPBK_Prediction, 813.1637, tolerance = .1,
               ignore_attr = TRUE)
  expect_equal(predobj$PredVar, 607.1478, tolerance = .1,
               ignore_attr = TRUE)
  expect_equal(predobj_wts$FPBK_Prediction, 351.2619, tolerance = .1,
               ignore_attr = TRUE) ## for the weights
  expect_equal(predobj_wts$PredVar, 26.29932, tolerance  = .1,
               ignore_attr = TRUE) ## for the weights
})



slmobj_ML <- slmfit(formula = counts ~ pred1 + pred2,
                    data = exampledataset,
                    xcoordcol = 'xcoords', ycoordcol = 'ycoords',
                    areacol = 'areavar',
                    estmethod = "ML")
predobj_ML <- predict(slmobj_ML)

test_that("Maximum Likelihood gives similar estimate to REML", {
  expect_equal(predobj$FPBK_Prediction, predobj_ML$FPBK_Prediction,
                    tolerance = 5)
})

slmobj_sph <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
                     xcoordcol = 'xcoords', ycoordcol = 'ycoords',
                     areacol = 'areavar',
                     CorModel = "Spherical")

slmobj_gau <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
                     xcoordcol = 'xcoords', ycoordcol = 'ycoords',
                     areacol = 'areavar',
                     CorModel = "Gaussian")
slmobj_none <- slmfit(formula = counts ~ pred1 + pred2,
                      data = exampledataset,
                      xcoordcol = 'xcoords', ycoordcol = 'ycoords',
                      areacol = 'areavar',
                      CorModel = "Exponential",
                      covestimates = c(0.1, 150, 1), estmethod = "None")


test_that("The other (non-exponential) covariance functions work", {
  predobj_sph <- predict(slmobj_sph)
  predobj_gau <- predict(slmobj_gau)
  predobj_none <- predict(slmobj_none)
  expect_equal(predobj$FPBK_Prediction, predobj_sph$FPBK_Prediction,
                    tolerance = 20)
  expect_equal(predobj$FPBK_Prediction, predobj_gau$FPBK_Prediction,
                    tolerance = 20)
  expect_equal(predobj$FPBK_Prediction, predobj_none$FPBK_Prediction,
                    tolerance = 20)
})

test_that("predict.slmfit objects are printed", {
  expect_error(print(predobj), NA)
})


test_that("deprecated predict function generates warning", {
  expect_warning(get.predinfo(predobj), NULL)
})






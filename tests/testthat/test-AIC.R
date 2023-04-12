slmobj_ml <- slmfit(formula = counts ~ pred1 + pred2,
                    data = exampledataset,
                    xcoordcol = 'xcoords', ycoordcol = 'ycoords',
                    areacol = 'areavar', estmethod = 'ML')

slmobj_reml <- slmfit(formula = counts ~ pred1 + pred2,
                      data = exampledataset,
                      xcoordcol = 'xcoords', ycoordcol = 'ycoords',
                      areacol = 'areavar')

test_that("AIC works for REML and ML", {
  expect_error(AIC(slmobj_reml), NA)
  expect_error(AIC(slmobj_ml), NA)
})

test_that("AIC does not change from current model", {
  expect_snapshot(AIC(slmobj_reml))
})

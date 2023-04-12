
slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
                 xcoordcol = 'xcoords', ycoordcol = 'ycoords')

test_that("Generalized R squared is generated", {
  expect_error(GR2(slmobj), NA)
})

slm_int <- slmfit(formula = counts ~ 1, data = exampledataset,
                  xcoordcol = 'xcoords', ycoordcol = 'ycoords')

test_that("Generalized R squared for intercept model is 0", {
  expect_equal(GR2(slm_int), 0)
})

test_that("Generalized R squared is not returned for a non slmfit object", {
  expect_match(GR2(predict(slm_int)), "Not a slmfit object")
})

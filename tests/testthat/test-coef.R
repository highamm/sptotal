slmobj <- slmfit(formula = counts ~ pred1 + pred2,
                 data = exampledataset,
                 xcoordcol = 'xcoords', ycoordcol = 'ycoords')

test_that("fixed effect coefficients are estimated", {
  expect_error(coef(slmobj), NA)
  expect_length(coef(slmobj), 3)
})

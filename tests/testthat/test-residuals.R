slmobj <- slmfit(formula = counts ~ pred1 + pred2,
                 data = exampledataset,
                 xcoordcol = 'xcoords', ycoordcol = 'ycoords')

residuals(slmobj, cross.validation = TRUE)

test_that("residuals are generated and are of the appropriate length", {
  expect_error(residuals(slmobj), NA)
  expect_length(residuals(slmobj), sum(!is.na(exampledataset$counts)))
})

test_that("normalized and cross validation residuals are computed", {
  expect_error(residuals(slmobj, type = "normalized"), NA)
  expect_error(residuals(slmobj, cross.validation = TRUE), NA)
  expect_error(residuals(slmobj, type = "normalized",
                         cross.validation = TRUE), NA)
})

slmobj <- slmfit(formula = counts ~ pred1 + pred2,
                 data = exampledataset,
                 xcoordcol = 'xcoords', ycoordcol = 'ycoords')

test_that("fitted values are returned", {
 expect_error(fitted(slmobj), NA)
})

test_that("fitted values for sampled sites only", {
  expect_length(fitted(slmobj), sum(!is.na(exampledataset$counts)))
})

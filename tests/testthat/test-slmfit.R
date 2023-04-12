slmobj <- slmfit(formula = counts ~ pred1 + pred2,
                 data = exampledataset, xcoordcol = 'xcoords',
                 ycoordcol = 'ycoords', areacol = 'areavar')

test_that("Fixed effects estimates do not change", {
  expect_equal(slmobj$CoefficientEsts, c(26.1044332, 2.0554473, 0.2140165),
               tolerance = .1)
})

test_that("Summary function yields no error", {
  expect_error(summary(slmobj), NA)
})

test_that("Summary output does not change", {
  expect_snapshot(summary(slmobj))
})



test_that("Other covariance functions fit", {
  slmobj_sph <- slmfit(formula = counts ~ pred1 + pred2,
                       data = exampledataset,
                       xcoordcol = 'xcoords', ycoordcol = 'ycoords',
                       areacol = 'areavar',
                       CorModel = "Spherical")

  slmobj_gau <- slmfit(formula = counts ~ pred1 + pred2,
                       data = exampledataset,
                       xcoordcol = 'xcoords', ycoordcol = 'ycoords',
                       areacol = 'areavar',
                       CorModel = "Gaussian")
  slmobj_none <- slmfit(formula = counts ~ pred1 + pred2,
                        data = exampledataset,
                        xcoordcol = 'xcoords', ycoordcol = 'ycoords',
                        areacol = 'areavar',
                        CorModel = "Exponential",
                        covestimates = c(0.1, 150, 1), estmethod = "None")
  expect_error(slmobj_none, NA)
  expect_error(slmobj_gau, NA)
  expect_error(slmobj_sph, NA)
})

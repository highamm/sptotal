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


exampledataset$pred_miss <- c(runif(30, 0, 1), rep(NA, 10))

test_that("observations with missing predictors generate a warning", {
  expect_warning(slmfit(formula = counts ~ pred_miss,
                        data = exampledataset,
                        xcoordcol = 'xcoords', ycoordcol = 'ycoords'),
                 NULL)
})

test_that("xcoordcol and ycoordcol arguments give names of columns", {
  expect_error(slmfit(formula = counts ~ pred1,
                      data = exampledataset,
                      xcoordcol = 'xcoords2', ycoordcol = 'ycoords'), NULL)

  expect_error(slmfit(formula = counts ~ pred1,
                      data = exampledataset,
                      xcoordcol = exampledataset$xcoords,
                      ycoordcol = 'ycoords'), NULL)

  expect_error(slmfit(formula = counts ~ pred1,
                      data = exampledataset,
                      xcoordcol = 'xcoords',
                      ycoordcol = exampledataset$ycoords), NULL)
})

test_that("covestimates must be length 3", {
  expect_error(slmfit(formula = counts ~ pred1,
                      data = exampledataset,
                      xcoordcol = 'xcoords',
                      ycoordcol = 'ycoords',
                      covestimates = c(0.1, 150), estmethod = "None"), NULL)

  expect_error(slmfit(formula = counts ~ pred1,
                      data = exampledataset,
                      xcoordcol = 'xcoords',
                      ycoordcol = 'ycoords', estmethod = "None"), NULL)

  expect_error(slmfit(formula = counts ~ pred1,
                      data = exampledataset,
                      xcoordcol = 'xcoords',
                      ycoordcol = 'ycoords',
                      covestimates = c(0.1, 150, NA),
                      estmethod = "None"), NULL)
})

test_that("non-standard values for estmethod and CorModel generate an error", {
  expect_error(slmfit(formula = counts ~ pred1,
                      data = exampledataset,
                      xcoordcol = 'xcoords',
                      ycoordcol = 'ycoords',
                      estmethod = "something_else"),
               NULL)

  expect_error(slmfit(formula = counts ~ pred1,
                      data = exampledataset,
                      xcoordcol = 'xcoords',
                      ycoordcol = 'ycoords',
                      CorModel = "something_else"),
               NULL)
})

simdata$many_levels <- factor(sample(letters, size = nrow(simdata),
                                     replace = TRUE))
test_that("factors with a large number of levels generate a warning", {
  expect_warning(slmfit(formula = Z ~ many_levels,
         data = simdata,
         xcoordcol = 'x',
         ycoordcol = 'y'),
         NULL)
})

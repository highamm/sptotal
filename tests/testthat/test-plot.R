
slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
                 xcoordcol = 'xcoords', ycoordcol = 'ycoords')

test_that("plotting model variogram works", {
  expect_error(plot(slmobj), NA)
})

test_that("plotting variogram works for spherical and gaussian covariances", {
  slmobj_gauss <- slmfit(formula = counts ~ pred1 + pred2,
                         data = exampledataset, xcoordcol = 'xcoords',
                         ycoordcol = 'ycoords', CorModel = "Gaussian")
  slmobj_spher <- slmfit(formula = counts ~ pred1 + pred2,
                         data = exampledataset, xcoordcol = 'xcoords',
                         ycoordcol = 'ycoords', CorModel = "Spherical")

  expect_error(plot(slmobj_gauss), NA)
  expect_error(plot(slmobj_spher), NA)
})


test_that("plotting predictions works", {
  pred_obj <- predict(slmobj)
  expect_error(plot(pred_obj), NA)
})




slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
                 xcoordcol = 'xcoords', ycoordcol = 'ycoords')

test_that("plotting model variogram works", {
  expect_error(plot(slmobj), NA)
})

slmobj_gauss <- slmfit(formula = counts ~ pred1 + pred2,
                       data = exampledataset, xcoordcol = 'xcoords',
                       ycoordcol = 'ycoords', CorModel = "Gaussian")
slmobj_spher <- slmfit(formula = counts ~ pred1 + pred2,
                       data = exampledataset, xcoordcol = 'xcoords',
                       ycoordcol = 'ycoords', CorModel = "Spherical")

test_that("plotting variogram works for spherical and gaussian covariances", {

  expect_error(plot(slmobj_gauss), NA)
  expect_error(plot(slmobj_spher), NA)
})

pred_obj <- predict(slmobj)

test_that("plotting predictions works", {
  expect_error(plot(pred_obj), NA)
})


## deprecated function checks
test_that("deprecated plotting functions generate warnings", {
  expect_warning(check.variogram(slmobj), NULL)
  expect_warning(check.variogram(slmobj_gauss), NULL)
  expect_warning(check.variogram(slmobj_spher), NULL)

  expect_warning(get.predplot(pred_obj), NULL)
})



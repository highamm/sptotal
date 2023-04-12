
slmobj <- slmfit(formula = counts ~ pred1 + pred2, data = exampledataset,
                 xcoordcol = 'xcoords', ycoordcol = 'ycoords')


test_that("log likelihood can be found", {
  expect_error(loglik.slmfit(slmobj), NA)
})

test_that("log likelihood does not change from snapshot", {
  expect_snapshot(loglik.slmfit(slmobj))
})

test_that("Log likelihood is not returned for a non slmfit object", {
  expect_match(loglik.slmfit(predict(slmobj)), "Not a slmfit object")
})





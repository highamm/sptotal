## stratification tests
exampledataset$stratavar <- "A"
exampledataset$stratavar[exampledataset$pred2 > -1.0] <- "B"
exampledataset$stratavar[exampledataset$pred2 > 0.1] <- "C"
exampledataset$stratavar <- as.factor(exampledataset$stratavar)


test_that("Stratification fits", {
  stratamod <- stratafit(counts ~ pred1, data = exampledataset,
                         xcoordcol = "xcoords", ycoordcol = "ycoords",
                         stratacol = "stratavar")
  expect_error(stratamod, NA)
})



test_that("including stratacol in slmfit analyzes strata separately", {
  stratamod <- stratafit(counts ~ pred1, data = exampledataset,
                         xcoordcol = "xcoords", ycoordcol = "ycoords",
                         stratacol = "stratavar")
  expect_error(stratamod, NA)
  expect_length(stratamod, 3)
  expect_error(print(stratamod), NA)
  expect_snapshot(summary(stratamod))
})

stratamod <- stratafit(counts ~ pred1, data = exampledataset,
                       xcoordcol = "xcoords", ycoordcol = "ycoords",
                       stratacol = "stratavar")

test_that("stratamod has length equal to the number of strata", {
  expect_equal(length(stratamod), nlevels(exampledataset$stratavar),
               tolerance = 0)
})

stratapred <- predict(stratamod)

test_that("stratification prediction of total does not change", {
  expect_equal(stratapred$summary_info[nrow(stratapred$summary_info), 1],
               780.75, tolerance = 0.1)
})

test_that("predict.stratafit objects are printed", {
  expect_error(print(stratapred), NA)
})

test_that("helper functions can be used on stratafit objects", {
  expect_equal(length(fitted(stratamod[[2]])), 15)
  expect_equal(AIC(stratamod[[3]]), 93.6, tolerance = 0.1)
})



test_that("including strata in stratacol and as fixed effect generates error", {
  expect_error(slmfit(counts ~ stratavar, data = exampledataset,
                         xcoordcol = "xcoords", ycoordcol = "ycoords",
                         stratacol = "stratavar"),
               NULL)
})

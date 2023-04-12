test_that("semi-variogram is generated", {
  expect_error(sv(na.omit(exampledataset),
                  "xcoords", "ycoords", "counts"),
               NA)
})

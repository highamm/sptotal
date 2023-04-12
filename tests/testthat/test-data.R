test_that("Package data sets load", {
  expect_type(exampledataset, "list")
  expect_type(AKmoose_df, "list")
  expect_type(simdata, "list")
  expect_type(USlakes, "list")
})

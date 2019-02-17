context("test_fpbk") # Workaround for a bug in testthat 2.0
library(testthat)
library(FPBKForestPack)

test_that("Sample test data loads", {
  #load(system.file("data/exampledataset.rda", package="FPBKPack2"))
  expect_type(exampledataset, "list")
})

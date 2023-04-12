locations <- expand.grid(1:10, 1:10)


test_that("exponential data is simulated", {
  sim_data <- geostatSim(locations, xcol = "Var1", ycol = "Var2",
                         parsil = 4, range = 20, nugget = 1,
                         CorModel = "Exponential")
  expect_error(sim_data, NA)
  expect_length(sim_data, ncol(locations) + 1)
})

test_that("anisotropic data can be simulated", {
  sim_aniso <- geostatSim(locations, xcol = "Var1", ycol = "Var2",
                         parsil = 1, range = 2, nugget = 3,
                         minorp = 0.5, rotate = 45,
                         CorModel = "Exponential")

  expect_error(sim_aniso, NA)
})

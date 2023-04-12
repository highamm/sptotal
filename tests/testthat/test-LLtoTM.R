test_that("longitude and latitude can be converted to TM", {
  tm_coords <- LLtoTM(cm = base::mean(AKmoose_df[ ,"lon"]),
                      lat = AKmoose_df[ ,"lat"],
                      lon = AKmoose_df[ ,"lon"])
  expect_equal(tm_coords$xy[ ,1], AKmoose_df$x)
  expect_equal(tm_coords$xy[ ,2], AKmoose_df$y)
})

test_that("longitude values are converted", {
  lon <- c(-95, 92)
  lat <- c(44, 45)
  small_tm <- LLtoTM(cm = base::mean(lon),
                      lat = lat,
                      lon = lon)
  expect_error(small_tm, NA)
})


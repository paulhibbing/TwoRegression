directions <-
  c("N", "NNE", "NE", "ENE",
    "E", "ESE", "SE", "SSE",
    "S", "SSW", "SW", "WSW",
    "W", "WNW", "NW", "NNW")

direction_vector <- sample(directions, 50, replace = TRUE)

test_that("direction changes work as expected", {

  testthat::expect_equal(
    TwoRegression:::get_directions(direction_vector),
    TwoRegression:::get_dcp5(direction_vector)
  )

})

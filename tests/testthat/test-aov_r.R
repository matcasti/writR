test_that("aov_r works", {
  test_data <- datasets::ChickWeight

  a <- aov_r(test_data, weight, Diet, Time, rowid = Chick)
  b <- aov_r(test_data, "weight", "Diet", "Time", rowid = "Chick")

  response <- "weight"; between <- "Diet"; within <- "Time"; rowid <- "Chick"
  c <- aov_r(test_data, response, between, within, rowid, character.only = TRUE)

  expect_equal(a, b)
  expect_equal(a, c)
  expect_equal(b, c)
})

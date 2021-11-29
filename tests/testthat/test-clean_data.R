test_that("clean_data works", {
  unquoted <- clean_data(data = ChickWeight,
                         x = Time, y = weight, rowid = Chick,
                         paired = TRUE, wide = TRUE)

  quoted <- clean_data(data = ChickWeight,
                       x = "Time", y = "weight", rowid = "Chick",
                       paired = TRUE, wide = TRUE, character.only = TRUE)

  expect_equal(unquoted, quoted)
})

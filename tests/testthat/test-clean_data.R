test_that("quoted and unquoted same results", {
  unquoted <- clean_data(data = ChickWeight,
                         x = Time, y = weight, rowid = Chick,
                         paired = TRUE, wide = TRUE)

  quoted <- clean_data(data = ChickWeight,
                       x = "Time", y = "weight", rowid = "Chick",
                       paired = TRUE, wide = TRUE, character.only = TRUE)

  expect_equal(unquoted, quoted)
})

test_that("quoted and unquoted same results within data.table .SD", {
  my_data <- data.table::as.data.table(ChickWeight)
  unquoted <- my_data[Diet == 1, clean_data(data = .SD,
                         x = Time, y = weight, rowid = Chick,
                         paired = TRUE, wide = TRUE)]

  quoted <- my_data[Diet == 1, clean_data(data = .SD,
                       x = "Time", y = "weight", rowid = "Chick",
                       paired = TRUE, wide = TRUE, character.only = TRUE)]

  expect_equal(unquoted, quoted)
})

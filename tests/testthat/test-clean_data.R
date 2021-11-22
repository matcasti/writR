test_that("check expected errors", {
  dataset <- datasets::ChickWeight

  # When `character.only` is set to TRUE

  expr <- quote(clean_data(dataset, Time, weight, character.only = TRUE))
  expect_error(eval(expr), regexp = "When `character.only` is set to TRUE, both `x` and `y` must be quoted variables of length one")

  expr <- quote(clean_data(dataset, "Time", weight, character.only = TRUE))
  expect_error(eval(expr), regexp = "When `character.only` is set to TRUE, both `x` and `y` must be quoted variables of length one")

  expr <- quote(clean_data(dataset, Time, "weight", character.only = TRUE))
  expect_error(eval(expr), regexp = "When `character.only` is set to TRUE, both `x` and `y` must be quoted variables of length one")

  expr <- quote(clean_data(dataset, c("Time", "Diet"), "weight", character.only = TRUE))
  expect_error(eval(expr), regexp = "When `character.only` is set to TRUE, both `x` and `y` must be quoted variables of length one")

  # When `character.only` is set to FALSE

  expr <- quote(clean_data(dataset, "Time", "weight", character.only = FALSE))
  expect_error(eval(expr), regexp = "When `character.only` is set to FALSE, both `x` and `y` must be unquoted variables of length one")

  expr <- quote(clean_data(dataset, "Time", weight, character.only = FALSE))
  expect_error(eval(expr), regexp = "When `character.only` is set to FALSE, both `x` and `y` must be unquoted variables of length one")

  expr <- quote(clean_data(dataset, Time, "weight", character.only = FALSE))
  expect_error(eval(expr), regexp = "When `character.only` is set to FALSE, both `x` and `y` must be unquoted variables of length one")

  expr <- quote(clean_data(dataset, c(Time, Diet), "weight", character.only = FALSE))
  expect_error(eval(expr), regexp = "When `character.only` is set to FALSE, both `x` and `y` must be unquoted variables of length one")

  # When `x`, `y` or data is missing

  expr <- quote(clean_data(dataset))
  expect_error(eval(expr), regexp = "`x` and `y` can't be null")

  expr <- quote(clean_data(dataset, x = Time))
  expect_error(eval(expr), regexp = "`x` and `y` can't be null")

  expr <- quote(clean_data(dataset, y = weight))
  expect_error(eval(expr), regexp = "`x` and `y` can't be null")

  expr <- quote(clean_data(x = Time, y = weight))
  expect_error(eval(expr), regexp = "`data` can't be null")
})

test_that("cent_disp works", {
  weight <- datasets::ChickWeight$weight
  is_normal(weight)
  expect_true(cent_disp(x = weight) == "*Mdn* = 103, *IQR* = 100.8")

  set.seed(123)
  weight <- rnorm(n = 578, mean = 103, sd = 101)
  is_normal(weight)
  expect_true(cent_disp(x = weight) == "*M* = 106.4, *SD* = 97")

  expect_true(cent_disp(weight, str.a = "{mean} ± {sd}") == "106.4 ± 97")
})

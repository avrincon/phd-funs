library("testthat")
library("avr")

test_that("Test sec_to_hms: 0 to 9 sec", {
  expect_identical(object = sec_to_hms(8), expected = "00:00:08")
})


test_that("Test sec_to_hms: invalid args are detected", {
  expect_error(sec_to_hms("a"))
  expect_error(sec_to_hms(iris))
})

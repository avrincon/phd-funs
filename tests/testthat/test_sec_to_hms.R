context("test_sec_to_hms.R")
library("testthat")
library("avr")

test_that("Test sec_to_hms: seconds are formatted correctly", {
  expect_identical(object = sec_to_hms(c(8, 66, 603-1, 601*5, 6000*6, 6e5)),
                   expected = c("00:00:08", "00:01:06", "00:10:02", "00:50:05",
                                "10:00:00", "166:40:00"))
})


test_that("sec_to_hms: invalid args are detected", {
  expect_error(sec_to_hms("a"))
  expect_error(sec_to_hms(-10))
  expect_error(sec_to_hms(iris))
  expect_error(sec_to_hms(c(1, NA)))
})



# to fix ---------------------------------------------------------------------

# test_dir("tests")
# test_file("test_sec_to_hms.R")
# test_file("C:/Users/avrin/ownCloud/Goettingen/PhD Thesis/Data/R custom packages/avr/tests/testthat/test_sec_to_hms.R")

library("testthat")
library("avr")

test_that("Test sec_to_hms: seconds are formatted correctly", {
  expect_identical(object = sec_to_hms(c(8, 66, 603-1, 601*5, 6000*6, 6e5)),
                   expected = c("00:00:08", "00:01:06", "00:10:02", "00:50:05",
                                "10:00:00", "166:40:00"))
})


test_that("Test sec_to_hms: invalid args are detected", {
  expect_error(sec_to_hms("a"))
  expect_error(sec_to_hms(iris))
})



# to fix ---------------------------------------------------------------------

# give warning?
# sec_to_hms(c(1,NA))

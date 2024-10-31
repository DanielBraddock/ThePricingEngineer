test_that("process_raw_crime_data works with default args", {
  expect_no_error(process_raw_crime_data())
})

test_that("process_raw_crime_data works with non-default args", {
  expect_no_error(process_raw_crime_data("20220923"))
})

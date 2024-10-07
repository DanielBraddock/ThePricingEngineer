test_that("scrape_road_safety_data does not error with default args", {
  expect_no_error(suppressWarnings(suppressMessages(scrape_road_safety_data())))
})

test_that("scrape_road_safety_data does not error with non-default year", {
  expect_no_error(suppressWarnings(suppressMessages(scrape_road_safety_data(2020))))
})

test_that("scrape_road_safety_data does not error with non-default vehicle arg", {
  expect_no_error(suppressWarnings(suppressMessages(scrape_road_safety_data(vehicle = TRUE))))
})

test_that("scrape_aadf does not error", {
  expect_no_error(suppressMessages(scrape_aadf()))
})

test_that("scrape_aadf year NULL returns data within last 2 years", {
  df <- suppressMessages(scrape_aadf(year = NULL))
  expected_year_max <- Sys.Date() |> format("%Y") |> as.numeric()
  expected_year_min <- expected_year_max - 2
  expected_year_options <- expected_year_min:expected_year_max
  actual_year <- df |> dplyr::pull("year") |> max(na.rm = TRUE)
  expect_in(actual_year, expected_year_options)
})

test_that("scrape_aadf year 2020 returns data with year = 2020", {
  df <- suppressMessages(scrape_aadf(year = 2020))
  expected_year <- 2020
  actual_year <- df |> dplyr::pull("year") |> max(na.rm = TRUE)
  expect_equal(actual_year, expected_year)
})

test_that("as_yyyymmdd_hhmmss works", {
  expect_no_error(as_yyyymmdd_hhmmss())
})

test_that("as_yyyymmdd_hhmmss gives expected result", {
  expect_equal(
    as.POSIXct("2024-08-28 16:37:35") |> as_yyyymmdd_hhmmss()
    , "20240828_163735"
  )
})

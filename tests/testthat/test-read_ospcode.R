test_that("read_ospcode works with default args", {
  expect_no_error(read_ospcode())
})

test_that("read_ospcode works with non-default args", {
  expect_no_error(read_ospcode("2206"))
})

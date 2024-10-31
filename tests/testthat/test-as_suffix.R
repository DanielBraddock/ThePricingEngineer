test_that("as_suffix works with NULL", {
  expect_equal(as_suffix(NULL), "")
})

test_that("as_suffix works with a quoted number (number as string)", {
  expect_equal(as_suffix("22"), "_22")
})

test_that("as_suffix works with an unquoted number (number as number)", {
  expect_equal(as_suffix(22), "_22")
})

test_that("as_suffix works with NULL and default not default", {
  expect_equal(as_suffix(NULL, "_Master"), "_Master")
})

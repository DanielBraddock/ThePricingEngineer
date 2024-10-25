test_that("read_sql works with defaults and correctly formatted name", {
  expect_no_error(read_sql("information_schema.tables"))
})

test_that("read_sql fails with defaults and incorrectly formatted name", {
  expect_error(read_sql("information_schema_tables"))
})

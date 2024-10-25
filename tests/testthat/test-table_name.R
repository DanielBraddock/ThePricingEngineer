test_that("table_name works", {
  expect_no_error(table_name(DBI::Id("information_schema", "tables")))
})

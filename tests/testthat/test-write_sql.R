test_that("test_write works with defaults", {
  df <- data.frame(x = 1:3)
  schema_table_name <- paste0("Test.test_", stringi::stri_rand_strings(1, 5))
  table_name <- schema_table_name |> stringr::str_split_i("\\.", 2)
  df |> write_sql(schema_table_name)
  table_search <- read_sql("information_schema.tables") |> 
    dplyr::filter(TABLE_SCHEMA == "Test", TABLE_NAME == table_name) |> 
    as.data.frame()
  DBI::dbRemoveTable(connect_nfu(), Id("Test", table_name))
  expect_true(nrow(table_search) > 0)
})

test_that("test_write works with time_stamp = TRUE", {
  df <- data.frame(x = 1:3)
  schema_table_name <- paste0("Test.test_", stringi::stri_rand_strings(1, 5))
  table_name <- schema_table_name |> stringr::str_split_i("\\.", 2)
  df |> write_sql(schema_table_name, time_stamp = TRUE)
  table_search <- read_sql("information_schema.tables") |> 
    dplyr::filter(TABLE_SCHEMA == "Test") |> 
    as.data.frame() |> 
    filter(TABLE_NAME |> stringr::str_detect(table_name))
  table_name_with_stamp <- table_search |> dplyr::pull(TABLE_NAME)
  DBI::dbRemoveTable(connect_nfu(), Id("Test", table_name_with_stamp))
  expect_true(nrow(table_search) > 0)
})

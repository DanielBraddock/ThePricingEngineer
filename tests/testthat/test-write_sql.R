test_that("test write_sql works with defaults", {
  conn <- connect_nfu()
  df <- data.frame(x = 1:3)
  schema_table_name <- paste0("Test.test_", stringi::stri_rand_strings(1, 5))
  table_name <- schema_table_name |> stringr::str_split_i("\\.", 2)
  if ("duckdb_connection" %in% class(conn)) {
    expect_no_error(df |> write_sql(schema_table_name, conn = conn))
    table_search <- 
      read_sql("information_schema.tables", conn = conn) |> 
      dplyr::filter(table_schema == "main", table_name == schema_table_name) |> 
      as.data.frame()
    DBI::dbRemoveTable(conn, Id("main", schema_table_name))
  } else {
    expect_no_error(df |> write_sql(schema_table_name))
    table_search <- read_sql("information_schema.tables") |> 
      dplyr::filter(TABLE_SCHEMA == "Test", TABLE_NAME == table_name) |> 
      as.data.frame()
    DBI::dbRemoveTable(connect_nfu(), Id("Test", table_name))
  }
  expect_true(nrow(table_search) > 0)
})

test_that("test write_sql works with time_stamp = TRUE", {
  conn <- connect_nfu()
  df <- data.frame(x = 1:3)
  schema_table_name <- paste0("Test.test_", stringi::stri_rand_strings(1, 5))
  table_name <- schema_table_name |> stringr::str_split_i("\\.", 2)
  if ("duckdb_connection" %in% class(conn)) {
    expect_no_error(df |> write_sql(schema_table_name, conn = conn, time_stamp = TRUE))
    table_search <- 
      read_sql("information_schema.tables", conn = conn) |> 
      dplyr::filter(table_schema == "main", table_name |> stringr::str_detect(schema_table_name)) |> 
      as.data.frame()
    schema_table_name_with_stamp <- table_search |> dplyr::pull(table_name)
    DBI::dbRemoveTable(conn, Id("main", schema_table_name_with_stamp))
  } else {
    expect_no_error(df |> write_sql(schema_table_name, time_stamp = TRUE))
    table_search <- read_sql("information_schema.tables") |> 
      dplyr::filter(TABLE_SCHEMA == "Test", TABLE_NAME |> stringr::str_detect(table_name)) |> 
      as.data.frame()
    table_name_with_stamp <- table_search |> dplyr::pull(TABLE_NAME)
    DBI::dbRemoveTable(connect_nfu(), Id("Test", table_name))
  }
  expect_true(nrow(table_search) > 0)
})

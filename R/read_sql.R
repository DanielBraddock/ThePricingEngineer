#' Read a table from SQL into R as a lazy tibble
#' 
#' @description
#' `read_sql()` gives NFU Pricing people an evocatively named way to read data 
#' into R from a SQL server. Let me demonstrate:
#' 
#' **Good:** `connect_nfu() |> tbl(I("information_schema.tables"))`
#' 
#' **Better:** `read_sql("information_schema.tables")`
#' 
#' Surely better than writing something like tbl(I("my_server.my_table")), 
#' read aloud like "tibble, eye", is writing something like "read sql". 
#' 
#' Better than [DBI::dbGetQuery()] *and* [DBI::dbReadTable()], 
#' it returns a lazy table instead of a data.frame. 
#' Lazy tables are great! 
#' See section 21.4 in https://r4ds.hadley.nz/databases.
#' 
#' Notice how, to access the default server, you simply give the table name. 
#' 
#' @details
#' It's great because:
#' 
#' * you don't need to remember which functions and packages you need
#' in order to read a table from SQL into R *eg* [dplyr::tbl()] and [base::I()]
#' * you don't need to pass the connection object! At least, not if you're happy
#' with the default server connected to by connect_nfu()
#'
#' @param name the name of the table to read from, always including the schema
#' *ie* "this_server.this_table"
#' @param conn a remote data source, *eg* server + database created from 
#' [DBI::dbConnect()]. Calls connect_nfu() by default. 
#' @param ... Other arguments passed on to [dplyr::tbl()]. 
#'
#' @return a lazy tibble, to be exact, of the class: tbl_Microsoft SQL Server,
#' tbl_dbi, tbl_sql, tbl_lazy, tbl
#' @export
#' 
#' @seealso [dplyr::tbl()], [base::I()], [dbplyr::in_schema()], 
#' [DBI::Id()]
#'
#' @examples 
#' # read a SQL table from the default server
#' df <- read_sql("information_schema.tables")
#' 
#' # alternatively
#' conn <- connect_nfu()
#' df <- read_sql("information_schema.tables", conn = conn)
#' 
#' \dontrun{
#' # read a SQL table from a different server
#' conn <- connect_nfu("a_different_server")
#' df <- conn |> read_sql("information_schema.tables")
#' }
read_sql <- function(name, conn = connect_nfu(), ...) {
  if (!"duckdb_connection" %in% class(conn)) name <- I(name)
  lazy_df <- conn |> dplyr::tbl(name, ... = ...)
  return(lazy_df)
}

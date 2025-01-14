#' Get prettified table name from conn (DBI::dbConnect) and Id (DBI::Id)
#' 
#' By default, gives the fully qualified name with server and database specified
#' Optionally, if full_name is FALSE, it will just give the schema and table
#'
#' @param Id the output of DBI::Id containing the schema and table name
#' @param conn the output of DBI::dbConnect giving the server and db
#' @param full_name Defaults to TRUE meaning server and db is also given
#'
#' @return a <chr> name of the table of the format server.db.schema.table
#'
#' @examples \dontrun{table_name(conn, Id)}
table_name <- function(Id, conn = connect_nfu(), full_name = TRUE) {
  table_name <- if (!"duckdb_connection" %in% class(conn) & full_name) {
    stringr::str_glue("{conn@info$sourcename}.{conn@info$dbname}.{Id@name[[1]]}.{Id@name[[2]]}")
  } else {
    stringr::str_glue("{Id@name[[1]]}.{Id@name[[2]]}")
  }
  return(table_name)
}
#' write_sql
#' 
#' Writes a data.frame to SQL.
#' It is essentially a wrapper around dplyr::copy_to with some messages.
#' The messages log the time the table starts being written, finishes, and how long it took.
#' 
#' @details
#' The name can include as many dots as you like.
#' On the work database it expects at least one dot.
#' In this case, everything before the first dot is taken as the schema.
#' And everything else is taken as the table.
#' Otherwise, the number of dots >= 0.
#' If there are zero dots, the name given is repeated as "the schema". 
#' *ie* "test1" will be saved as "test1.test1". 
#' Though this is pretty meaningless. 
#' It's just a little workaround. 
#' 
#' If timestamp TRUE, "schema.table" becomes something like 
#' "schema.table_yyyymmdd_hhmmss"
#'
#' @param df data.frame to write to SQL. 
#' @param name table to write in the format "my_schema.my_table" (if time_stamp = TRUE, suffix added, see details)
#' @param conn a remote data source, eg SQL server + database created from DBI::dbConnect. Calls connect_nfu() by default. 
#' @param time_stamp defaults to FALSE, creating a datetime stamp suffix for your table. 
#' @param overwrite defaults to TRUE, overwriting any existing table. 
#' @param temporary defaults to FALSE, ensuring the table persists after the sessions ends. 
#' @param ... args to pass to dplyr::copy_to
#'
#' @return NULL
#' @export
#'
#' @examples \dontrun{
#' df |> write_sql("this_schema", "this_table")
#' df |> write_sql("this_schema", "this_table", conn = conn)
#' }
write_sql <- function(df
                      , name
                      , conn = connect_nfu()
                      , time_stamp = FALSE
                      , overwrite = TRUE
                      , temporary = FALSE
                      , ...) {
  
  # for work dbConnections, make sure name has both schema and name part
  if (!"duckdb_connection" %in% class(conn)) {
    stopifnot(name |> stringr::str_detect("\\.")) # there is a dot
  }
  
  # break out schema and table parts of name into separate objects
  to_schema <- name |> stringr::str_split_i("\\.", 1)
  to_table <- name |> stringr::str_remove(paste0(to_schema, "."))
  
  # log
  t0 <- Sys.time()
  to_table_final <- if (time_stamp) paste0(to_table, "_", as_yyyymmdd_hhmmss(t0)) else to_table
  name_final <- stringr::str_glue("{to_schema}.{to_table_final}")
  if (!"duckdb_connection" %in% class(conn)) name_final <- I(name_final)
  id_final <- DBI::Id(to_schema, to_table_final)
  message(paste0("Table saving to ", table_name(conn = conn, Id = id_final)))
  message(paste0(as_yyyymmdd_hhmmss(t0), ": start writing table"))
  
  # write to SQL
  df |> 
    dplyr::copy_to(
      dest = conn
      , df = _
      , name = name_final
      , overwrite = overwrite
      , temporary = temporary
      , ...
    )
  
  # log
  t1 <- Sys.time()
  message(paste0(as_yyyymmdd_hhmmss(t1), ": end writing table"))
  d_write <- (t1 - t0) |> as.numeric(units = "mins") |> round(2)
  message(paste0(d_write, " minutes taken to write table\n"))
  message(paste0("Table saved to ", table_name(conn = conn, Id = id_final)))
  message("================================================================")
  
  return(NULL)
}

#' write_sql
#' 
#' This function is called for it's side-effects of writing a local data.frame to SQL.
#' It is essentially a wrapper around dplyr::copy_to with some messages.
#' The messages log the time the table starts being written, finishes, and how long it took.
#'
#' @param df the local data.frame to write to SQL. 
#' @param to_schema the name of the schema to write to eg "this_schema". 
#' @param to_table the name of the table to write to eg "this_table" NB a time-stamp is added for you as a suffix if time_stamp = TRUE. 
#' @param to_conn a remote data source, eg SQL server + database created from DBI::dbConnect. Calls connect_nfu() by default. 
#' @param time_stamp defaults to FALSE, creating a datetime stamp suffix for your table. 
#' @param overwrite defaults to TRUE, overwriting any existing table. 
#' @param temporary defaults to FALSE, ensuring the table persists after the sessions ends. 
#' @param ... args to pass to copy_to. 
#'
#' @return NULL
#' @export
#'
#' @examples \dontrun{write_sql(df, conn, "this_schema", "this_table")}
write_sql <- function(df, to_schema, to_table, to_conn = connect_nfu(), time_stamp = FALSE, overwrite = TRUE, temporary = FALSE, ...) {
  
  # to pass to messages and use in copy_to
  to_table_final <- if (time_stamp) paste0(to_table, "_", as_yyyymmdd_hhmmss(t0)) else to_table
  id_table <- DBI::Id(to_schema, to_table_final)

  # log
  message(paste0("Table saving to ", table_name(conn = to_conn, Id = id_table)))
  t0 <- Sys.time()
  message(paste0(as_yyyymmdd_hhmmss(t0), ": start writing table"))
  
  # write to SQL
  df |> 
    dplyr::copy_to(
      dest = to_conn
      , df = _
      , name = id_table
      , overwrite = overwrite
      , temporary = temporary
      , ...
    )
  
  # log
  t1 <- Sys.time()
  message(paste0(as_yyyymmdd_hhmmss(t1), ": end writing table"))
  d_write <- (t1 - t0) |> as.numeric(units = "mins") |> round(2)
  message(paste0(d_write, " minutes taken to write table\n"))
  message(paste0("Table saved to ", table_name(conn = to_conn, Id = id_table)))
  message("================================================================")
  
  return(NULL)
}

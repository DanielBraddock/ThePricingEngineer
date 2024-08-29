#' write_sql
#' 
#' This function is called for it's side-effects of writing a local data.frame to SQL
#'
#' @param df the local data.frame to write to SQL
#' @param to_conn a remote data source, eg SQL server + database created from DBI::dbConnect
#' @param to_schema the name of the schema to write to eg "this_schema"
#' @param to_table the name of the table to write to (needs no datetime stamp) eg "this_table"
#' @param time_stamp defaults to TRUE, creating a datetime stamp suffix for you table
#' @param overwrite defaults to TRUE, overwriting any existing table
#' @param temporary defaults to FALSE, ensuring the table persists after the sessions ends
#'
#' @return NULL
#' @export
#'
#' @examples \dontrun{write_sql(df, conn, "this_schema", "this_table")}
write_sql <- function(df, to_conn, to_schema, to_table, time_stamp = TRUE, overwrite = TRUE, temporary = FALSE) {

 # log
  t0 <- Sys.time()
  message("================================================================")
  message(paste0(as_yyyymmdd_hhmmss(t0), ": start writing table"))
  to_table <- if (time_stamp) paste0(to_table, "_", as_yyyymmdd_hhmmss(t0)) else to_table
  
  # write to SQL
  id_table <- DBI::Id(to_schema, to_table)
  df |> 
    dplyr::copy_to(
      dest = to_conn
      , df = _
      , name = id_table
      , overwrite = overwrite
      , temporary = temporary
    )
  
  # log
  t1 <- Sys.time()
  message(paste0(as_yyyymmdd_hhmmss(t1), ": end writing table"))
  d_write <- (t1 - t0) |> as.numeric(units = "mins") |> round(2)
  message(paste0(d_write, " minutes taken to write table\n"))
  message(paste0("Table saved to ", table_name(to_conn, id_table)))
  
  return(NULL)
}

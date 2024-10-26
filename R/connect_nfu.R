#' Connect to database via ODBC
#' 
#' A convenience wrapper around DBI::dbConnect(). 
#' Usefully for Pricing, it defaults to the UWPSQL06 server
#' 
#' @details
#' NB ODBC Data Sources must be set up correctly. 
#' Basically, add a User DSN for each server. 
#' I used the driver SQL Server Native Client 11.0. 
#' Name them after the server for the defaults to work nicely. 
#' You can use the same name for the Description. 
#' For example, add a DSN called "UWPSQL06" for the server "UWPSQL06". 
#'
#' @param dsn The Data Source Name. A character scalar. 
#' @param server The server name. A character scalar. 
#' @param database The database name. A character scalar. 
#' @param ... Arguments passed on to [DBI::dbConnect()]
#'
#' @return The output of dbConnect: I expect the class "Microsoft SQL Server"
#' @export
#'
#' @examples
#' conn <- connect_nfu()
#' conn_6 <- connect_nfu("UWPSQL06")
#' conn_4 <- connect_nfu("UWPSQL04")
#' conn_2 <- connect_nfu("UWPSQL02")
connect_nfu <- function(dsn = "UWPSQL06"
                        , server = dsn
                        , database = "Sandbox"
                        , ...) {
  conn <- tryCatch(
    expr = DBI::dbConnect(
      drv = odbc::odbc()
      , dsn = dsn
      , server = server
      , database = database
      , ... = ...
    )
    , error = function(e) duckdb::dbConnect(duckdb::duckdb())
  )
  
  return(conn)
}

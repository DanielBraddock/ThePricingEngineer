#' System Date and Time in YYYYMMDD_HHMMSS format
#' 
#' @param x the date-time to be formatted as YYYYMMDD_HHMMSS, and defaulting to Sys.time() the function is called
#'
#' @return a <chr> value of the current date-time in YYYYMMDD_HHMMSS format
#' @export
#'
#' @examples 
#' as_yyyymmdd_hhmmss()
#' t0 <- Sys.time()
#' as_yyyymmdd_hhmmss(t0)
as_yyyymmdd_hhmmss <- function(x = Sys.time()) {
  date_time <- format(x, "%Y%m%d_%H%M%S")
  return(date_time)
}
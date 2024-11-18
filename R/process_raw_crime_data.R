#' Process raw crime data and left join onto postcode data (OSPCode)
#'
#' @param version_crime *eg* "20220923". 
#' Default (NULL) means we pull from the table with no suffix
#' @param conn Defaults to connect_nfu(), *ie* the default server. 
#'
#' @return lazy tibble
#' @export
#'
#' @examples
#' \dontrun{
#' process_raw_crime_data() |> head()
#' process_raw_crime_data("20220923") |> head()
#' }
process_raw_crime_data <- function(version_crime = NULL, conn = connect_nfu()) {
  
  version_crime_suffix <- as_suffix(version_crime)
  
  raw_crime_data <- read_sql(paste0("Postcodes_Arc207912.Postcode_ExternalData_raw_Crime", version_crime_suffix))
  
  crime_data <- raw_crime_data
  crime_data <- crime_data |> dplyr::filter(!is.na(.data[["longitude"]]) & !is.na(.data[["latitude"]]))
  crime_data <- crime_data |> dplyr::summarise(crime_count = dplyr::n(), .by = c("longitude", "latitude", "crime_type"))
  
  return(crime_data)
}
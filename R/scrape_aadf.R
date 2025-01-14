
#' Scrape the latest AADF data from the government website
#' 
#' Uses rvest to scrape the data location and httr2 to perform a request. 
#' This saves a .zip file containing the dataset in a .csv file to a temp dir. 
#' This data is then read in and returned. 
#' 
#' @param year The year of data to return. By default, NULL, the most recent accident year available. 
#' @param raw_counts By default, FALSE, which means it scrapes "dft_traffic_counts_aadf". 
#' If set to TRUE, then it scrapes "dft_traffic_counts_raw_counts" instead. 
#'
#' @return A tibble containing the aadf (or raw_counts) data
#' @export
#'
#' @examples
#' \dontrun{
#' aadf <- scrape_aadf()
#' aadf <- scrape_aadf(raw_counts = TRUE)
#' }
scrape_aadf <- function(year = NULL, raw_counts = FALSE) {
  
  stopifnot(is.null(year) | is.numeric(year))
  
  # avoid name clashes
  latest_year = year
  
  # there are two datasets we might want: aadf and raw_counts
  aadf_or_raw_counts <- if (raw_counts) "raw_counts" else "aadf"
  
  # define temporary paths for the files
  destfile <- tempfile(fileext = ".zip")
  destdir <- destfile |> 
    stringr::str_split_1("\\\\") |> 
    head(-1) |> 
    stringr::str_flatten("\\")
  
  # define the target filename to be extracted from the zip file
  filename <- paste0("dft_traffic_counts_", aadf_or_raw_counts, ".csv")
  
  # download zip file to temporary location
  "https://roadtraffic.dft.gov.uk/downloads" |> 
    rvest::read_html() |> 
    rvest::html_nodes("a") |> 
    rvest::html_attr("href") |> 
    stringr::str_subset(paste0(aadf_or_raw_counts, "\\.zip$")) |> 
    httr2::request() |> 
    httr2::req_perform(path = destfile)
  
  # extract csv to temporary location
  unzip(destfile, files = filename, exdir = destdir)
  
  # read in the csv
  df <- readr::read_csv(file.path(destdir, filename))
  
  # filter to year (by default, year = NULL gives the most recent year)
  df <- if (is.null(latest_year)) {
    df |> dplyr::filter(.data[["year"]] == max(.data[["year"]], na.rm = TRUE))
  } else {
    df |> dplyr::filter(.data[["year"]] == latest_year)
  }
  
  return(df)
  
}

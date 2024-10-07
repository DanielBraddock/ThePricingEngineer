
#' Scrape the latest AADF data from the government website
#' 
#' Uses rvest to scrape the data location and httr2 to perform a request. 
#' This saves a .zip file containing the dataset in a .csv file to a temp dir. 
#' This data is then read in and returned. 
#' 
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
scrape_aadf <- function(raw_counts = FALSE) {
  
  # there are two datasets we might want: aadf and raw_counts
  aadf_or_raw_counts <- if (raw_counts) "raw_counts" else "aadf"
  
  # define temporary paths for the files
  destfile <- tempfile(fileext = ".zip")
  destdir <- destfile |> 
    stringr::str_split_1("\\\\") |> 
    purrr::discard(\(x) x |> stringr::str_detect("\\.zip")) |> 
    stringr::str_c(collapse = "\\")
  
  # define the target filename to be extracted from the zip file
  filename <- paste0("dft_traffic_counts_", aadf_or_raw_counts, ".csv")
  
  # download zip file to temporary location
  # maybe it should be https://roadtraffic.dft.gov.uk/downloads?
  "https://www.data.gov.uk/dataset/208c0e7b-353f-4e2d-8b7a-1a7118467acc/gb-road-traffic-counts" |> 
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
  
  return(df)
  
}

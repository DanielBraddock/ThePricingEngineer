
#' Scrape the latest Road Safety Data from the government website
#' 
#' Also associated with (not sure exactly what the relationship is) the term "Stats19". 
#' This is data on traffic collisions. 
#'
#' @param year The year of data to return. By default, NULL, the most recent year available. 
#' @param vehicle If TRUE, then this scrapes "dft-road-casualty-statistics-vehicle" data 
#' instead of "dft-road-casualty-statistics-collision" data. By default it is FALSE. 
#'
#' @return A tibble containing the collisions (or vehicle) data
#' @export
#'
#' @examples
#' \dontrun{
#' accidents <- scrape_road_safety_data()
#' vehicles <- scrape_road_safety_data(vehicle = TRUE)
#' accidents_2020 <- scrape_road_safety_data(2020)
#' vehicles_2020 <- scrape_road_safety_data(2020)
#' }
scrape_road_safety_data <- function(year = NULL, vehicle = FALSE) {
  
  # handle which dataset will be downloaded
  collision_or_vehicle <- if (vehicle) "vehicle" else "collision"
  file_prefix <- stringr::str_glue("dft-road-casualty-statistics-{collision_or_vehicle}")
  
  # list the available paths for the appropriate dataset (vehicle or collision)
  available_paths <- 
    "https://www.data.gov.uk/dataset/cb7ae6f0-4be6-4935-9277-47e5ce24a11f/road-safety-data" |> 
    rvest::read_html() |> 
    rvest::html_nodes("a") |> 
    rvest::html_attr("href") |> 
    stringr::str_subset(paste0(file_prefix, "-20[0-9]{2}\\.csv"))
  
  # select the appropriate file by year
  path <- if (is.null(year)) {
    available_paths |> sort() |> tail(1)
  } else {
    available_paths |> stringr::str_subset(as.character(year))
  }
  
  df <- readr::read_csv(path)
  
  # because SQL doesn't want cols of type <time>
  if (!vehicle) df <- df |> dplyr::mutate(time = time |> as.character())
  
  return(df)
  
}

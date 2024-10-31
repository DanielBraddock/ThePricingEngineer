
#' Convert foreign object to an sf object with British National Grid, safely
#'
#' @param df data.frame
#' @param x variable name to use as x coord
#' @param y variable name to use as y coord
#'
#' @return An sf object
#' @export
#'
#' @examples
#' \dontrun{
#' df |> st_as_sf("Eastings", "Northings")
#' }
st_as_sf_27700_safe <- function(df, x, y) {
  df_sf <- df
  df_sf <- df_sf |> filter(!is.na(.data[[x]]))
  df_sf <- df_sf |> filter(!is.na(.data[[y]]))
  df_sf <- df_sf |> filter(.data[[x]] != "NULL")
  df_sf <- df_sf |> filter(.data[[y]] != "NULL")
  df_sf <- df_sf |> st_as_sf(coords = c(x, y), crs = 27700)
  return(df_sf)
}
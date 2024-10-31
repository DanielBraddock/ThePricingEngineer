
#' Convert a string into a suffix
#' 
#' Generally, this simply means add a "_" to the beginning of your string. 
#' But it's main purpose is to use inside other functions, 
#' specifically taking function args as input, which might be NULL (default)
#' and as such we want to return a dummy suffix of "".
#' This means that instead of building a function called like:
#' 
#' * pull_data(version = "_22")
#' 
#' I can build a function that's called like:
#' 
#' * pull_data(version = "22")
#' 
#' Because inside pull_data I'm doing:
#' 
#' version <- version |> as_suffix()
#' 
#' Which is appending "_" at the beginning, or, if NULL, not doing anything. 
#'
#' @param x string to format as suffix
#' @param default what to return when x is null. Defaults to ""
#'
#' @return A string
#'
#' @examples
#' as_suffix(NULL)
#' as_suffix("22")
#' as_suffix(22)
#' as_suffix(NULL, "_Master")
as_suffix <- function(x, default = "") {
  stopifnot(is.null(x) | is.character(x) | is.numeric(x))
  if (is.numeric(x)) x <- as.character(x)
  x_as_suffix <- if (is.null(x)) default else paste0("_", as.character(x))
  return(x_as_suffix)
}

#' Read OSPCode
#' 
#' Specify the table version you want. By default, reads OSPCode_Master.
#'
#' @param version_postcodes *eg* "2206". 
#' Default (NULL) creates a suffix of "_Master"
#' @param conn Defaults to connect_nfu(), *ie* the default server. 
#'
#' @return lazy tibble
#' @export
#'
#' @examples
#' read_ospcode() # reads OSPCode_Master
#' read_ospcode("2206") # reads OSPCode_2206
read_ospcode <- function(version_postcodes = NULL, conn = connect_nfu()) {
  
  version_postcodes_suffix <- as_suffix(version_postcodes, default = "_Master")
  
  ospcode <- read_sql(paste0("Postcodes_Arc207912.OSPCode", version_postcodes_suffix))
  
  return(ospcode)
}
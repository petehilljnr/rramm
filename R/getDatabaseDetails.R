#' getDatabaseDetails Function
#' 
#' This returns the details of the database you are connected to via the headers (which contains the token)
#' The returned object contains information regarding the map projection you may need to convert to/from
#' 
#' @param headers A headers object created using the getHeaders function
#' @export
#' @examples 
#' dbDetails <- getDatabaseDetails(headers=getHeaders(...))


getDatabaseDetails = function(headers) {
  #  Function to return useful database information, especially the EPSG code ($mapProjectDetail)
  #  that should be used when doing geometry attribute functions
  
  db_details_req  = httr::GET(
    'https://apps.ramm.co.nz:443/RammApi6.1/v1/database',
    headers
  )
  
  db_details = jsonlite::fromJSON(content(db_details_req,'text'))
  
  return(db_details)
}
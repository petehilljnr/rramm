#' getSchema Function
#'
#' Retrieves column information for the specified RAMM table
#'
#' @param headers Authorisation headers created using getHeaders()
#' @param table_name RAMM table you want column information for.
#' @param loadType What type of column information is required - 'CoreAndLocation' (i.e. core and primary_key),'Specified' or 'All'
#' @param named_columns Vector of columns that information is requierd for.

#' @export
#' @examples
#'
#'
#' hdrs <- getAuthorisedHeaders(userName='api_demo', password='thursday', database='RAMM API Demo')
#' #Core
#' getSchema(hdrs,'carr_way','Core')
#' #All
#' getSchema(hdrs,'carr_way','All')
#' #Specified
#' getSchema(hdrs,'carr_way','Specified',columns=list('road_id','carr_way_no'))
#'

getSchema = function(headers,table_name,load_type='CoreAndLocation',named_columns=''){
  #RAMM load types 0:4 seem a little funky - specify our own
  load_type_lookup = c(
    'CoreAndLocation' = 4,#Core *and* primary key - RAMM core
    'Specified' = 2,
    'All' = 3
  )

  data_req = httr::GET(
    httr::modify_url(
      url = paste0('https://apps.ramm.co.nz:443/RammApi6.1/v1/schema/',table_name),
      query=list(
        loadType=load_type_lookup[load_type],
        columns=paste(named_columns,collapse=','))
    )
   ,
    headers
  )
  ct = httr::content(data_req,'text')
  result = jsonlite::fromJSON(ct)
  #map dataclasses to R types
  class_lookup = c(
    '0'='logical',
    '1'='dunno',
    '2'='Date',
    '3'='dunno again',
    '4'='integer',
    '5'='numeric',
    '6'='character'
    )

  result$R_data_type = class_lookup[as.character(result$dataClass)]
  return(result)
}



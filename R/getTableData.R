#' getTableData Function
#' 
#' Retrieves data from the specified RAMM table from the authorised database (as per the token in the headers).
#' You need to get the headers using the getHeaders() function
#' 
#' @param headers A headers object created using the getHeaders function
#' @param table_name The name of the table you want to extract data from (must be a valid table name)
#' @param named_columns A vector of all of the columns you want to return (must be valid column names)
#' @param get_geometry Return a geometry(TRUE) or not (FALSE).  Will append the field "wkt_geom" to your returned data
#' @param expand_lookups Automatically return the inbuilt lookup names instead of the parent value e.g. road_id returns road name
#' @param filters A list of filters created using the createFilter function
#' @export
#' @examples 
#' #The following example returns the raw road_id, sign_type, and wkt_geom fields from the sign table of the database authorised through the headers
#' 
#' sign_table <- getTableData(headers=headers, table_name="sign",named_columns=c("road_id","sign_type"),get_geometry=TRUE, expand_lookups=FALSE)

getTableData = function(headers,table_name,named_columns,get_geometry=FALSE,expand_lookups=FALSE, filters=list()){
  #  Function to query any RAMM table
  #  named_columns is an array e.g. c('road_id','carrway_start_m')
  
  query_params = jsonlite::toJSON(list(getGeometry=get_geometry
                             ,tableName=table_name
                             ,loadType='Specified'
                             ,expandLookups=expand_lookups
                             ,columns=named_columns
                             ,filters=filters)
                        ,auto_unbox = TRUE
  )
  
  data_req  = httr::POST(
    'https://apps.ramm.co.nz:443/RammApi6.1/v1/data/table' ,
    headers,
    body = query_params
  )
  
  ct = httr::content(data_req,'text')
  result = jsonlite::fromJSON(ct)$rows$values
  
  
  if(get_geometry==TRUE) {
    named_columns = c(named_columns, 'wkt_geom')
  }
  
  output = data.frame(matrix(unlist(result),ncol=length(named_columns),byrow=TRUE),stringsAsFactors = FALSE)
  colnames(output) = named_columns
  
  return(output)
  
}
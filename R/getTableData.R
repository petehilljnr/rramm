#' getTableData Function
#'
#' Retrieves data from the specified RAMM table from the authorised database (as per the token in the headers).
#' You need to get the headers using the getHeaders() function
#'
#' @param headers Authorised access headers created using  getHeaders(...).
#' @param table_name The name of the table you want to extract data from (must be a valid table name)
#' @param load_type Which columns of data will be returned - can be
#' 'All', 'CoreAndLocation' (i.e. core columns with primary key) or 'Specified'.
#' @param named_columns If loadType = 'Specified' a character vector of required columns (must be valid column names).
#' @param get_geometry (Boolean) Return geometry with the data. Returns an SF class data.frame in NZTM coordiantes (i.e. EPSG/SRID 2193).
#' @param filters A list of filters created using the createFilter function.
#' @export
#' @examples
#' \dontrun{
#' #The following example returns the raw road_id, sign_type as an SF data.frame with spatial coordinates
#'
#' sign_table <- getTableData(
#' headers=hdrs,
#' table_name="sign",
#' load_type='specified',
#' named_columns=c("road_id","sign_type"),
#' get_geometry=TRUE
#' )
#'
#' }

getTableData = function(
  headers,
  table_name,
  load_type='CoreAndLocation',
  named_columns='',
  get_geometry=FALSE,
  filters=list()
){
  # RAMM POST request does not returning named JSON objects,
  # So have to first get column data type information etc via a getSchema call

  column_info = getSchema(headers,table_name,load_type,named_columns)
  column_types = column_info$R_data_types

  load_type_lookup = c(
    'CoreAndLocation' = 4,#Core and primary key
    'Specified' = 2,
    'All' = 3
  )

  query_params = jsonlite::toJSON(
    list(getGeometry=get_geometry
        ,tableName=table_name
        ,loadType=load_type_lookup[load_type]
        ,getSchema=FALSE
        ,getMetaData=FALSE
        ,expandLookups=FALSE
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
  rows = jsonlite::fromJSON(ct)$rows


  if(get_geometry==TRUE) {
    named_columns = c(named_columns, 'wkt_geom')
    column_types = c(column_types,'character')
  }

  result_df = read.csv(
    text=sapply(rows$values,paste,collapse=','),
    colClasses = column_types,
    header=FALSE
  )

  colnames(result_df) = named_columns

  #retrive the correct projection details
  #and convert to SF object and
  if(get_geometry==TRUE) {
    srid = as.numeric(getDatabaseDetails(headers)$mapProjectDetail)
    print(srid)
    result_df = result_df %>%
      sf::st_as_sf(wkt='wkt_geom') %>%
      sf::st_set_crs(srid) %>%
      sf::st_transform(2193) #always return NZTM coordinates
  }

  return(result_df)

}

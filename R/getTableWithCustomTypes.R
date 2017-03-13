#' getTableWithCustomTypes Function
#'
#' Retrieve RAMM table and force column types - the schema type listing from the RAMM API is not always correct!
#' You need to get the headers using the getHeaders() function
#'
#' @param headers Authorised access headers created using  getHeaders(...).
#' @param table_name The name of the table you want to extract data from (must be a valid table name)
#' @param load_type Which columns of data will be returned - can be
#' 'All', 'CoreAndLocation' (i.e. core columns with primary key) or 'Specified'.
#' @param named_columns If loadType = 'Specified' a character vector of required columns (must be valid column names).
#' @param get_geometry (Boolean) Return geometry with the data. Returns an SF class data.frame in NZTM coordiantes (i.e. EPSG/SRID 2193).
#' @param filters A \strong{LIST} of filters created using createFilter(...).
#' @param custom_types_df A dataframe of column names and the types that will be forced. needs to be named column_name and column_type
#' @export
#'

getTableWithCustomTypes = function(
  headers,
  table_name,
  custom_types_df,
  load_type='CoreAndLocation',
  named_columns='',
  get_geometry=FALSE,
  filters=list()
){
  # RAMM POST request does not returning named JSON objects,
  # So have to first get column data type information etc via a getSchema call

  column_info = getSchema(headers,table_name,load_type,named_columns) %>%
    left_join(custom_types_df,
              by = c('columnName'='column_name')
              )

  column_names = column_info$columnName
  column_types = column_info$column_type

  if(get_geometry==TRUE) {
    column_names = c(column_names, 'wkt_geom')
    column_types = c(column_types,'character')
  }

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

  if (data_req$status_code != 200){
    stop(
      paste('Status code',data_req$status_code,ct)
    )

  } else { #all good - process the response

    #TODO: need to catch when zero rows are returned.
    rows = jsonlite::fromJSON(ct)$rows

    if (length(rows) == 0){ #filter returns no records
      warning('no records returned')
      #return an empty data.frame
      result_df <- read.table(text = "",
                              colClasses = column_types,
                              col.names =  column_names
      )

    } else {

      collapsed_vals = sapply(rows$values,paste,collapse='|')

      result_df = read.delim(
        text= collapsed_vals,
        sep='|',
        colClasses = column_types,
        header=FALSE
      )

      colnames(result_df) = column_names
    }
    #retrive the correct projection details
    #and convert to SF object and
    if(get_geometry==TRUE) {
      srid = as.numeric(getDatabaseDetails(headers)$mapProjectDetail)

      result_df = result_df %>%
        sf::st_as_sf(wkt='wkt_geom') %>%
        sf::st_set_crs(srid) %>%
        sf::st_transform(2193) #always return NZTM coordinates
    }

    return(result_df)

  }

}

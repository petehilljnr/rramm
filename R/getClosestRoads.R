#' getClosestRoadsFunction
#'
#' Retrieves a dataframe of the closest road ids and distances to a given WKT
#' Note if the WKT is a Polygon, it will give all roads that touch that WKT and will return a distance of 0.
#' The distance parameter is ignore if the WKT is a Polygon
#' Have not tested with a linestring as yet.
#' WKT must be in EPSG 4326.  Use the convertWKT function if required
#'
#' @param headers A headers object created using the getHeaders function
#' @param wkt A valid WKT string in EPSG:4326
#' @param search_distance Distance in metres for search radius e.g. 50 = 50m search radius
#' @export
#' @examples
#' # The following example will find all roads within 1km of the point Lat: -45.32112 and Lng: 170.40021 on the database authorised through the headers object
#' closest_roads <- getClosestRoads(headers=headers, wkt="POINT(170.40021 -45.32112)", 1000)

getClosestRoads = function(headers, wkt, search_distance=50) {
  #  Function to return all the closest roads to a given geometry, tested POINT and POLYGON
  #  Note:  The wkt must be enclosed in quotes

  query_params = paste0('"',wkt,'"')  #enclosing wkt in quotes

  data_req  = httr::POST(
    'https://apps.ramm.co.nz/RammApi6.1/v1/geometryattributes/closeroads' ,
    headers,
    body = query_params,
    query= list(distance=search_distance)
  )

  result = httr::content(data_req,'text')
  output = jsonlite::fromJSON(result)

  return(output)

}

#' convertWKT
#' 
#' Converts a WKT string from one EPSG to another
#' @param wkt String: Valid representation of a WKT geometry
#' @param epsg_from String: The current EPSG number of the WKT e.g. 2193 for NZTM
#' @param epsg_to String: The EPGS number you want to convert to e.g. 4326 for WGS84
#' @export
#' @examples 
#' new_wkt <- convertWKT(wkt = old_wkt, epsg_from = "2193", epsg_to = "4326")


convertWKT = function(wkt,epsg_from,epsg_to){
  
  crs_from = rgdal::CRS(paste0("+init=epsg:", epsg_from))
  crs_to = rgdal::CRS(paste0("+init=epsg:", epsg_to))
  
  geom = rgeos::readWKT(wkt)
  sp::proj4string(geom) = crs_from
  new_geom = rgdal::spTransform(geom,crs_to)
  
  return(rgeos::writeWKT(new_geom))
  
}

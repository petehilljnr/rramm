library(tidyverse)
library(jsonlite)
library(rgdal)
library(rgeos)
library(leaflet)
library(htmlwidgets)
library(httr)

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
  
  crs_from = CRS(paste0("+init=epsg:", epsg_from))
  crs_to = CRS(paste0("+init=epsg:", epsg_to))
  
  geom = readWKT(wkt)
  proj4string(geom) = crs_from
  new_geom = spTransform(geom,crs_to)
  
  return(writeWKT(new_geom))
  
}
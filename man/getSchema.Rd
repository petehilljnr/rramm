% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/getSchema.R
\name{getSchema}
\alias{getSchema}
\title{getSchema Function}
\usage{
getSchema(headers, table_name, load_type = "CoreAndLocation",
  named_columns = "")
}
\arguments{
\item{headers}{Authorisation headers created using getHeaders()}

\item{table_name}{RAMM table you want column information for.}

\item{named_columns}{Vector of columns that information is requierd for.}

\item{loadType}{What type of column information is required - 'CoreAndLocation' (i.e. core and primary_key),'Specified' or 'All'}
}
\description{
Retrieves column information for the specified RAMM table
}
\examples{


hdrs <- getAuthorisedHeaders(userName='api_demo', password='thursday', database='RAMM API Demo')
#Core
getSchema(hdrs,'carr_way','Core')
#All
getSchema(hdrs,'carr_way','All')
#Specified
getSchema(hdrs,'carr_way','Specified',columns=list('road_id','carr_way_no'))

}

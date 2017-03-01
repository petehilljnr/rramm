#' getHeaders Function
#'
#' This function creates the headers for your requests to the RAMM API
#' @param userName RAMM user login
#' @param password Password for your login
#' @param database Name of the RAMM database you want to connect to
#' @export
#' @examples
#' headers <- getHeaders(userName='xxxx', password='xxxxx', database='A RAMM database')

getHeaders = function(userName,password,database) {
  #  The basis for all of the API calls is the headers with the authorization token
  #  This function should be called first to retrieve a valid token
  #  It returns the headers to be used in subsequent API calls
  #  TODO:  Error message/catching for invalid login

  site = 'https://apps.ramm.co.nz'
  basePath = '/RammApi6.1/v1/'
  headers = httr::add_headers('Content-type'='application/json', 'referer'='https://test.com')

  #authenticate
  authenticateParams =sprintf('authenticate/login?database=%s&userName=%s&password=%s',
                              database,userName,password)

  #need to escape spaces etc
  auth_url = URLencode(paste0(site,basePath,authenticateParams))

  #fetch authorization key
  r = httr::POST(auth_url,
           headers
           #add_headers('Content-type'='application/json', 'referer'='https://test.com')
  )

  ramm_key = httr::content(r)

  keyed_headers = httr::add_headers(
    Authorization = paste0('Bearer ',ramm_key),
    'Content-type'='application/json',
    'referer'='https://nz.mwhglobal.com'
  )

  return(keyed_headers)
}


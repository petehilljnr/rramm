#' downloadMedia Function
#'
#' Downloads and saves all multimedia objects associated with a particular asset
#' You need to get the headers using the getHeaders() function
#'
#' @param headers Authorised access headers created using  getHeaders(...).
#' @param table_name The name of the table the asset is in (must be a valid table name)
#' @param asset_id The id of the asset in the table you want to download
#' @param path The path to where all files will be downloaded to
#' @importFrom dplyr "%>%"
#' @export
#' @examples
#' \dontrun{
#' #The following example downloads all multimedia objects associated with sign id 1
#'
#' sign_table <- getTableData(
#' headers=hdrs,
#' table_name="sign",
#' asset_id=1,
#' path="./temp_folder/"
#' )
#'
#' }

downloadMultimedia = function(headers,
                              table_name,
                              asset_id,
                              path) {

  if (!dir.exists(path)) {
    stop(paste("The folder", path, "doesn't exist.  You must create it first."))
  } else {
    data_req  = httr::GET(
      paste0(
        "https://api.ramm.com:443/v1/multimedia/",
        table_name,
        "?assetIds=",
        asset_id,
        collapse = ""
      ),
      headers
    )

    ct = httr::content(data_req)

    print(data_req$status_code)

    if (data_req$status_code != 200) {
      stop(paste('Status code', ct$message))

    } else {
      metaData = ct %>% {
        tibble::tibble(
          id = purrr::map_int(., "id"),
          fileInfo = purrr::map(., "metaInformation")
        )
      } %>% dplyr::transmute(
        id,
        file_name = purrr::map_chr(fileInfo, "fileName"),
        file_ext = purrr::map_chr(fileInfo, "fileExtension")
      )

      metaData = metaData %>%
        dplyr::mutate(result = purrr::pmap_chr(list(file_name, id),
                                               function(file_name, id) {
                                                 media_req = httr::GET(
                                                   paste0(
                                                     "https://api.ramm.com:443/v1//multimedia/media/",
                                                     id,
                                                     collapse = ""
                                                   ),
                                                   headers
                                                 )

                                                 if (media_req$status_code != 200) {
                                                   warning(
                                                     paste(
                                                       'Error downloading file: ',
                                                       file_name,
                                                       "(status code: ",
                                                       media_req$status_code,
                                                       " )"
                                                     )
                                                   )
                                                   "Download Error"
                                                 } else {
                                                   tryCatch ({
                                                     writeBin(httr::content(media_req, "raw"),
                                                              paste0(path, file_name, collapse = ""))
                                                     "OK"
                                                   },
                                                   error = function(err) {
                                                     print (err)
                                                     "Error"
                                                   })
                                                 }
                                               }))
    }
  }
}

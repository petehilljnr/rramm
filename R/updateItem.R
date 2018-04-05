hdrs = getAuthorisedHeaders('api_demo','thursday','RAMM API Demo')

table = "sign"
id = 808
data = data.frame(columnName = c("location","side"),
                  newValue = c("10000","J"))

query_params = list(getGeometry=FALSE
       ,getSchema=FALSE
       ,getMetaData=TRUE
       ,expandLookups=FALSE
       ,assetId = id
)

url = URLencode(paste0('https://apps.ramm.co.nz:443/RammApi6.1/v1/data/',table,collapse = ""))

data_req  = httr::GET(
  url,
  query = query_params,
  hdrs
)

ct = httr::content(data_req,'text')
metaData = jsonlite::fromJSON(ct)
metaId = metaData$itemMetadata$id
metaHash = metaData$itemMetadata$dataHash

url = URLencode(paste0('https://apps.ramm.co.nz:443/RammApi6.1/v1/data/',metaId,'/',metaHash,collapse = ""))

updateData = jsonlite::toJSON(data, auto_unbox = TRUE)

data_req = httr::POST(
  url,
  body = updateData,
  hdrs
)

ct = httr::content(data_req,'text')
ct
sign2 = getTableData(hdrs,
                     table_name = "sign",
                     load_type = "All",
                     filters = list(createFilter("sign_id","EqualTo","808")))


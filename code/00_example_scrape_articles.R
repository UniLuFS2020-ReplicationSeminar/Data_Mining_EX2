library(httr)
library(jsonlite)

api_key <- readLines("credentials/guardian_key.txt", n = 1)
# api_key <- rstudioapi::askForPassword()
query <- "Clean Energy"
from_date <- "2023-01-01"
base_url <- "https://content.guardianapis.com/search"

response <- GET(url = base_url, query = list('api-key' = api_key, q = query,
                                             from_date = from_date))


json_response <- content(response, "text")
json_response <- fromJSON(json_response)
str(json_response)
results_df <- as.data.frame(json_response$response$results)
summary(results_df)

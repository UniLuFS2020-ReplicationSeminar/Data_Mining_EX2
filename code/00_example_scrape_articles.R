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

#search for articles based on electric cars to make a comparison. 

query_car <- "electric car"

response_car <- GET(url = base_url, query = list('api-key' = api_key, q = query_car,
                                             from_date = from_date))

json_response_c <- content(response_car, "text")
json_response_c <- fromJSON(json_response_c)
str(json_response_c)
results_df_c <- as.data.frame(json_response_c$response$results)
summary(results_df_c)

###NEW###

#For the start, we can use the pre-made wrapper function guardianapi
install.packages("guardianapi")
library(guardianapi)

api_key <- readLines("credentials/guardian_key.txt", n = -1, warn = FALSE)
Sys.setenv(GUARDIAN_API_KEY = api_key) 

query_energy <- "clean energy"
query_car <- "electric car"
from_date <- "2023-01-01"

results_energy <- gu_content(query = query_energy, from_date = from_date)
results_energy <- gu_content(query = query_car, from_date = from_date)

###NEW###

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
#I am expending the time range for further results. 

query_car <- "electric car"
from_date_c <- "2020-01-01"

response_car <- GET(url = base_url, query = list('api-key' = api_key, q = query_car,
                                             from_date_c = from_date_c))

json_response_c <- content(response_car, "text")
json_response_c <- fromJSON(json_response_c)
str(json_response_c)
results_df_c <- as.data.frame(json_response_c$response$results)
summary(results_df_c)

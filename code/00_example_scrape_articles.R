###NEW###

#For the start, we can use the pre-made wrapper function guardianapi
install.packages("guardianapi")
library(guardianapi)

api_key <- readLines("credentials/guardian_key.txt", n = 1, warn = FALSE)
Sys.setenv(GU_API_KEY = api_key) 
gu_api_key(check_env = TRUE)

query_energy <- "clean energy"
query_car <- "electric car"
from_date <- "2023-01-01"

results_energy <- gu_content(query = query_energy, from_date = from_date)
results_car <- gu_content(query = query_car, from_date = from_date)

results_energy$check <- rep(0, nrow(results_energy))

library(stringr)

for (i in 1:nrow(results_energy)){
  if(any(str_detect(results_energy$body_text[i], "clean energy"))) {
    results_energy$check[i] <- 1
  } else {
    results_energy$check[i] <- 0
  }
}

results_energy_only <- results_energy[results_energy$check==1,]

results_car$check <- rep(0, nrow(results_car))

for (i in 1:nrow(results_car)){
  if(any(str_detect(results_car$body_text[i], "electric car"))) {
    results_car$check[i] <- 1
  } else {
    results_car$check[i] <- 0
  }
}

results_car_only <- results_car[results_car$check==1,]

library(tidyverse)
results_energy_selected <- select(results_energy_only, 
                               type, 
                               publication,
                               pillar_name, 
                               section_name, 
                               byline, 
                               web_title, 
                               headline, 
                               standfirst, 
                               body_text, 
                               web_publication_date, 
                               first_publication_date, 
                               last_modified, 
                               production_office, 
                               tags,
                               is_live, 
                               wordcount, 
                               char_count)

results_car_selected <- select(results_car_only, 
                               type, 
                               publication,
                               pillar_name, 
                               section_name, 
                               byline, 
                               web_title, 
                               headline, 
                               standfirst, 
                               body_text, 
                               web_publication_date, 
                               first_publication_date, 
                               last_modified, 
                               production_office, 
                               tags,
                               is_live, 
                               wordcount, 
                               char_count)
###NEW###

library(httr)
library(jsonlite)
library(tidyverse)

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
                                             from_date = from_date_c))

json_response_c <- content(response_car, "text")
json_response_c <- fromJSON(json_response_c)
str(json_response_c)
results_df_c <- as.data.frame(json_response_c$response$results)
summary(results_df_c)




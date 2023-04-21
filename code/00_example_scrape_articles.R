# Install the package guardianapi which includes a pre-made wrapper function to interact with The Guardian's API 
# install.packages("guardianapi")

# Load the package "guardianapi"
library(guardianapi)

# Define the api-key as a variable by reading it from a text-file
api_key <- readLines("credentials/guardian_key.txt", n = 1, warn = FALSE)

# Set the GU_API_KEY environment variable to the value of the api_key
Sys.setenv(GU_API_KEY = api_key) 

# Assign the API key to the gu.API.key variable (-> gu_content accesses api-key automatically)
gu_api_key(check_env = TRUE)

# Define variables which will be used to query The Guardian's API 
query_energy <- "clean energy"
query_car <- "electric car"
from_date <- "2020-01-01"

# Query The Guardian's API for articles with the above defined criteria
results_energy <- gu_content(query = query_energy, from_date = from_date)
results_car <- gu_content(query = query_car, from_date = from_date)

# ## Save and Load the scrape if you don't want to request the data everytime
# save(results_energy, file = 'data/data_raw/scrape_energy_raw.Rdata')
# save(results_car, file = 'data/data_raw/scrape_car_raw.Rdata')
# load('data/data_raw/')
# load('data/data_raw/')
# ##

## Unfortunately, we can see that there are certain articles that are no related 
# to the topics "clean energy" and / or "electric car" at all. E.g.
results_energy$body_text[54]
results_car$body_text[52]
# Hence, we need to filter out these undesired observations by.

# Add a variable to each data set which will be used to filter the undesired observations out:
results_energy$check <- rep(0, nrow(results_energy))
results_car$check <- rep(0, nrow(results_car))

# Load the package "stringr"
library(stringr)

# Loop to filter undesired observations out (results_energy data set):
# loop over the total number of observations in "results_energy",
# if body_text contains "clean energy", then change the check variable to one,
# otherwise, check variable should remain 0.

for (i in 1:nrow(results_energy)) {
  if (any(str_detect(results_energy$body_text[i], "clean energy")) |
      any(str_detect(results_energy$body_text[i], "renewable energy"))) {
    results_energy$check[i] <- 1
  } else {
    results_energy$check[i] <- 0
  }
}

# Loop to filter undesired observations out (results_car data set):           
# Create the same loop but observe for "electric car" OR "electric vehicle"
for (i in 1:nrow(results_car)) {
  if (any(str_detect(results_car$body_text[i], "electric car")) |
      any(str_detect(results_car$body_text[i], "electric vehicle"))) {
    results_car$check[i] <- 1
  } else {
    results_car$check[i] <- 0
  }
}

# Keep only the initially desired articles (those that include the words "clean energy" or "electric car", respectively)
results_energy_selected <- results_energy[results_energy$check==1,]
results_car_selected <- results_car[results_car$check==1,]

# Load the package "tidyverse"
library(tidyverse)

# Select the variables that can be used for an analysis later on and sort them accordingly ("results_energy_only data set")
results_energy_selected <- select(results_energy_selected, 
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

# Select the variables that can be used for an analysis later on and sort them accordingly ("results_car_only data set")
results_car_selected <- select(results_car_selected, 
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

# Save the data frames in the data_raw folder
save(results_car_selected, file = "data/data_processed/results_car_selected.rdata")
save(results_energy_selected, file = "data/data_processed/results_energy_selected.rdata")



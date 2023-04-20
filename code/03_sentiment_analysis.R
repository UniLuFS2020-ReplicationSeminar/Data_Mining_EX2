# Load necessary libraries
library(tidytext)
library(dplyr)
library(ggplot2)
library(lubridate)


load('data/data_processed/results_energy_selected.rdata')
load('data/data_processed/results_car_selected.rdata')

test_energy <- results_energy_selected %>% 
  select(first_publication_date, body_text)
test_energy <- cbind(id = seq_len(nrow(test_energy)), test_energy)
test_energy$first_publication_date <- as.Date(test_energy$first_publication_date)



# Perform sentiment analysis
sentiments <- test_energy %>%
  unnest_tokens(word, body_text) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(id) %>%
  summarize(sent_score = mean(value))

# Add the sentiment scores to the original dataframe
test_energy <- left_join(test_energy, sentiments, by = "id")

ggplot(test_energy, aes(x = first_publication_date, y = sent_score)) +
  geom_point() +
  geom_smooth()

article_counts <- test_energy %>%
  group_by(first_publication_date) %>%
  summarize(count = n())

# Count the number of articles by month
monthly_article_counts <- test_energy %>%
  mutate(month = floor_date(first_publication_date, unit = "month")) %>%
  group_by(month) %>%
  summarize(count = n())

# Plot the number of articles by month
ggplot(monthly_article_counts, aes(x = month, y = count)) +
  geom_col() +
  scale_x_date(date_breaks = "1 month", date_labels = "%b %Y") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5))




## Syuzhet Sentiment Analysis

library(syuzhet)

# Perform sentiment analysis
sentiments_syu <- test_energy %>%
  mutate(sent_score_syu = get_sentiment(body_text, method = "syuzhet")) %>%
  select(id, sent_score)

# Add the sentiment scores to the original dataframe
test_energy <- left_join(test_energy, sentiments_syu, by = "id")








# Load necessary libraries
library(tidytext)
library(dplyr)
library(textdata)
library(syuzhet)
library(scales)

# Define text data
text <- "This is a great day. I am so happy. But the weather is mediocre"

#AFINN Score is between 5 and -5
rescale_afinn <- function(x, old_range = c(-5, 5), new_range = c(-1, 1)) {
  (x - old_range[1]) / (old_range[2] - old_range[1]) * (new_range[2] - new_range[1]) + new_range[1]
}

# Split text into sentences
sentences <- tibble(line = 1:length(text), text = unlist(strsplit(text, "(?<=[.!?])\\s", perl = TRUE)))

# Perform sentiment analysis
sentiment_sent <- sentences %>%
  unnest_tokens(word, text) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(line) %>%
  rescale(senteces$value)
  summarize(sentiment = mean(value)) %>%
  mutate(sentiment = rescale(sentiment))
# View results


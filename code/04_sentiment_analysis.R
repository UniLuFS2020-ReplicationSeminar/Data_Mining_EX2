# Load the necessary libraries
library(tidytext)
library(dplyr)
library(ggplot2)
library(syuzhet)
library(vader)
library(tidyverse)
library(lubridate)
library(purrr)

# Load the samples 
load("data/data_processed/sampled_data_car.Rdata")
load("data/data_processed/sampled_data_energy.Rdata")

test_energy <- sampled_data_energy %>% 
  select(first_publication_date, body_text)
test_energy <- cbind(id = seq_len(nrow(test_energy)), test_energy)
test_energy$first_publication_date <- as.Date(test_energy$first_publication_date)


#Sentiment Analysis ----

# We will perform various sentiment analysis methods on our articles and compare the results

# AFINN Method uses a range of 5 and -5 so we will rescale to compare it with the other methods
# Define rescale function
rescale_afinn <- function(x, old_range = c(-5, 5), new_range = c(-1, 1)) {
  (x - old_range[1]) / (old_range[2] - old_range[1]) * (new_range[2] - new_range[1]) + new_range[1]
}

# Calculate sentiment scores using the AFINN method
afinn_scores <- test_energy %>%
  unnest_tokens(word, body_text) %>%
  inner_join(get_sentiments("afinn")) %>%
  group_by(id) %>%
  summarize(sent_score = mean(value)) %>%
  ungroup() %>%
  mutate(sent_score = rescale_afinn(sent_score))

# Compute the Syuzhet method
# syuzhet_scores <- test_energy %>%
#   mutate(sent_score = get_sentiment(body_text, method = "syuzhet"))
# summary(syuzhet_scores$sent_score)

# Because Syuzhet Score calculates an absolute Value 
# Articles with a lot of sentences will get higher values
#So we need to first loop through the sentences of each article and get the sentiment score average 
syuzhet_scores <- test_energy %>%
  mutate(sentences = map(body_text, get_sentences)) %>%
  mutate(sent_score = map_dbl(sentences, ~ mean(get_sentiment(.x, method = "syuzhet")))) %>%
  select(-sentences)
summary(syuzhet_scores$sent_score)


# Compute sentiment scores using the bing method
#test fast computation
bing_scores <- test_energy %>%
  mutate(sentences = map(body_text, get_sentences)) %>%
  mutate(sent_score = map_dbl(sentences, ~ mean(get_sentiment(.x, method = "bing")))) %>%
  select(-sentences)
summary(bing_scores$sent_score)


# Compute sentiment scores using the nrc method
# Takes Too Long
nrc_scores <- test_energy %>%
  mutate(sentences = map(body_text, get_sentences)) %>%
  mutate(sent_score = map_dbl(sentences, ~ mean(get_sentiment(.x, method = "nrc")))) %>%
  select(-sentences)
summary(nrc_scores$sent_score)



test <- test_energy$body_text[1]
test_sent <- get_sentences(test)
nrc_vector <- get_sentiment(test_sent, method="nrc")
mean(nrc_vector)
as.numeric(get_vader(text = test_sent, rm_qm = TRUE)["compound"])


#Vader Sentiment Analysis
# TAKES TOO LONG
# vader_scores <- test_energy %>%
#   mutate(sent_score = map_dbl(body_text, ~ as.numeric(get_vader(text = .x, rm_qm = TRUE)["compound"])))


# Add sentiment scores to the original data frame
sentiments_model <- test_energy %>%
  left_join(afinn_scores %>% select(id, afinn_score = sent_score), by = "id") %>%
  left_join(syuzhet_scores %>% select(id, syuzhet_score = sent_score), by = "id") %>%
  left_join(bing_scores %>%  select(id, bing_score = sent_score), by = "id") 
  # %>% left_join(vader_scores %>% select(id, vader_score = sent_score), by = "id") 
 
sentiments_model_long <- sentiments_model %>%
  select(first_publication_date, afinn_score, syuzhet_score, bing_score) %>%
  pivot_longer(cols = c(afinn_score, syuzhet_score, bing_score),
               names_to = "model",
               values_to = "score")

plot <- ggplot(sentiments_model_long, aes(x = first_publication_date, y = score, color = model)) +
  geom_smooth(se = FALSE) +
  labs(title = "Sentiment Scores Over Time",
       x = "Publication Date", y = "Average Sentiment Score", color = "Sentiment Analysis Method") +
  theme_minimal()

ggsave('output/plots/sentiment_energy_over_time.png', width = 10, height = 10)




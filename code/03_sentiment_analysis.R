# Load the necessary libraries
library(tidytext)
library(dplyr)
library(ggplot2)
library(syuzhet)
library(vader)
library(tidyverse)
library(lubridate)

load('data/data_processed/results_energy_selected.rdata')
load('data/data_processed/results_car_selected.rdata')

test_energy <- results_energy_selected %>% 
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
sent_scores <- numeric(nrow(test_energy))

for (i in seq_along(sent_scores)) {
  sentences <- get_sentences(test_energy$body_text[i])
  sent_scores[i] <- mean(get_sentiment(sentences, method = "syuzhet"))
}

syuzhet_scores <- test_energy %>%
  mutate(sent_score = sent_scores)
summary(syuzhet_scores$sent_score)


#test fast computation
fast_scores <- test_energy %>%
  mutate(sentences = map(body_text, get_sentences)) %>%
  mutate(sent_score = map_dbl(sentences, ~ mean(get_sentiment(.x, method = "syuzhet")))) %>%
  select(-sentences)


# Compute sentiment scores using the bing method
bing_scores <- test_energy %>%
  unnest_tokens(word, body_text) %>%
  inner_join(get_sentiments("bing")) %>%
  group_by(id) %>%
  summarize(sent_score = mean(value)) %>%
  ungroup()

sent_scores <- numeric(nrow(test_energy))

for (i in seq_along(sent_scores)) {
  sentences <- get_sentences(test_energy$body_text[i])
  sent_scores[i] <- mean(get_sentiment(sentences, method = "bing"))
}

bing_scores <- test_energy %>%
  mutate(sent_score = sent_scores)
summary(bing_scores$sent_score)



# Compute sentiment scores using the nrc method
nrc_scores <- test_energy %>%
  mutate(sentences = map(body_text, get_sentences)) %>%
  mutate(sent_score = map_dbl(sentences, ~ mean(get_sentiment(.x, method = "nrc")))) %>%
  select(-sentences)



test <- test_energy$body_text[1]
test_sent <- get_sentences(test)
nrc_vector <- get_sentiment(test_sent, method="nrc")
mean(syuzhet_vector)

get_vader(text = test, rm_qm = TRUE)["compound"]

# Compute sentiment scores using the vader method
vader_scores <- data.frame(id = test_energy$id,
                           sent_score = sapply(test_energy$body_text, function(x) get_vader(text = x, rm_qm = TRUE)["compound"]))

# Add sentiment scores to the original data frame
sentiments <- test_energy %>%
  left_join(afinn_scores, by = "id") %>%
  left_join(syuzhet_scores %>% select(id, syuzhet_score = sent_score), by = "id") %>%
  left_join(bing_scores, by = "id") %>%
  left_join(vader_scores %>% select(id, vader_score = sent_score), by = "id") %>%
  rename(afinn_score = sent_score.x, bing_score = sent_score.y)
















## OLD -----



# Perform sentiment analysis using four different methods
sentiments <- test_energy %>%
  mutate(
    afinn_score = unnest_tokens(word, body_text) %>%
      inner_join(get_sentiments("afinn")) %>%
      summarize(sent_score = mean(value)) %>%
      pull(sent_score),
    syuzhet_score = get_sentiment(body_text, method = "syuzhet"),
    bing_score = unnest_tokens(word, body_text) %>%
      inner_join(get_sentiments("bing")) %>%
      summarize(sent_score = mean(value)) %>%
      pull(sent_score),
    vader_score = get_vader_sentiment(body_text)$compound
  ) %>%
  mutate(afinn_score = rescale_afinn(afinn_score))
# Reshape the data for plotting
sentiments_long <- sentiments %>%
  select(id, first_publication_date, afinn_score, syuzhet_score, bing_score, vader_score) %>%
  pivot_longer(-c(id, first_publication_date), names_to = "method", values_to = "sent_score")

# Plot the average sentiment scores by method and publication date
ggplot(sentiments_long, aes(x = first_publication_date, y = sent_score, color = method)) +
  geom_smooth() +
  labs(x = "Publication Date", y = "Average Sentiment Score", color = "Sentiment Analysis Method")
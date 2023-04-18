#Script to evaluate the data

library(dplyr)
library(ggplot2)

#Load datasets

results_car <- load("data/data_processed/results_car_selected.Rdata")
results_energy <- load("data/data_processed/results_energy_selected.Rdata")
                    
# Count occurrences of "sustainable" in body_text column
keyword <- "sustainable"
results_energy_selected <- mutate(results_energy_selected, count = rowSums(across(everything(), ~str_detect(tolower(.x), keyword))))

# Create a bar graph to visualize the results
plot1 <- ggplot(results_energy_selected, aes(x = factor(count), fill = factor(count))) +
  geom_bar() +
  scale_fill_discrete(name = "Count of 'sustainable'") +
  labs(title = "Occurrences of 'sustainable' in body text",
       x = "Count",
       y = "Frequency")

print(plot1)



# Count occurrences of "challenge" in body_text column
keyword_2 <- "challenge"
results_energy_selected <- mutate(results_energy_selected, count = rowSums(across(everything(), ~str_detect(tolower(.x), keyword_2))))

# Create a bar graph to visualize the results
plot2 <- ggplot(results_energy_selected, aes(x = factor(count), fill = factor(count))) +
  geom_bar() +
  scale_fill_discrete(name = "Count of 'challenge'") +
  labs(title = "Occurrences of 'challenge' in body text",
       x = "Count",
       y = "Frequency")

print(plot2)


#show in which articles the keyword has been used a specific amount of time

# Define the keyword and number of occurrences to search for
num_occurrences <- 4

# Filter the dataset to rows where the keyword occurs the specified number of times in the "body_text" column
results_energy_selected %>%
  filter(str_count(tolower(body_text), keyword_2) == num_occurrences)



# Visualize Pairs 


# Tokenize all the words from the article and remove stopwords
tidy_clean_energy <- results_energy_selected %>%
  unnest_tokens(word, body_text) %>%
  anti_join(stop_words) %>%
  mutate(word = str_remove_all(word, "[^[:alnum:]]")) %>%
  filter(str_length(word) > 2)

#Filter to words used more than 3 times
word_counts <- tidy_clean_energy %>%
  count(word, sort = TRUE) %>%
  filter(n > 3)

word_pairs <- tidy_clean_energy %>%
  mutate(word_lead = lead(word)) %>%
  filter(!is.na(word_lead)) %>%
  count(word, word_lead, sort = TRUE) %>%
  filter(n > quantile(n, probs = .95)) %>%
  slice_head(n = 50)

# Create a bar plot showing the top word pairs using ggplot2
ggplot(word_pairs, aes(x = reorder(paste(word, word_lead), n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal()

# Save the plot in a specific directory by specifying the full path to the desired location
ggsave("output/plots/top_word_pairs_energy.png", width = 10, height = 8)


# Tokenize all the words from the article and remove stopwords
tidy_car <- results_car_selected %>%
  unnest_tokens(word, body_text) %>%
  anti_join(stop_words) %>%
  mutate(word = str_remove_all(word, "[^[:alnum:]]")) %>%
  filter(str_length(word) > 2)

#Filter to words used more than 3 times
word_counts_car <- tidy_car %>%
  count(word, sort = TRUE) %>%
  filter(n > 3)

word_pairs_car <- tidy_car %>%
  mutate(word_lead = lead(word)) %>%
  filter(!is.na(word_lead)) %>%
  count(word, word_lead, sort = TRUE) %>%
  filter(n > quantile(n, probs = .95)) %>%
  slice_head(n = 50)

# Create a bar plot showing the top word pairs using ggplot2
ggplot(word_pairs_car, aes(x = reorder(paste(word, word_lead), n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal()

# Save the plot in a specific directory by specifying the full path to the desired location
ggsave("output/plots/top_word_pairs_car.png", width = 10, height = 8)



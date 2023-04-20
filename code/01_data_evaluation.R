#Script to evaluate the data

library(dplyr)
library(ggplot2)
library(stringr)

#Load datasets

results_car <- load("data/data_raw/results_car_selected.Rdata")
results_energy <- load("data/data_raw/results_energy_selected.Rdata")

#Specific word count in all articles-----------

# Count occurrences of "sustainable" in body_text column
# results are mutated on to the dataframe
keyword <- "sustainable"
results_energy_selected <- mutate(
  results_energy_selected, 
  count = rowSums(across(everything(), ~str_detect(tolower(.x), keyword))))

# Create a bar graph to visualize the results
# add a NA filter so the articles where the word is not found are not stated on the graph
plot1 <- ggplot(results_energy_selected %>% filter(!is.na(count) & count != 0), 
                aes(x = factor(count), fill = factor(count))) +
  geom_bar() +
  scale_fill_discrete(name = "Count of 'sustainable'") +
  labs(title = "Occurrences of 'sustainable' in body text of articles with 'clean energy' as main topic",
       x = "Occurrences",
       y = "Amount of articles")

print(plot1)

#save plot 1
ggsave("output/plots/word_count_sustainable.png", width = 16, height = 9)

# Count occurrences of "challenge" in body_text column
keyword_2 <- "challenge"
results_energy_selected <- mutate(results_energy_selected,
 count = rowSums(across(everything(), ~str_detect(tolower(.x), keyword_2))))

# Create a bar graph to visualize the results
plot2 <- ggplot(results_energy_selected %>% filter(!is.na(count) & count != 0), 
                aes(x = factor(count), fill = factor(count))) +
  geom_bar() +
  scale_fill_discrete(name = "Count of 'challenge'") +
  labs(title = "Occurrences of 'challenge' in body text of articles with 'clean energy' as main topic",
       x = "Occurrences",
       y = "Amount of articles")

print(plot2)

#Save second plot
ggsave("output/plots/word_count_challenge.png", width = 16, height = 9)

#The same search is done in hte car articles
results_car_selected <- mutate(
  results_car_selected, 
  count = rowSums(across(everything(), ~str_detect(tolower(.x), keyword))))

plot3 <- ggplot(results_car_selected %>% filter(!is.na(count) & count != 0), 
                aes(x = factor(count), fill = factor(count))) +
  geom_bar() +
  scale_fill_discrete(name = "Count of 'sustainable'") +
  labs(title = "Occurrences of 'sustainable' in body text of articles with 'car' as main topic",
       x = "Occurrences",
       y = "Amount of articles")

print(plot3)

#Save third plot
ggsave("output/plots/word_count_sustainable_car.png", width = 16, height = 9)

# Count occurrences of "challenge" in body_text column of car articles
keyword_2 <- "challenge"
results_car_selected <- mutate(results_car_selected,
                                  count = rowSums(across(everything(), ~str_detect(tolower(.x), keyword_2))))

#Create the plot for the 
plot4 <- ggplot(results_car_selected %>% filter(!is.na(count) & count != 0), 
                aes(x = factor(count), fill = factor(count))) +
  geom_bar() +
  scale_fill_discrete(name = "Count of 'challenge'") +
  labs(title = "Occurrences of 'challenge' in body text of articles with 'car' as main topic",
       x = "Occurrences",
       y = "Amount of articles")

print(plot4)

#Save third plot
ggsave("output/plots/word_count_challenge_car.png", width = 16, height = 9)

#show in which articles the keyword has been used a specific amount of time

# Define the keyword and number of occurrences to search for
num_occurrences <- 4

# Filter the dataset to rows where the keyword occurs the specified number of times
# in the "body_text" column
results_energy_selected %>%
  filter(str_count(tolower(body_text), keyword_2) == num_occurrences)



# Visualize Pairs --------
library(tidytext)
library(gridExtra)

# Create a function to generate the word pairs plot
generate_word_pairs_plot <- function(data, title) {
  # Tokenize all the words from the article and remove stopwords
  tidy_data <- data %>%
    unnest_tokens(word, body_text) %>%
    anti_join(stop_words) %>%
    mutate(word = str_remove_all(word, "[^[:alnum:]]")) %>%
    filter(str_length(word) > 2)

  # Filter to words used more than 3 times
  word_counts <- tidy_data %>%
    count(word, sort = TRUE) %>%
    filter(n > 3)

  word_pairs <- tidy_data %>%
    mutate(word_lead = lead(word)) %>%
    filter(!is.na(word_lead)) %>%
    count(word, word_lead, sort = TRUE) %>%
    filter(n > quantile(n, probs = .95)) %>%
    slice_head(n = 25)

  # Create a bar plot showing the top word pairs using ggplot2
  p <- ggplot(word_pairs, aes(x = reorder(paste(word, word_lead), n), y = n)) +
    geom_col(fill = "steelblue") +
    coord_flip() +
    ggtitle(title) +
    theme_minimal()

  return(p)
}

# Generate the word pairs plot for the energy data
p1 <- generate_word_pairs_plot(results_energy_selected, "Top Bi-Grams Energy")

# Generate the word pairs plot for the car data
p2 <- generate_word_pairs_plot(results_car_selected, "Top Bi-Grams Cars")

# Combine the plots into a single grid
grid.arrange(p1, p2, ncol = 1)

combined_plot <- arrangeGrob(p1, p2, ncol = 2)

# Save the grob object using ggsave()
ggsave("output/plots/top_word_pairs_combined.png",
 combined_plot, width = 16, height = 9)

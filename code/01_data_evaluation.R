#Script to evaluate the data

library(dplyr)
library(ggplot2)
library(stringr)
library(tidytext)
library(gridExtra)
library(ggpubr)

#Load datasets

results_car <- load("data/data_processed/results_car_selected.Rdata")
results_energy <- load("data/data_processed/results_energy_selected.Rdata")

#Specific word count in all articles-----------

# Count occurrences of "sustainable" in body_text column
# results are mutated on to the dataframe
sustainable_keyword <- "sustainable"
results_energy_selected <- mutate(
  results_energy_selected, 
  count = rowSums(across(everything(), ~str_detect(tolower(.x), sustainable_keyword))))

# Create a bar graph to visualize the results
# add a NA filter so the articles where the word is not found are not stated on the graph
sustainable_energy_plot <- ggplot(results_energy_selected %>% filter(!is.na(count) & count != 0), 
                aes(x = factor(count), fill = factor(count))) +
  geom_bar() +
  scale_fill_discrete(name = "Count of 'sustainable'") +
  labs(title = "Occurrences of 'sustainable' in body text of articles with 'clean energy' as main topic",
       x = "Occurrences",
       y = "Amount of articles")

print(sustainable_energy_plot)

#save plot 1
ggsave("output/plots/word_count_sustainable.png", width = 16, height = 9)

# Count occurrences of "challenge" in body_text column
challenge_keyword <- "challenge"
results_energy_selected <- mutate(results_energy_selected,
 count = rowSums(across(everything(), ~str_detect(tolower(.x), challenge_keyword))))

# Create a bar graph to visualize the results
challenge_energy_plot <- ggplot(results_energy_selected %>% filter(!is.na(count) & count != 0), 
                aes(x = factor(count), fill = factor(count))) +
  geom_bar() +
  scale_fill_discrete(name = "Count of 'challenge'") +
  labs(title = "Occurrences of 'challenge' in body text of articles with 'clean energy' as main topic",
       x = "Occurrences",
       y = "Amount of articles")

print(challenge_energy_plot)

#Save second plot
ggsave("output/plots/word_count_challenge.png", width = 16, height = 9)

#The same search is done in the car articles
results_car_selected <- mutate(
  results_car_selected, 
  count = rowSums(across(everything(), ~str_detect(tolower(.x), sustainable_keyword))))

sustainable_car_plot <- ggplot(results_car_selected %>% filter(!is.na(count) & count != 0), 
                aes(x = factor(count), fill = factor(count))) +
  geom_bar() +
  scale_fill_discrete(name = "Count of 'sustainable'") +
  labs(title = "Occurrences of 'sustainable' in body text of articles with 'electrc car' as main topic",
       x = "Occurrences",
       y = "Amount of articles")

print(sustainable_car_plot)

#Save third plot
ggsave("output/plots/word_count_sustainable_car.png", width = 16, height = 9)

# Count occurrences of "challenge" in body_text column of car articles
challenge_keyword <- "challenge"
results_car_selected <- mutate(results_car_selected,
                                  count = rowSums(across(everything(), ~str_detect(tolower(.x), challenge_keyword))))

#Create the plot for the 
challenge_car_plot <- ggplot(results_car_selected %>% filter(!is.na(count) & count != 0), 
                aes(x = factor(count), fill = factor(count))) +
  geom_bar() +
  scale_fill_discrete(name = "Count of 'challenge'") +
  labs(title = "Occurrences of 'challenge' in body text of articles with 'electric car' as main topic",
       x = "Occurrences",
       y = "Amount of articles")

print(challenge_car_plot)

#Save third plot
ggsave("output/plots/word_count_challenge_car.png", width = 16, height = 9)

#show in which articles the keyword has been used a specific amount of time

# Filter the dataset to show the rows where the keyword "sustainable occurs more than 1 time in the body text of clean energy related articles
Articles_energy_sustainable <- results_energy_selected %>%
  filter(str_count(tolower(body_text), sustainable_keyword) >= 1)

#filter the dataset to show the same results for the keyword "challange"
Articles_energy_challenge <- results_energy_selected %>%
  filter(str_count(tolower(body_text), challenge_keyword) >= 1)



# Visualize Bi-Grams Pairs -----------------------------------------------------------


generate_word_pairs_data <- function(data) {
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
  
  return(word_pairs)
}

generate_word_pairs_plot <- function(word_pairs, title, xlab = "Word Pairs", highlight_x = NULL) {
  # Add a new column specifying the color for each bar based on its x-value
  # Analyze what top word pairs are identical in our 2 datasets
  word_pairs$color <- ifelse(!is.null(highlight_x) & 
                               paste(word_pairs$word, word_pairs$word_lead) 
                             %in% highlight_x, "Identical", "Non-identical")
  
  # Create a bar plot showing the top word pairs using ggplot2
  p <- ggplot(word_pairs, aes(x = reorder(paste(word, word_lead), n), y = n)) +
    geom_col(aes(fill = color)) +
    scale_fill_manual(values = c("Identical" = "skyblue", "Non-identical" = "maroon")) +
    coord_flip() +
    ggtitle(title) +
    xlab(xlab) +
    theme_minimal() +
    labs(fill ="Bi-Gram Matches on Both Topics") +
    theme(legend.position = "bottom")
  
  return(p)
}
set.seed(123)
# Generate the word pairs data for the energy data
word_pairs_energy <- generate_word_pairs_data(results_energy_selected)

# Generate the word pairs data for the car data
word_pairs_car <- generate_word_pairs_data(results_car_selected)

# Find the common word pairs between the two plots
common_pairs <- intersect(paste(word_pairs_energy$word, word_pairs_energy$word_lead), paste(word_pairs_car$word, word_pairs_car$word_lead))

# Generate the word pairs plot for the energy data
plot_bigrams_energy <- generate_word_pairs_plot(word_pairs_energy, "Top Bi-Grams Clean Energy", "Energy Word Pairs", common_pairs)

# Generate the word pairs plot for the car data
plot_bigrams_car <- generate_word_pairs_plot(word_pairs_car, "Top Bi-Grams Electric Cars", "Car Word Pairs", common_pairs)

#Combine the plots with ggpubrs to have a common legend
combined_plot <- ggpubr::ggarrange(plot_bigrams_energy, plot_bigrams_car, ncol = 2, common.legend = TRUE, legend="bottom")
combined_plot
# Save the grob object using ggsave()
ggsave("output/plots/top_word_pairs_combined.png",
       combined_plot, width = 16, height = 9)


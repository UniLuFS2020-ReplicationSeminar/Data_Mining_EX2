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




# Word Tree/Graph --------

library(tidytext)
library(igraph)

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
  filter(n > 3)

word_pairs_graph <- graph_from_data_frame(word_pairs)

## Add some specifics to visualize WordTree Better
# Set the size of the nodes
V(word_pairs_graph)$size <- word_counts$n / max(word_counts$n) * 20 + 2

# Set the size of the labels based on the size of the nodes
V(word_pairs_graph)$label.cex <- V(word_pairs_graph)$size / max(V(word_pairs_graph)$size) * 2 + 0.5

# Set the color of the nodes based on the size of the nodes
V(word_pairs_graph)$color <- rgb(V(word_pairs_graph)$size / max(V(word_pairs_graph)$size), 0, 0)

# Increase the resolution of the image by increasing the width and height parameters
png("output/plots/word_tree.png", width = 1600, height = 1200)
plot(word_pairs_graph,
     edge.arrow.size = 0.5,
     vertex.label.cex = V(word_pairs_graph)$label.cex,
     vertex.label.color = "black",
     vertex.label.family = "sans",
     vertex.color = V(word_pairs_graph)$color)
dev.off()

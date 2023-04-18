library(dplyr)
library(ggplot2)

#Load datasets

results_car <- load("data/data_processed/results_car_selected.Rdata")
results_energy <- load("data/data_processed/results_energy_selected.Rdata")


# Word Tree/Graph --------

library(tidytext)
library(igraph)
library(grid)
library(gridExtra)
library(ggraph)
library(tidygraph)
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
  slice_head(n = 100)

# Create a bar plot showing the top word pairs using ggplot2
ggplot(word_pairs, aes(x = reorder(paste(word, word_lead), n), y = n)) +
  geom_col(fill = "steelblue") +
  coord_flip() +
  theme_minimal()

# Save the plot in a specific directory by specifying the full path to the desired location
ggsave("output/plots/top_word_pairs.png", width = 10, height = 8)


# Calculate the frequency of each word
word_freq <- word_pairs %>%
  group_by(word) %>%
  summarize(freq = sum(n))

# Create an igraph object from the word_pairs data frame
word_pairs_graph <- graph_from_data_frame(word_pairs)

# Add the frequency data as a vertex attribute
V(word_pairs_graph)$freq <- word_freq$freq

# Plot the network graph using ggraph
ggraph(word_pairs_graph, layout = "fr") +
  geom_edge_link(aes(width = n), edge_colour = "grey", show.legend = FALSE) +
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_point(aes(size = freq), color = "steelblue") +
  scale_size_continuous(range = c(3, 8)) +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_void()



# Word Graph/Tree

# Calculate the frequency of each word
word_freq <- word_pairs %>%
  group_by(word) %>%
  summarize(freq = sum(n))

# Create an igraph object from the word_pairs data frame
word_pairs_graph <- graph_from_data_frame(word_pairs)

# Add the frequency data as a vertex attribute
V(word_pairs_graph)$freq <- word_freq$freq

## Add some specifics to visualize WordTree Better
# Set the size of the nodes based on the frequency of the word occurrence
V(word_pairs_graph)$size <- V(word_pairs_graph)$freq / max(V(word_pairs_graph)$freq) * 20 + 2

# Only show labels for nodes with a size above a certain threshold
V(word_pairs_graph)$label <- ifelse(V(word_pairs_graph)$size > 10, V(word_pairs_graph)$name, NA)

# Set the size of the labels based on the size of the nodes
V(word_pairs_graph)$label.cex <- V(word_pairs_graph)$size / max(V(word_pairs_graph)$size) * 2 + 0.5

# Set the color of the nodes based on the size of the nodes
V(word_pairs_graph)$color <- rgb(V(word_pairs_graph)$size / max(V(word_pairs_graph)$size), 0, 0)

# Plot the network graph using ggraph
ggraph(word_pairs_graph, layout = "fr") +
  geom_edge_link(aes(width = n), edge_colour = "grey", show.legend = FALSE) +
  scale_edge_width(range = c(0.2, 2)) +
  geom_node_point(aes(size = size), color = V(word_pairs_graph)$color) +
  scale_size_continuous(range = c(3, 8)) +
  geom_node_text(aes(label = label), repel = TRUE,
                 size = V(word_pairs_graph)$label.cex * 2,
                 color = "grey",
                 family = "sans") +
  theme_void()


# Save the plot in a specific directory by specifying the full path to the desired location
ggsave("output/plots/word_tree_ggraph.png", width = 10, height = 8)
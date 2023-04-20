library(dplyr)
library(ggplot2)
library(tidytext)
library(igraph)
library(grid)
library(gridExtra)
library(ggraph)
library(tidygraph)

# Load datasets
results_car <- load("data/data_raw/results_car_selected.Rdata")
results_energy <- load("data/data_raw/results_energy_selected.Rdata")

# Word Tree/Graph --------


# Create a function to generate the word pairs network graph
generate_word_pairs_graph <- function(data, file_name, title) {
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
    slice_head(n = 100)
  
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
  V(word_pairs_graph)$label <- ifelse(V(word_pairs_graph)$size > 5, V(word_pairs_graph)$name, NA)
  
  # Set the size of the labels based on the size of the nodes
  V(word_pairs_graph)$label.cex <- V(word_pairs_graph)$size / max(V(word_pairs_graph)$size) * 2 + 0.5
  
  # Set the color of the nodes based on the size of the nodes
  V(word_pairs_graph)$color <- rgb(1 - V(word_pairs_graph)$size / max(V(word_pairs_graph)$size), 1 - V(word_pairs_graph)$size / max(V(word_pairs_graph)$size), 1)
  
  # Plot the network graph using ggraph
  set.seed(123)
  
  ggraph(word_pairs_graph, layout = "fr") +
    geom_edge_link(aes(width = n), edge_colour = "pink", show.legend = FALSE) +
    scale_edge_width(range = c(0.2, 2)) +
    geom_node_point(aes(size = size), color = "pink", shape = 21, fill = V(word_pairs_graph)$color) +
    scale_size_continuous(range = c(3, 8)) +
    geom_node_text(aes(label = label), repel = TRUE,
                   size = V(word_pairs_graph)$label.cex * 2,
                   color = "black",
                   family = "sans") +
    ggtitle(title) +
    theme(plot.title = element_text(hjust = 0.5, size = rel(1.5), color = "pink")) +
    theme_void() +
    theme(plot.background = element_rect(fill = "white"))
  
  # Save the plot in a specific directory by specifying the full path to the desired location
  ggsave(paste0("output/plots/", file_name), width = 10, height = 8)
}

# Generate and save the word pairs network graph for the energy data with a centered title
generate_word_pairs_graph(results_energy_selected, "word_tree_ggraph_energy.png", "Energy Word Graph")

# Generate and save the word pairs network graph for the car data with a centered title
generate_word_pairs_graph(results_car_selected, "word_tree_ggraph_car.png", "Car Word Graph")


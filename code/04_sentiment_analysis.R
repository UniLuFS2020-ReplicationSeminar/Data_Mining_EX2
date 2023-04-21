# Load the necessary libraries
library(tidytext)
library(dplyr)
library(ggplot2)
library(syuzhet)
# library(vader)
library(tidyverse)
library(lubridate)
library(purrr)


# Load the samples 
load("data/data_processed/sampled_data_car.Rdata")
load("data/data_processed/sampled_data_energy.Rdata")

#Sentiment Analysis ----

# We will perform various sentiment analysis methods on our articles and compare the results

# AFINN Method uses a range of 5 and -5 so we will rescale to compare it with the other methods
# Define rescale function
# Define a function to rescale AFINN scores to a range of -1 to 1
rescale_afinn <- function(x) {
  (x + 5) / 10 * 2 - 1
}

compute_and_plot_sentiments <- function(sample_dataset, title_suffix) {
  # Add an id column and convert the first_publication_date column to a Date type
  sample_dataset <- cbind(id = seq_len(nrow(sample_dataset)), sample_dataset)
  sample_dataset$first_publication_date <- as.Date(sample_dataset$first_publication_date)
  
  # Calculate sentiment scores using the AFINN method
  afinn_scores <- sample_dataset %>%
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
  print(paste0("Starting Syuzhet....", title_suffix))
  syuzhet_scores <- sample_dataset %>%
    mutate(sentences = map(body_text, get_sentences)) %>%
    mutate(sent_score = map_dbl(sentences, ~ mean(get_sentiment(.x, method = "syuzhet")))) %>%
    select(-sentences)
  summary(syuzhet_scores$sent_score)
  
  # Compute the Bing method
  print(paste0("Starting Bing....", title_suffix))
  bing_scores <- sample_dataset %>%
    mutate(sentences = map(body_text, get_sentences)) %>%
    mutate(sent_score = map_dbl(sentences, ~ mean(get_sentiment(.x, method = "bing")))) %>%
    select(-sentences)
  summary(bing_scores$sent_score)
  
  # Compute sentiment scores using the nrc method
  # Takes Too Long
  # nrc_scores <- test_energy %>%
  #   mutate(sentences = map(body_text, get_sentences)) %>%
  #   mutate(sent_score = map_dbl(sentences, ~ mean(get_sentiment(.x, method = "nrc")))) %>%
  #   select(-sentences)
  # summary(nrc_scores$sent_score)
  
  #Test if the 4th method works with one article only
  # test <- test_energy$body_text[1]
  # test_sent <- get_sentences(test)
  # nrc_vector <- get_sentiment(test_sent, method="nrc")
  # mean(nrc_vector)
  # as.numeric(get_vader(text = test_sent, rm_qm = TRUE)["compound"])
  
  
  #Vader Sentiment Analysis
  # TAKES TOO LONG
  # vader_scores <- test_energy %>%
  #   mutate(sent_score = map_dbl(body_text, ~ as.numeric(get_vader(text = .x, rm_qm = TRUE)["compound"])))
  
  
  
  
  # Add sentiment scores to the original data frame
  sentiments_model <- sample_dataset %>%
    left_join(afinn_scores %>% select(id, afinn_score = sent_score), by = "id") %>%
    left_join(syuzhet_scores %>% select(id, syuzhet_score = sent_score), by = "id") %>%
    left_join(bing_scores %>% select(id, bing_score = sent_score), by = "id") #%>%
    # left_join(vader_scores %>% select(id, vader_score = sent_score), by = "id")
  
  
  # Reshape the data for plotting
  sentiments_model_long <- sentiments_model %>%
    select(first_publication_date, afinn_score, syuzhet_score, bing_score) %>%
    pivot_longer(cols = c(afinn_score, syuzhet_score, bing_score),
                 names_to = "model",
                 values_to = "score")
  
  # Create a plot that visualizes the sentiment scores over time
  plot_title <- paste0("Sentiment Scores Over Time", title_suffix)
  plot <- ggplot(sentiments_model_long, aes(x = first_publication_date, y = score, color = model)) +
    geom_smooth(se = FALSE) +
    labs(title = plot_title, x = "Date", y = "Sentiment Score") +
    theme_minimal()
  
  return(plot)
}

# Call the function with your data frame and title suffix
set.seed(123)
plot_car <- compute_and_plot_sentiments(sampled_data_car, " (Electric Car)")
plot_energy <- compute_and_plot_sentiments(sampled_data_energy, " (Clean Energy)")

# Get the y-axis limits for both plots
y_limits_car <- ggplot_build(plot_car)$layout$panel_params[[1]]$y.range
y_limits_energy <- ggplot_build(plot_energy)$layout$panel_params[[1]]$y.range

# Set the y-axis limits to be the same for both plots
y_limits <- range(c(y_limits_car, y_limits_energy))
plot_car <- plot_car + ylim(y_limits)
plot_energy <- plot_energy + ylim(y_limits)


# Arrange the plots in a grid with 2 columns
# grid_plot <- grid.arrange(plot_car, plot_energy,
#                           ncol = 2)

#Combine the plots with ggpubrs to have a common legend
grid_plot <- ggpubr::ggarrange(plot_car, plot_energy,
                               ncol = 2, common.legend = TRUE, legend="right")
grid_plot

# Save the grid plot to the output/plots/ folder in high quality
ggsave("output/plots/sentiment_over_time_combined_legend.jpg",
       grid_plot,
       width = 16,
       height = 9,
       dpi = 300)

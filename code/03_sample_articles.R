library(dplyr)
library(ggplot2)
library(gridExtra)


load('data/data_processed/results_energy_selected.rdata')
load('data/data_processed/results_car_selected.rdata')


plot_article_counts_and_sample <- function(data, output_file, suffix, sampled_data_name) {
  # Sample the data
  sampled_data <- data %>%
    mutate(month = format(first_publication_date, "%Y-%m")) %>%
    group_by(month) %>%
    group_modify(~ slice_sample(.x, n = min(10, nrow(.x)))) %>%
    ungroup() %>%
    select(-month)
  
  # Assign the sampled data to the specified variable name
  assign(sampled_data_name, sampled_data, envir = .GlobalEnv)
  
  # Save the sampled data to an .Rdata file
  save(list = sampled_data_name, file = paste0("data/data_processed/", sampled_data_name, ".Rdata"), envir = .GlobalEnv)
  
  # Create a summary data frame for the original data
  original_summary <- data %>%
    mutate(month = format(first_publication_date, "%Y-%m")) %>%
    count(month)
  
  # Create a summary data frame for the sampled data
  sampled_summary <- get(sampled_data_name) %>%
    mutate(month = format(first_publication_date, "%Y-%m")) %>%
    count(month)
  
  # Add the suffix to the names of the data frames
  original_summary_name <- paste0("original_summary", suffix)
  assign(original_summary_name, original_summary)
  
  sampled_summary_name <- paste0("sampled_summary", suffix)
  assign(sampled_summary_name, sampled_summary)
  
  # Create a bar plot for the original data
  original_plot_name <- paste0("original_plot", suffix)
  original_plot <- ggplot(original_summary, aes(x = month, y = n)) +
    geom_col(fill = "skyblue") +
    labs(title = "Number of Articles in Each Month (Original Data)", x = "Month", y = "Number of Articles") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  assign(original_plot_name, original_plot)
  
  # Create a bar plot for the sampled data
  sampled_plot_name <- paste0("sampled_plot", suffix)
  sampled_plot <- ggplot(sampled_summary, aes(x = month, y = n)) +
    geom_col(fill = "skyblue") +
    labs(title = "Number of Articles in Each Month (Sampled Data)", x = "Month", y = "Number of Articles") +
    theme(axis.text.x = element_text(angle = 90, hjust = 1))
  assign(sampled_plot_name, sampled_plot)
  
  # Arrange the plots in a grid
  grid_name <- paste0("grid", suffix)
  grid <- grid.arrange(original_plot, sampled_plot, ncol = 2)
  assign(grid_name, grid)
  
  # Save the grid to a file
  ggsave(output_file, grid, width = 16, height = 9)
  
  # Remove old data frames and plots from environment
  rm(list=ls(pattern="original_summary"))
  rm(list=ls(pattern="sampled_summary"))
  rm(list=ls(pattern="original_plot"))
  rm(list=ls(pattern="sampled_plot"))
  rm(list=ls(pattern="grid"))
}




# Call the function with your data frame and output file path
plot_article_counts_and_sample(results_car_selected, "output/plots/article_counts_car.png", "_car",
                               'sampled_data_car')
plot_article_counts_and_sample(results_energy_selected, "output/plots/article_counts_energy.png", "_energy",
                               'sampled_data_energy')




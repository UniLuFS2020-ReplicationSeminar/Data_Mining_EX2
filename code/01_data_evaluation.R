#Script to evaluate the data

library(dplyr)
library(ggplot2)

#Load datasets

results_car <- load("results_car_selected.Rdata")
results_energy <- load("results_energy_selected.Rdata")
                    
# Count occurrences of "better future" in body_text column
keyword <- "sustainable"
results_energy_selected$count <- sapply(strsplit(tolower(results_energy_selected$body_text), "\\W"), function(x) sum(keyword %in% x))

# Create a bar graph to visualize the results
plot1 <- ggplot(results_energy_selected, aes(x = factor(count), fill = factor(count))) +
  geom_bar() +
  scale_fill_discrete(name = "Count of 'sustainable'") +
  labs(title = "Occurrences of 'sustainable' in body text",
       x = "Count",
       y = "Frequency")

print(plot1)

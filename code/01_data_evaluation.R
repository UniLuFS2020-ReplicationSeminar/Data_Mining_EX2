#Script to evaluate the data

library(dplyr)
library(ggplot2)

#Load datasets

results_car <- load("results_car_selected.Rdata")
results_energy <- load("results_energy_selected.Rdata")
                    
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


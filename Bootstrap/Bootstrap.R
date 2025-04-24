# Load necessary packages
library(tidyverse)

# Load your data
catan_data <- read_csv("./data/catanstats.csv")

# View the structure
#glimpse(catan_data)

# Add a column identifying game number (assuming 4 players per game)
catan_data <- catan_data %>%
  mutate(game_id = rep(1:(n()/4), each = 4))

# Mark the winner in each game
winners <- catan_data %>%
  group_by(game_id) %>%
  mutate(winner = ifelse(points == max(points), 1, 0)) %>%
  ungroup()

### Bootstrapping, simulating winners sample ###
set.seed(42)

n_iterations <- 1000 #10000

bootstrap_results <- map_dfr(1:n_iterations, function(i) {
  sample_winners <- winners %>%
    sample_frac(replace = TRUE)
  sample_winners %>%
    select('1settlersc1', '1settlersc2', '1settlersc3') %>%
    pivot_longer(cols = everything(), names_to = "slot", values_to = "resource") %>%
    count(resource) %>%
    mutate(iteration = i)
})

# Summarizing the data#
summary <- bootstrap_results %>%
  group_by(resource) %>%
  summarise(mean_count = mean(n), .groups = "drop") %>%
  arrange(desc(mean_count))

print(summary)

###Key Takeaways: 
# Wood, Ore, and Sheep seem to be the most important resources for starting a winning settlement, as they have the highest mean counts.
# Resources like Clay, Desert, and combinations like 2 Sheep (2S) or 3 Gold (3G) seem to be less important.
# If your strategy is to pick starting resources that lead to the most wins, focusing on Wood (W) and Ore (O), followed by Sheep (S) and Wheat (L) might be a strong choice.

###Incorperating wieght of Token count (# associated with the resource)
library(ggplot2)

# Create the plot
ggplot(summary, aes(x = reorder(resource, -mean_count), y = mean_count)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(
    title = "Average Frequency of Resources in Winning First Settlements",
    x = "Resource",
    y = "Mean Count",
    caption = "Based on 10,000 bootstrap iterations"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) 

#Creating a weighting function

# Assuming each resource is tied to a specific number token in your dataset
resource_weights <- tibble(
  resource = c("W", "O", "S", "L", "C", "2S", "D", "3G", "2L", "2W"),
  token = c(6, 8, 5, 10, 3, 12, 2, 11, 4, 9)
)

# Frequency weights based on the number token roll probabilities
number_weights <- tibble(
  token = c(6, 8, 5, 9, 4, 10, 3, 11, 2, 12),
  weight = c(5/36, 5/36, 4/36, 4/36, 3/36, 3/36, 2/36, 2/36, 1/36, 1/36)
)

# Join the resource weights with the number weights
# First, ensure the summary table contains 'resource' and 'mean_count'
weighted_resources <- summary %>%
  left_join(resource_weights, by = "resource") %>%
  left_join(number_weights, by = "token") %>%
  mutate(weighted_count = mean_count * weight)

# Check the result
weighted_resources

#Visualizing weighted counts#
# Plot the weighted resources
ggplot(weighted_resources, aes(x = reorder(resource, -weighted_count), y = weighted_count)) +
  geom_bar(stat = "identity", fill = "lightgreen", color = "black") +
  labs(
    title = "Weighted Average Frequency of Resources in Winning First Settlements",
    x = "Resource",
    y = "Weighted Mean Count",
    caption = "Weights based on Catan number token roll probabilities"
  ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

#interpreting#
# Higher weighted mean counts indicate that a resource is not only common in winning first settlements but also tied to highly valuable tiles that are rolled frequently in the game (like 6 or 8).
# Lower weighted mean counts indicate that a resource may appear in settlements but is associated with less frequent tiles (like 2 or 12), or it might just not be as influential in winning settlements.







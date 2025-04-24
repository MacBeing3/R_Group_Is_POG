CatanData$winloss <- NA
CatanData$winloss[CatanData$points <= 9] <- "Loss"
CatanData$winloss[CatanData$points >= 10] <- "Win"

library(tidyverse)

library(ggplot2)

##win rate by player turn 

wins_only <- CatanData %>%
  filter(winloss == "Win")

win_rate_player <- CatanData %>%
  group_by(player) %>%
  summarise(
    total = n(),
    wins = sum(winloss == "Win"),
    win_rate = wins / total
  )

ggplot(win_rate_player, aes(x = factor(player), y = win_rate, fill = factor(player))) +
  geom_col() +
  labs(title = "Win Rate by Turn Position",
       x = "Turn Position (Player)",
       y = "Win Rate",
       fill = "Player") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

##win rate by color 
win_rate_color <- CatanData %>%
  group_by(color) %>%
  summarise(
    total = n(),
    wins = sum(winloss == "Win"),
    win_rate = wins / total
  )

ggplot(win_rate_color, aes(x = color, y = win_rate, fill = color)) +
  geom_col() +  # Use 'geom_col' to create a bar plot
  scale_fill_manual(values = c("royalblue2", "darkgoldenrod1", "brown2", "snow2")) +  # Assign custom colors to each bar
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Win Rate by Color",
       x = "Color",
       y = "Win Rate") +
  theme_minimal()

##win rate resource
Catan_long <- CatanData %>%
  pivot_longer(
    cols = matches("settlenum|settlersc"),
    names_to = c("settlement", ".value"),
    names_pattern = "(\\dsettle)(num\\d|rsc\\d)"
  ) %>%
  rename(number = num1, resource = rsc1)  # fix names for consistency

Catan_long <- Catan_long %>%
  mutate(
    number = as.numeric(number),
    resource = str_trim(resource),
    resource = ifelse(str_detect(resource, "^[A-Z]$"), resource, NA)
  ) %>%
  drop_na(number, resource)

win_rate_resource <- Catan_long %>%
  group_by(resource) %>%
  summarise(
    total = n(),
    wins = sum(winloss == "Win"),
    win_rate = wins / total
  )

label_map <- c("L" = "Lumber", "C" = "Clay", "S" = "Sheep", "W" = "Wheat", "O" = "Ore")

# Generate the plot
ggplot(win_rate_resource, aes(x = resource, y = win_rate, fill = resource)) +
  geom_col() +
  scale_fill_manual(values = c("L" = "forestgreen",    # Lumber (L) = Green
                               "C" = "chocolate",   # Clay (C) = Brown
                               "S" = "darkolivegreen2",       # Sheep (S) = Blue
                               "W" = "goldenrod1",     # Wheat (W) = Yellow
                               "O" = "cadetblue4")) +       # Ore (O) = Gray
  scale_y_continuous(labels = scales::percent_format()) +
  scale_x_discrete(labels = label_map) +  # Replace abbreviations with full names
  labs(title = "Win Rate by Starting Resource",
       x = "Resource",
       y = "Win Rate") +
  theme_minimal()




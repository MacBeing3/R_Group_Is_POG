CatanData$winloss <- NA
CatanData$winloss[CatanData$points <= 9] <- "Loss"
CatanData$winloss[CatanData$points >= 10] <- "Win"

library(tidyverse)

library(ggplot2)

##Win rate by player turn 
##Group data by player turn order
##Calculate the total games played by each player
##Calculate the number of wins
##Compute the win rate for each turn position
win_rate_player <- CatanData %>%
  group_by(player) %>%
  summarise(
    total = n(),
    wins = sum(winloss == "Win"),
    win_rate = wins / total
  )
##Create graph
ggplot(win_rate_player, aes(x = factor(player), y = win_rate, fill = factor(player))) +
  geom_col() +
  labs(title = "Win Rate by Turn Position",
       x = "Turn Position (Player)",
       y = "Win Rate",
       fill = "Player") +
  scale_y_continuous(labels = scales::percent) +
  theme_minimal()

##Win rate by color 
##Calculate win rate by player color
win_rate_color <- CatanData %>%
  group_by(color) %>%
  summarise(
    total = n(),
    wins = sum(winloss == "Win"),
    win_rate = wins / total
  )
##Create Graph
ggplot(win_rate_color, aes(x = color, y = win_rate, fill = color)) +
  geom_col() +  
  scale_fill_manual(values = c("royalblue2", "darkgoldenrod1", "brown2", "snow2")) +  # Assign custom colors to each bar
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Win Rate by Color",
       x = "Color",
       y = "Win Rate") +
  theme_minimal()

##Win rate resource
##Convert wide format (many settlenumX/settlerscX columns) to long format
##Each row becomes one settlement's number and resource
##Rename columns for clarity
Catan_long <- CatanData %>%
  pivot_longer(
    cols = matches("settlenum|settlersc"),
    names_to = c("settlement", ".value"),
    names_pattern = "(\\dsettle)(num\\d|rsc\\d)"
  ) %>%
  rename(number = num1, resource = rsc1)  # fix names for consistency
##Clean/filter data
##Convert to numeric
Catan_long <- Catan_long %>%
  mutate(
    number = as.numeric(number),
    resource = str_trim(resource),
    resource = ifelse(str_detect(resource, "^[A-Z]$"), resource, NA)
  ) %>%
  drop_na(number, resource)
##Calculate win rate based on each resource type seen in starting settlements
win_rate_resource <- Catan_long %>%
  group_by(resource) %>%
  summarise(
    total = n(),
    wins = sum(winloss == "Win"),
    win_rate = wins / total
  )
##Map single-letter resource codes to full names
label_map <- c("L" = "Lumber", "C" = "Clay", "S" = "Sheep", "W" = "Wheat", "O" = "Ore")
##Create Graph
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




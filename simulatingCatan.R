# Catan Strategy Predictor: Mock Data + Random Forest Model
# ---------------------------------------------------------

# Load libraries
library(dplyr)
library(caret)
library(randomForest)

# Set seed for reproducibility
set.seed(123)

# ----- 1. Simulate Mock Catan Data -----

# Simulate 200 player-game entries (4 players x 50 games)
n <- 200
catan_data <- data.frame(
  player_id = rep(1:4, times = 50),
  game_id = rep(1:50, each = 4),
  turn_order = rep(1:4, times = 50),
  
  tile1_num = sample(c(2:12), n, replace = TRUE),
  tile1_res = sample(c("wood", "brick", "wheat", "sheep", "ore"), n, replace = TRUE),
  tile2_num = sample(c(2:12), n, replace = TRUE),
  tile2_res = sample(c("wood", "brick", "wheat", "sheep", "ore"), n, replace = TRUE),
  tile3_num = sample(c(2:12), n, replace = TRUE),
  tile3_res = sample(c("wood", "brick", "wheat", "sheep", "ore"), n, replace = TRUE),
  
  final_points = round(runif(n, 5, 10), 1)
)

# Assign 1 winner per game
winners <- sample(1:4, 50, replace = TRUE)
catan_data$win <- catan_data$turn_order == rep(winners, each = 4)

# ----- 2. Feature Engineering -----

# Dice number to probability lookup
prob_lookup <- c(
  "2" = 1/36, "3" = 2/36, "4" = 3/36, "5" = 4/36,
  "6" = 5/36, "7" = 0, "8" = 5/36, "9" = 4/36,
  "10" = 3/36, "11" = 2/36, "12" = 1/36
)

# Calculate average probability and other features
catan_data <- catan_data %>%
  rowwise() %>%
  mutate(
    avg_prob = sum(
      prob_lookup[as.character(tile1_num)],
      prob_lookup[as.character(tile2_num)],
      prob_lookup[as.character(tile3_num)]
    ),
    resource_diversity = n_distinct(c(tile1_res, tile2_res, tile3_res)),
    has_wood = as.integer("wood" %in% c(tile1_res, tile2_res, tile3_res)),
    has_brick = as.integer("brick" %in% c(tile1_res, tile2_res, tile3_res))
  ) %>%
  ungroup()

# ----- 3. Train/Test Split -----

set.seed(42)
train_index <- createDataPartition(catan_data$win, p = 0.8, list = FALSE)
train_data <- catan_data[train_index, ]
test_data <- catan_data[-train_index, ]

# ----- 4. Train Random Forest Model -----


# TO THIS:
train_data$win <- as.factor(train_data$win)
test_data$win <- as.factor(test_data$win)

rf_model <- randomForest(
  win ~ avg_prob + resource_diversity + has_wood + has_brick + turn_order,
  data = train_data,
  importance = TRUE
)


# Predict on test set
pred_probs <- predict(rf_model, newdata = test_data, type = "prob")[, "TRUE"]
test_data$predicted_prob <- pred_probs


# ----- 5. Results & Plots -----

# View predictions
print(head(test_data %>% select(player_id, game_id, win, predicted_prob)))
## look at this ^^^^^^^

# Show feature importance
print(importance(rf_model))
varImpPlot(rf_model)

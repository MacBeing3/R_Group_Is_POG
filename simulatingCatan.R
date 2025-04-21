# Catan Strategy Predictor: Mock Data + Random Forest Model
# ---------------------------------------------------------

# Load libraries
library(dplyr)
library(caret)
library(randomForest)

# Set seed for reproducibility
set.seed(123)

# ----- 1. Simulate Mock Catan Data -----

CatanData <- read.csv(file = 'data/catanstats.csv', header = TRUE)

# Creating object that has all of the desired colors
colors <- c("red", "blue", "white", "orange")

# Creating new variable for color
# Split separates the observations by game Num, 50 groups for 50 games
# lapply does the random sample of colors for each group within each group
# This way, each color is found in each game without repeats within games
# Unlist separates data back into individual observations
CatanData$color <- unlist(
  lapply(split(CatanData, CatanData$gameNum), function(group)
    sample(colors))
)

CatanData[c(
  "X1settlersc1", 
  "X1settlersc2", 
  "X1settlersc3",
  "X2settlersc1", 
  "X2settlersc2", 
  "X2settlersc3"
)] <- lapply(CatanData[c(
  "X1settlersc1", 
  "X1settlersc2", 
  "X1settlersc3",
  "X2settlersc1", 
  "X2settlersc2", 
  "X2settlersc3"
)], function(col) {
  replace(col, is.na(col), "A")
})



input_game <- CatanData #will be replaced when turn into function

# Simulate 200 player-game entries (4 players x 50 games)
n <- 200

#subset of data for analysis


# Assign 1 winner per game

#losing is the default state
input_game$win <- rep(FALSE,n)

for(game in 1:50){
  
  maybe_win <- input_game$player[input_game$points >= 10 & input_game$gameNum == game]
  
  win_player <- min(maybe_win)
  
  input_game$win[((game-1)*4) +win_player] <- TRUE
  
}

input_game$player[input_game$win == TRUE]


# ----- 2. Feature Engineering -----

# Dice number to probability lookup
prob_lookup <- c(
  "2" = 1/36, "3" = 2/36, "4" = 3/36, "5" = 4/36,
  "6" = 5/36, "7" = 0, "8" = 5/36, "9" = 4/36,
  "10" = 3/36, "11" = 2/36, "12" = 1/36
)

# Calculate average probability and other features
input_game <- input_game %>%
  rowwise() %>%
  mutate( #issue is that number is sometimes zero in the third one
    avg_prob = sum(
      prob_lookup[as.character(X1settlenum1)],
      prob_lookup[as.character(X1settlenum2)],
      prob_lookup[as.character(X1settlenum3)],
      
      prob_lookup[as.character(X2settlenum1)],
      prob_lookup[as.character(X2settlenum2)],
      prob_lookup[as.character(X2settlenum3)]
    ),
    resource_diversity = n_distinct(c(
      X1settlersc1, 
      X1settlersc2, 
      X1settlersc3,
      X2settlersc1, 
      X2settlersc2, 
      X2settlersc3)),
    
    has_wood = as.integer("L" %in% c(
      X1settlersc1, 
      X1settlersc2, 
      X1settlersc3,
      X2settlersc1, 
      X2settlersc2, 
      X2settlersc3)),
    
    has_brick = as.integer("C" %in% c(
      X1settlersc1, 
      X1settlersc2, 
      X1settlersc3,
      X2settlersc1, 
      X2settlersc2, 
      X2settlersc3))
  ) %>%
  ungroup()

# ----- 3. Train/Test Split -----
simplified_input <- input_game[-c(4:15,28:36)]


set.seed(42)
train_index <- createDataPartition(simplified_input$win, p = 0.8, list = FALSE)
train_data <- simplified_input[train_index, ]
test_data <- simplified_input[-train_index, ]

# ----- 4. Train Random Forest Model -----


# TO THIS:
train_data$win <- as.factor(train_data$win)
test_data$win <- as.factor(test_data$win)

rf_model <- randomForest(
  win ~ avg_prob + resource_diversity + has_wood + has_brick + player,
  data = train_data,
  importance = TRUE
)


# Predict on test set
pred_probs <- predict(rf_model, newdata = test_data, type = "prob")[, "TRUE"]
test_data$predicted_prob <- pred_probs


# ----- 5. Results & Plots -----

# View predictions
print(head(test_data %>% select(player, gameNum, win, predicted_prob)))
## look at this ^^^^^^^

# Show feature importance
print(importance(rf_model))
varImpPlot(rf_model)

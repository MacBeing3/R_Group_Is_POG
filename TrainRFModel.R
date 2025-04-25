#training RF_model


library(dplyr)
library(caret)
library(randomForest)




trainCatanModel <- function(full_data) {
  
  
  # Label winners per game
  full_data$win <- rep(FALSE, nrow(full_data))
  for (game in unique(full_data$gameNum)) {
    maybe_win <- full_data$player[full_data$points >= 10 & full_data$gameNum == game]
    if (length(maybe_win) > 0) {
      full_data$win[((game - 1) * 4) + min(maybe_win)] <- TRUE
    }
  }
  
  # Dice prob lookup based on 2 dice rolling
  prob_lookup <- c("0"=0, "2"=1/36, "3"=2/36, "4"=3/36, "5"=4/36,
                   "6"=5/36, "7"=0, "8"=5/36, "9"=4/36,
                   "10"=3/36, "11"=2/36, "12"=1/36)
  
  # preparing our sub info 
  #     (data made from other data)
  full_data <- full_data %>%
    rowwise() %>%
    mutate(
      avg_prob = sum( #calcs avg probability of both tiles
        prob_lookup[as.character(X1settlenum1)],
        prob_lookup[as.character(X1settlenum2)],
        prob_lookup[as.character(X1settlenum3)],
        prob_lookup[as.character(X2settlenum1)],
        prob_lookup[as.character(X2settlenum2)],
        prob_lookup[as.character(X2settlenum3)]
      ), #calcs the resource diversity (has different resources)
      resource_diversity = n_distinct(c(
        X1settlersc1, X1settlersc2, X1settlersc3,
        X2settlersc1, X2settlersc2, X2settlersc3)),
      has_wood = as.integer("L" %in% c( # if has lumber -> TRUE
        X1settlersc1, X1settlersc2, X1settlersc3,
        X2settlersc1, X2settlersc2, X2settlersc3)),
      has_brick = as.integer("C" %in% c( #if has clay -> True
        X1settlersc1, X1settlersc2, X1settlersc3,
        X2settlersc1, X2settlersc2, X2settlersc3))
    ) %>%
    ungroup()
  
  #getting rid of factors we aren't tracking
  simplified_data <- full_data %>%
    select(win, avg_prob, resource_diversity, has_wood, has_brick, player)
  
  # Train model
  set.seed(42)
  simplified_data$win <- as.factor(simplified_data$win)
  model <- randomForest(
    win ~ avg_prob + resource_diversity + has_wood + has_brick + player,
    data = simplified_data
  )
  
  return(model)
}

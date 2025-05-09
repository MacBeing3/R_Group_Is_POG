#Predictor functions

library(dplyr)
library(caret)
library(randomForest)

#sourcing the train model function
#separating for readability in RMarkdown

source('TrainRFModel.R',local = knitr::knit_global())

predictCatan <- function(gameData, model) {
  prob_lookup <- c("0"=0, "2"=1/36, "3"=2/36, "4"=3/36, "5"=4/36,
                   "6"=5/36, "7"=0, "8"=5/36, "9"=4/36,
                   "10"=3/36, "11"=2/36, "12"=1/36)
  
  # Feature engineering
  processed_data <- gameData %>%
    rowwise() %>%
    mutate(
      avg_prob = sum(
        prob_lookup[as.character(X1settlenum1)],
        prob_lookup[as.character(X1settlenum2)],
        prob_lookup[as.character(X1settlenum3)],
        prob_lookup[as.character(X2settlenum1)],
        prob_lookup[as.character(X2settlenum2)],
        prob_lookup[as.character(X2settlenum3)]
      ),
      resource_diversity = n_distinct(c(
        X1settlersc1, X1settlersc2, X1settlersc3,
        X2settlersc1, X2settlersc2, X2settlersc3)),
      has_wood = as.integer("L" %in% c(
        X1settlersc1, X1settlersc2, X1settlersc3,
        X2settlersc1, X2settlersc2, X2settlersc3)),
      has_brick = as.integer("C" %in% c(
        X1settlersc1, X1settlersc2, X1settlersc3,
        X2settlersc1, X2settlersc2, X2settlersc3))
    ) %>%
    ungroup()
  
  # Keep only model-relevant columns
  pred_input <- processed_data %>%
    select(avg_prob, resource_diversity, has_wood, has_brick, player)
  
  
  # Predict
  probs <- predict(model, newdata = pred_input, type = "prob")[, "TRUE"]
  #return(probs)
}








predictCatanPlayer <- function(new_row, model) {
  prob_lookup <- c("0"=0, "2"=1/36, "3"=2/36, "4"=3/36, "5"=4/36,
                   "6"=5/36, "7"=0, "8"=5/36, "9"=4/36,
                   "10"=3/36, "11"=2/36, "12"=1/36)
  
  new_row <- new_row %>%
    mutate(
      avg_prob = sum(
        prob_lookup[as.character(X1settlenum1)],
        prob_lookup[as.character(X1settlenum2)],
        prob_lookup[as.character(X1settlenum3)],
        prob_lookup[as.character(X2settlenum1)],
        prob_lookup[as.character(X2settlenum2)],
        prob_lookup[as.character(X2settlenum3)]
      ),
      resource_diversity = n_distinct(c(
        X1settlersc1, X1settlersc2, X1settlersc3,
        X2settlersc1, X2settlersc2, X2settlersc3)),
      has_wood = as.integer("L" %in% c(
        X1settlersc1, X1settlersc2, X1settlersc3,
        X2settlersc1, X2settlersc2, X2settlersc3)),
      has_brick = as.integer("C" %in% c(
        X1settlersc1, X1settlersc2, X1settlersc3,
        X2settlersc1, X2settlersc2, X2settlersc3))
    )
  
  pred_input <- new_row %>%
    select(avg_prob, resource_diversity, has_wood, has_brick, player)
  
  prob <- predict(model, newdata = pred_input, type = "prob")[, "TRUE"]
  
  return(prob)
}



# Compare row 2
#cat("Row 2 via predictCatan = ", all_probs[2], "\n")
#cat("Row 2 via predictCatanPlayer = ", predictCatanPlayer(CatanData[2, ], rf_model), "\n")





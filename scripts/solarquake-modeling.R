library(tidyverse)
library(caret)
load("data/solarquakes.rdata")

# Partition data for training and testing
set.seed(2002, sample.kind = "Rounding")
test_index <- createDataPartition(y = solarquakes$quake.count, times = 1, p = 0.2, list = FALSE)
trainSet <- solarquakes[-test_index,]
testSet <- solarquakes[test_index,]


# Create 'RMSE' function for comparing model success
RMSE <- function(actual_ratings, predicted_ratings){
  sqrt(mean((actual_ratings - predicted_ratings)^2))
}


##### INITIAL MODEL #####
# Calculate "Just the Average" as prediction
just_avg_rmse <- RMSE(testSet$quake.count, mean(trainSet$quake.count))


##### LINEAR MODEL #####
# Train and test linear model for predicting quake.count from flare.count
train_lm <- train(quake.count ~ flare.count, data = trainSet, method = "lm")
preds_lm <- testSet %>% mutate(pred.lm = predict(train_lm, newdata = testSet)) %>%
  pull(pred.lm)
linear_rmse <- RMSE(testSet$quake.count, preds_lm)


##### KNN MODEL #####
# Train and test kNN model
control <- trainControl(method = "cv", number = 10, p = .9)
train_knn <- train(quake.count ~ flare.count, data = trainSet, method = "knn", 
                   tuneGrid = data.frame(k = seq(9, 101, 2)),
                   trControl = control)
ggplot(train_knn, highlight = TRUE)
train_knn$finalModel
train_knn$bestTune

preds_knn <- testSet %>% mutate(pred.knn = predict(train_knn, newdata = testSet)) %>%
  pull(pred.knn)
knn_rmse <- RMSE(testSet$quake.count, preds_knn)


##### RANDOM FOREST MODEL #####
# Train and test random forest model
grid <- data.frame(mtry = c(1, 5, 10, 25, 50, 100))
train_rf <- train(quake.count ~ flare.count, data = trainSet, method = "rf", 
                  tuneGrid = grid,
                  trControl = control)
train_rf$finalModel
train_rf$bestTune

preds_rf <- testSet %>% mutate(pred.rf = predict(train_rf, newdata = testSet)) %>%
  pull(pred.rf)
rf_rmse <- RMSE(testSet$quake.count, preds_rf)


##### MODEL PERFORMANCE #####
models <- tibble(Model = c("Just the Average", 
                           "Linear Model", 
                           "kNN Model",
                           "Random Forest Model"), 
                 RMSE = c(just_avg_rmse, linear_rmse, knn_rmse, rf_rmse)
)
save(models, file = "data/models.RData")

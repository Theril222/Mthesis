library(xgboost)
library(Matrix)
library(caret)
library(dplyr)



testnum <- df4[,c('duration','AOIFCAF', 'AOIFCModell', 'AOIFCTotal', 'fixcount', 'gapcount', 'saccadecount'  ,'Abgeschlossen', 'outcome')  ] 



# Convert to matrix
data_matrix <- as.matrix(testnum[,1:8])
label <- as.factor(testnum$outcome)   # XGBoost requires labels to be numeric (0-based)


# Training and test sets
set.seed(128)
trainIndex <- createDataPartition(label, p = 0.7, list = FALSE)
train_data2 <- data_matrix[trainIndex, ]
train_label2 <- label[trainIndex]
test_data2 <- data_matrix[-trainIndex, ]
test_label2 <- label[-trainIndex]


# Define training control 
train_control5 <- trainControl(method = "cv", number = 5, 
                              verboseIter = FALSE)

# Define a grid of hyperparameters to tune
tune_grid5 <- expand.grid(
  nrounds = c(50, 100, 150),
  max_depth = c(3, 50,5, 9),
  eta = c(0.01, 0.1, 0.3),
  gamma = c(0, 1),
  colsample_bytree = c(0.5, 0.8),
  min_child_weight = c(5, 10, 20),
  subsample = c(0.5, 0.8)     # samples used per boosting round
)

# Train the XGBoost model 
xgb_model <- train(
  x = train_data2,
  y = train_label2,
  method = "xgbTree",
  trControl = train_control5,
  tuneGrid = tune_grid5,
  verbose = FALSE
)

# Print the best hyperparameters
print(xgb_model$bestTune)


# Extract xgboost model
xgb_trained <- xgb_model$finalModel


# Plot feature importance
importance_matrix <- xgb.importance(model = xgb_trained)
xgb.plot.importance(importance_matrix)


# Print a summary of the cross-validation results
print(xgb_model)

# Make predictions
predictions <- predict(xgb_model, newdata = test_data2)


# Evaluate performance
conf_matrix <- confusionMatrix(predictions, test_label2)
print(conf_matrix)



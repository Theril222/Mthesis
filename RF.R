library(dplyr)
library(lubridate)
library(caret)
library(randomForest)


set.seed(126)

# Define training control
train_control3 <- trainControl(method = "cv", number = 5) # 5-fold cross-validation


# Define a grid of hyperparameters
tune_grid3 <- expand.grid(mtry = c(2, 3, 4)) # Number of variables randomly sampled as candidates at each split

# Train the model with cross-validation
rf_model <- train(outcome ~ ., data = train, method = "rf",
                  trControl = train_control3,
                  tuneGrid = tune_grid3,
                  ntree = 100) # Number of trees in the forest



# Show the best hyperparameters
print(rf_model$bestTune)

# Show the CV results
print(rf_model)

# Extract the best model
best_rf_model <- rf_model$finalModel

# variable importance
varImpPlot(best_rf_model)

# Make predictions on the test set
predictions <- predict(rf_model, newdata = test)

# Evaluate accuracy
confusionMatrix(predictions, t2)
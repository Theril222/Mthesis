library(caret)
library(gbm)
library(pdp)



set.seed(127)
# Define training control 
train_control4 <- trainControl(method = "cv", number = 5)

# Define a grid of hyperparameters to tune
tune_grid4 <- expand.grid(n.trees = c(50, 100, 150),
                         interaction.depth = c(1, 3, 5),
                         shrinkage = c(0.01, 0.1),
                         n.minobsinnode = c(10, 20))


# Train the GBM model
gbm_model <- train(outcome ~ ., data = train, method = "gbm",
                   trControl = train_control4,
                   tuneGrid = tune_grid4,
                   verbose = FALSE)


# Show best parameters
print(gbm_model$bestTune)

# Show Summary of CVs
print(gbm_model)



# Make predictions using the trained model
predictions <- predict(gbm_model, newdata = test)

# Evaluate model accuracy
confusionMatrix(predictions, t2)

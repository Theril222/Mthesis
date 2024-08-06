library(catboost)
library(caret)



label2 <- as.factor(testnum$outcome)
numeric <- as.numeric(label2) - 1
data_matrix2 <- as.matrix(testnum[,1:6])

# Test and Training Data
set.seed(129)
trainIndex <- createDataPartition(label, p = 0.7, list = FALSE)
train_data3 <- data_matrix2[trainIndex, ]
train_label3 <- numeric[trainIndex]
test_data3 <- data_matrix2[-trainIndex, ]
test_label3 <- numeric[-trainIndex]

# Create Pool objects
train_pool <- catboost.load_pool(data = train_data3, label = train_label3)
test_pool <- catboost.load_pool(data = test_data3, label = test_label3)


# Parameter grid 
param_grid <- list(
  learning_rate = c(0.01, 0.1, 0.3),
  depth = c(3, 6, 9),
  iterations = c(50, 100, 150),
  l2_leaf_reg = c(1, 3, 5),
  border_count = c(32, 64),
  bootstrap_type = 'Bernoulli' 
)

# Store the best results
best_score <- 0
best_params <- list()

# Loop over all combinations
for (learning_rate in param_grid$learning_rate) {
  for (depth in param_grid$depth) {
    for (iterations in param_grid$iterations) {
      for (l2_leaf_reg in param_grid$l2_leaf_reg) {
        for (border_count in param_grid$border_count) {
          for (bootstrap_type in param_grid$bootstrap_type) {
            
            # Handle parameters based on bootstrap type
            params <- list(
              iterations = iterations,
              depth = depth,
              learning_rate = learning_rate,
              l2_leaf_reg = l2_leaf_reg,
              border_count = border_count,
              bootstrap_type = bootstrap_type,
              loss_function = 'MultiClass',
              custom_metric = list('MultiClass')
            )
            
            # Include subsample for Bernoulli
            if (bootstrap_type == 'Bernoulli') {
              params$subsample <- runif(1, 0.8, 1.0)  # SubsampleValue between 0.8 and 1.0
            }
            
            # Train model
            model <- catboost.train(train_pool, params = params)
            
            # Make predictions
            predictions <- catboost.predict(model, test_pool, prediction_type = 'Class')
            
            # Calculate accuracy
            accuracy <- mean(predictions == test_label3)
            
            # Best score so far
            if (accuracy > best_score) {
              best_score <- accuracy
              best_params <- params
            }
            
          }
        }
      }
    }
  }
}

# Best parameters
print("Best Parameters:")
print(best_params)


# Train final model
final_model <- catboost.train(train_pool, params = best_params)

# Prediction
final_predictions <- catboost.predict(final_model, test_pool, prediction_type = 'Class')


# Convert predictions to class labels
final_predictions_class <- as.factor(final_predictions)


# Evaluate accuracy
conf_matrix <- confusionMatrix(final_predictions_class, as.factor(test_label3))
print(conf_matrix)
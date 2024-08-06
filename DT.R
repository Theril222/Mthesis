library(dplyr)
library(e1071)
library(rpart)
library(rpart.plot)
library(caret)
library(C50)



set.seed(124)
# CV
train_control = trainControl(method = "cv", number = 5, search = "grid")

## Tuning Grid Customizition
multi_classification_Tree_Grid =  expand.grid(maxdepth = c(1,3,5,7,9))



# training a Regression model 
model = train(outcome~., data = train, method = "rpart2", trControl = train_control, tuneGrid = multi_classification_Tree_Grid)
print(model)

model2 <- rpart(outcome ~., data = train, maxdepth = 3, method = 'class')
rpart.plot(model2)

# make predictions on test data
pred_y = predict(model2, test, type = 'class')


# Evaluate accuracy
confusionMatrix(data = factor(pred_y), reference =t2)


#C50


set.seed(125)
tune_grid <- expand.grid(trials = c(1, 5, 10),          # Number of boosting iterations
                         model = c("tree"),   # DT
                         winnow = c(TRUE, FALSE))      # Attribute winnowing


# Define train control with CV
train_control2 <- trainControl(method = "cv", number = 5) 


# Train the model 
c5_model <- train(outcome ~ ., data = train, method = "C5.0",
                  trControl = train_control2,
                  tuneGrid = tune_grid)


# Show the best parameter
print(c5_model$bestTune)

tree <- C5.0(outcome~., data = train, model = 'tree', winnow = TRUE, trials = 5)

plot(tree)
predictions <- predict(tree, newdata = test)

# Evaluate accuracy
confusionMatrix(predictions, t2)




Classes <- unique(test$outcome)

model2 <- rpart(outcome ~., data = train)
prp(model2, extra = 1)

# specifying the CV technique which will be passed into the train() function later and number parameter is the "k" in K-fold cross validation
train_control = trainControl(method = "cv", number = 3, search = "grid")

## Customsing the tuning grid (ridge regression has alpha = 0)
multi_classification_Tree_Grid =  expand.grid(maxdepth = c(1,3,5,7,9))



# training a Regression model while tuning parameters (Method = "rpart")
model = train(outcome~., data = train, method = "rpart2", trControl = train_control, tuneGrid = multi_classification_Tree_Grid)



#use model to make predictions on test data
pred_y = predict(model, test)
pred_x = predict(model2, test, type = 'class')

print('---------------------------------------------------------')
print('Evaluation of decision tree with rpart:')
confusionMatrix(pred_x, t2)

print('---------------------------------------------------------')
print('Evaluation of decision tree with rpart (Regression-Model):')
# confusion Matrix
confusionMatrix(data = pred_y, reference =t2)

target = outcome ~ duration + AOIFCAF  + AOIFCModell  + AOIFCTotal + fixcount  + Abgeschlossen 

tree = rpart(target, data = train, method = "class")
rpart.plot(tree)

predictions = predict(tree, test, type = 'class')

print(predictions)
print('---------------------------------------------------------')
print('Evaluation of decision tree with rpart tuned variables:')
confusionMatrix(factor(predictions), t2)


tree = ctree(outcome ~ ., data = train)
plot(tree, main="Conditional Inference Tree for Cognitive Load")

# build model
tree = C5.0(outcome ~ ., data = train, trials=10)

print('---------------------------------------------------------')
print('Evaluation of decision tree with c5.0:')
# make predictions
confusionMatrix(predict(tree, newdata=test, type = 'class'), t2)





fit <- randomForest(outcome ~ ., train,ntree=500)
summary(fit)
predictedrf = predict(fit,test)


print('---------------------------------------------------------')
print('Evaluation of random Forest:')
confusionMatrix(factor(predictedrf), t2)


# train a model using our training data
model_gbm = gbm(outcome ~.,
                data = train,
                distribution = "multinomial",
                cv.folds = 3,
                shrinkage = .01,
                n.minobsinnode = 10,
                n.trees = 500)       # 500 tress to be built


#use model to make predictions on test data
pred_test = predict.gbm(object = model_gbm,
                        newdata = test,
                        n.trees = 500,           # 500 tress to be built
                        type = "response")



# Give class names to the highest prediction value.
class_names = colnames(pred_test)[apply(pred_test, 1, which.max)]
result = data.frame(test$outcome, class_names)


print('---------------------------------------------------------')
print('Evaluation of GBM:')
conf_mat = confusionMatrix(test$outcome, as.factor(class_names))
print(conf_mat)


testnum <- df4[,c('duration','AOIFCAF', 'AOIFCModell', 'AOIFCTotal', 'fixcount', 'Abgeschlossen', 'outcome')  ] 
testnum$outcome <- as.numeric(testnum$outcome)
testnum <- testnum %>% mutate(outcome = outcome - 1)
summary(testnum)

train_index <- sample(1:nrow(testnum), nrow(testnum)*0.7)
# Full data set
data_variables <- as.matrix(testnum[,1:6])
data_label <- testnum[,"outcome"]
data_matrix <- xgb.DMatrix(data = as.matrix(testnum), label = data_label)
# split train data and make xgb.DMatrix
train_data   <- data_variables[train_index,]
train_label  <- data_label[train_index]
train_matrix <- xgb.DMatrix(data = train_data, label = train_label)
# split test data and make xgb.DMatrix
test_data  <- data_variables[-train_index,]
test_label <- data_label[-train_index]
test_matrix <- xgb.DMatrix(data = test_data, label = test_label)

numberOfClasses <- length(unique(testnum$outcome))
xgb_params <- list("objective" = "multi:softprob",
                   "eval_metric" = "mlogloss",
                   "num_class" = numberOfClasses)
nround    <- 500 # number of XGBoost rounds
cv.nfold  <- 3

# Fit cv.nfold * cv.nround XGB models and save OOF predictions
cv_model <- xgb.cv(params = xgb_params,
                   data = train_matrix, 
                   nrounds = nround,
                   nfold = cv.nfold,
                   verbose = FALSE,
                   prediction = TRUE)


OOF_prediction <- data.frame(cv_model$pred) %>%
  mutate(max_prob = max.col(., ties.method = "last"),
         label = train_label + 1)
head(OOF_prediction)

print('---------------------------------------------------------')
print('Evaluation of xgb train-parameters:')
# confusion matrix
confusionMatrix(factor(OOF_prediction$max_prob),
                factor(OOF_prediction$label),
                mode = "everything")


bst_model <- xgb.train(params = xgb_params,
                       data = train_matrix,
                       nrounds = nround)



# Predict hold-out test set
test_pred <- predict(bst_model, newdata = test_matrix)
test_prediction <- matrix(test_pred, nrow = numberOfClasses,
                          ncol=length(test_pred)/numberOfClasses) %>%
  t() %>%
  data.frame() %>%
  mutate(label = test_label + 1,
         max_prob = max.col(., "last"))
# confusion matrix of test set
print('---------------------------------------------------------')
print('Evaluation of xgb test-parameters:')
confusionMatrix(factor(test_prediction$max_prob),
                factor(test_prediction$label),
                mode = "everything")

testing <- df4
testing$outcome <- as.numeric(testing$outcome)
testing <- testing %>% mutate(outcome = outcome - 1)

#separate target variable  from input variables
X <- testing %>% select(-outcome)  
Y <- testing$outcome 


train_indices <- sample(nrow(testing), 0.7 * nrow(testing)) #sample 70% for training set
X_train <- X[train_indices, ] #extract features for training set
y_train <- Y[train_indices] #extract target values for training set

X_test <- X[-train_indices, ] 
y_test <- Y[-train_indices]

xgb_train <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
xgb_test <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
xgb_params <- list(
  booster = "gbtree",
  eta = 0.01,
  max_depth = 8,
  gamma = 4,
  subsample = 0.75,
  colsample_bytree = 1,
  objective = "multi:softprob",
  eval_metric = "mlogloss",
  num_class = length(levels(df4$outcome))
)

xgb_model <- xgb.train(
  params = xgb_params,
  data = xgb_train,
  nrounds = 5000,
  verbose = 1
)


importance_matrix <- xgb.importance(
  feature_names = colnames(xgb_train), 
  model = xgb_model
)

xgb.plot.importance(importance_matrix)

xgb_preds <- predict(xgb_model, as.matrix(X_test), reshape = TRUE)
xgb_preds <- as.data.frame(xgb_preds)
colnames(xgb_preds) <- levels(df4$outcome)


xgb_preds$PredictedClass <- apply(xgb_preds, 1, function(y) colnames(xgb_preds)[which.max(y)])
xgb_preds$ActualClass <- levels(df4$outcome)[y_test + 1]


print('---------------------------------------------------------')
print('Evaluation of tuned xgb:')
confusionMatrix(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass))
cm <- confusionMatrix(factor(xgb_preds$ActualClass), factor(xgb_preds$PredictedClass))
cfm <- as_tibble(cm$table)



features <- X_train
labels <- y_train
train_pool <- catboost.load_pool(data = features, label = labels)

model_cb <- catboost.train(train_pool,  NULL,
                           params = list(loss_function = 'MultiClass',
                                         iterations = 5000, metric_period=10, logging_level = 'Silent'))

real_data <- X_test
real_pool <- catboost.load_pool(real_data)

prediction <- catboost.predict(model_cb, real_pool, prediction_type = 'Class' )

print('---------------------------------------------------------')
print('Evaluation of catboost:')
confusionMatrix(factor(prediction),
                factor(y_test),
                mode = "everything")
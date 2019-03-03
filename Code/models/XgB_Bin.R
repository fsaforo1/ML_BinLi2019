library(ggplot2) # Data visualization
library(readr) # CSV file I/O, e.g. the read_csv function
library(caret)
library(DMwR) # smote
library(xgboost)
library(Matrix)
library(reshape) #melt
library(pROC) # AUC

modData <- spotify[,-c(1,2,3,17)]
prop.table(table(modData$target))
modData$Time <- NULL


set.seed(194)
inTrain <- createDataPartition(y = modData$target, p = .7, list = F)
train <- modData[inTrain,]
testcv <- modData[-inTrain,]
inTest <- createDataPartition(y = testcv$target, p = .6, list = F)
test <- testcv[inTest,]
cv <- testcv[-inTest,]
rm(inTrain, inTest, testcv, spotify)


i <- grep("target", colnames(train)) # Get index target column
train_smote <- SMOTE(target ~ ., as.data.frame(train), perc.over = 20000, perc.under=100)

prop.table(table(train_smote$target))


# Back to numeric
train$target <- as.numeric(levels(train$target))[train$target]
train_smote$target <- as.numeric(levels(train_smote$target))[train_smote$target]

# As Matrix
train <- Matrix(as.matrix(train), sparse = TRUE)
train_smote <- Matrix(as.matrix(train_smote), sparse = TRUE)
test <- Matrix(as.matrix(test), sparse = TRUE)
cv <- Matrix(as.matrix(cv), sparse = TRUE)

# Create XGB Matrices
train_xgb <- xgb.DMatrix(data = train[,-i], label = train[,i])
train_smote_xgb <- xgb.DMatrix(data = train_smote[,-i], label = train_smote[,i])
test_xgb <- xgb.DMatrix(data = test[,-i], label = test[,i])
cv_xgb <- xgb.DMatrix(data = cv[,-i], label = cv[,i])

# Watchlist
watchlist <- list(train  = train_xgb, cv = cv_xgb)

# set parameters:
parameters <- list(
  # General Parameters
  booster            = "gbtree",          
  silent             = 0,                 
  # Booster Parameters
  eta                = 0.3,               
  gamma              = 0,                 
  max_depth          = 6,                 
  min_child_weight   = 1,                 
  subsample          = 1,                 
  colsample_bytree   = 1,                 
  colsample_bylevel  = 1,                 
  lambda             = 1,                 
  alpha              = 0,                 
  # Task Parameters
  objective          = "binary:logistic",   
  eval_metric        = "auc",
  seed               = 1900               
)



# Original
xgb.model <- xgb.train(parameters, train_xgb, nrounds = 100, watchlist)

#Plot:
melted <- melt(xgb.model$evaluation_log, id.vars="iter")
ggplot(data=melted, aes(x=iter, y=value, group=variable, color = variable)) + geom_line()

# Smote
xgb_smote.model <- xgb.train(parameters, train_smote_xgb, nrounds = 100, watchlist)
#xgb_smote.model$evaluation_log

#Plot:
melted <- melt(xgb_smote.model$evaluation_log, id.vars="iter")
ggplot(data=melted, aes(x=iter, y=value, group=variable, color = variable)) + geom_line()


q <-  0.5

# Original
xgb.predict <- predict(xgb.model, test)
xgb.predictboolean <- ifelse(xgb.predict >= q,1,0)  
roc <- roc(test[,i], predict(xgb.model, test, type = "prob"))
xgb.cm <- confusionMatrix(xgb.predictboolean, test[,i])
xgb.cm$table
print(paste("AUC of XGBoost is:",roc$auc))
print(paste("F1 of XGBoost is:", xgb.cm$bytarget["F1"]))
xgb.cm$bytarget

# SMOTE
roc_smote <- roc(test[,i], predict(xgb_smote.model, test, type = "prob"))
xgb_smote.predict <- predict(xgb_smote.model, test)
xgb_smote.predictboolean <- ifelse(xgb_smote.predict >= q,1,0)  
xgb_smote.cm <- confusionMatrix(xgb_smote.predictboolean, test[,i])
xgb_smote.cm$table
print(paste("AUC of SMOTE XGBoost is:",roc_smote$auc))
print(paste("F1 of SMOTE XGBoost is:", xgb_smote.cm$bytarget["F1"]))
xgb_smote.cm$bytarget
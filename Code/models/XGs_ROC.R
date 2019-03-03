
library("R.utils")
modData <- loadToEnv("Out_1.Rdata")[["modData"]]
str(modData)
train <- loadToEnv("Out_1.Rdata")[["train"]]
test <- loadToEnv("Out_1.Rdata")[["test"]]
rm(modData)
Out <- 'target'
pred <- names(train)[!names(train)%in% Out]

set.seed(194)
grid_default <- expand.grid(
  nrounds = 100,
  max_depth = 6,
  eta = 0.3,
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

train_control <- caret::trainControl(
  method = "none",
  verboseIter = FALSE, # no training log
  allowParallel = TRUE, # FALSE for reproducible results
  summaryFunction = twoClassSummary,
  classProbs = TRUE# FALSE for reproducible results 
)

xgb_base <- caret::train(
  x = train[,pred],
  y = train[,Out],
  trControl = train_control,
  tuneGrid = grid_default,
  method = "xgbTree",
  metric = "ROC",
  verbose = TRUE
)

nrounds <- 400
tune_grid <- expand.grid(
  nrounds = seq(from = 10, to = nrounds, by = 40),
  eta = c(0.025, 0.05, 0.1, 0.3),
  max_depth = c(2, 3, 4, 5, 6),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = 1,
  subsample = 1
)

tune_control <- caret::trainControl(
  method = "cv", # cross-validation
  number = 3, # with n folds 
  #index = createFolds(tr_treated$Id_clean), # fix the folds
  verboseIter = FALSE, # no training log
  allowParallel = TRUE, # FALSE for reproducible results 
  summaryFunction = twoClassSummary,
  classProbs = TRUE# FALSE for reproducible results 
)

xgb_tune <- caret::train(
  x = train[,pred],
  y = train[,Out],
  trControl = tune_control,
  tuneGrid = tune_grid,
  method = "xgbTree",
  metric = "ROC",
  verbose = TRUE
)

# helper function for the plots
tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$ROC, probs = probs), min(x$results$ROC))) +
    theme_bw()
}

tuneplot(xgb_tune)


tune_grid2 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = ifelse(xgb_tune$bestTune$max_depth == 2,
                     c(xgb_tune$bestTune$max_depth:4),
                     xgb_tune$bestTune$max_depth - 1:xgb_tune$bestTune$max_depth + 1),
  gamma = 0,
  colsample_bytree = 1,
  min_child_weight = c(1, 2, 3),
  subsample = 1
)

xgb_tune2 <- caret::train(
  x = train[,pred],
  y = train[,Out],
  trControl = tune_control,
  tuneGrid = tune_grid2,
  method = "xgbTree",
  metric = "ROC",
  verbose = TRUE
)

tuneplot(xgb_tune2)
xgb_tune2$bestTune


tune_grid3 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = 0,
  colsample_bytree = c(0.4, 0.6, 0.8, 1.0),
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = c(0.5, 0.75, 1.0)
)

xgb_tune3 <- caret::train(
  x = train[,pred],
  y = train[,Out],
  trControl = tune_control,
  tuneGrid = tune_grid3,
  method = "xgbTree",
  metric = "ROC",
  verbose = TRUE
)

tuneplot(xgb_tune3, probs = .95)
xgb_tune3$bestTune


tune_grid4 <- expand.grid(
  nrounds = seq(from = 50, to = nrounds, by = 50),
  eta = xgb_tune$bestTune$eta,
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = c(0, 0.05, 0.1, 0.5, 0.7, 0.9, 1.0),
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune4 <- caret::train(
  x = train[,pred],
  y = train[,Out],
  trControl = tune_control,
  tuneGrid = tune_grid4,
  method = "xgbTree",
  metric = "ROC",
  verbose = TRUE
)

tuneplot(xgb_tune4)
xgb_tune4$bestTune


tune_grid5 <- expand.grid(
  nrounds = seq(from = 5, to = 400, by = 10),
  eta = c(0.01, 0.015, 0.025, 0.05, 0.1),
  max_depth = xgb_tune2$bestTune$max_depth,
  gamma = xgb_tune4$bestTune$gamma,
  colsample_bytree = xgb_tune3$bestTune$colsample_bytree,
  min_child_weight = xgb_tune2$bestTune$min_child_weight,
  subsample = xgb_tune3$bestTune$subsample
)

xgb_tune5 <- caret::train(
  x = train[,pred],
  y = train[,Out],
  trControl = tune_control,
  tuneGrid = tune_grid5,
  method = "xgbTree",
  metric = "ROC",
  verbose = TRUE
)

tuneplot(xgb_tune5)
xgb_tune5$bestTune

library(PRROC)
library(pROC)
library(purrr)
as.numeric(train$target) - 1
auprcSummary <- function(data, lev = NULL, model = NULL){
  prob_good <- data$target #take the probability of good class
  the_curve <- pr.curve(scores.class0 = prob_good,
                        weights.class0 = as.numeric(data$obs)-1, #provide the class labels as 0/1
                        curve = FALSE)
  out <- the_curve$auc.integral
  names(out) <- "AUPRC"
  out
}



(final_grid <- expand.grid(
  nrounds = xgb_tune5$bestTune$nrounds,
  eta = xgb_tune5$bestTune$eta,
  max_depth = xgb_tune5$bestTune$max_depth,
  gamma = xgb_tune5$bestTune$gamma,
  colsample_bytree = xgb_tune5$bestTune$colsample_bytree,
  min_child_weight = xgb_tune5$bestTune$min_child_weight,
  subsample = xgb_tune5$bestTune$subsample
))



tune_fCont <- caret::trainControl(
  method = "cv", # cross-validation
  number = 3, # with n folds 
  #index = createFolds(tr_treated$Id_clean), # fix the folds
  verboseIter = FALSE, # no training log
  allowParallel = TRUE,
  summaryFunction = twoClassSummary,
  classProbs = TRUE# FALSE for reproducible results 
)


(xgb_final <- caret::train(
  x = train[,pred],
  y = train[,Out],
  method = "xgbTree",
  metric = "ROC",
  verbose = TRUE,
  trControl = tune_fCont,
  tuneGrid = final_grid
))

tuneplot(xgb_final)
xgb_final$bestTune

confusionMatrix(test$target, predict(xgb_tune3, newdata = test))


model_list <- list(Base = tune_grid,
                   Tune1 = tune_grid2,
                   Tune2 = tune_grid3,
                   Tune3 = tune_grid4,
                   Tune4 = tune_grid5,
                   Final = xgb_finalROC)

model_list_roc <- model_list %>%
  map(test_roc, data = test)

model_list_roc %>%
  map(auc)

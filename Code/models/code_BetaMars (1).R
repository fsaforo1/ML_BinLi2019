# betareg caret
library(caret)
library(betareg)

set.seed(194)
# Recursive Feature Elimination
ctrl_fs <- rfeControl(functions = rfFuncs,
                      method = "cv",     # default is 10 CVs
                      verbose = FALSE)

outcomeName <- 'popularity'                                                # Specify Outcome Variable
predictors <- names(spotTrain)[!names(spotTrain) %in% outcomeName]         # Specify Predictor Variables
Target_Profile <- rfe(spotTrain[1:1000,predictors], spotTrain[1000,outcomeName],
                      rfeControl = ctrl_fs)




# create a betareg caret model
# type and library
betaregression <- list(type='Regression', 
                       library='betareg',
                       loop=NULL)

# parameters to tune
prm <- data.frame(parameter=c("link", "type", "link.phi"),
                  class= rep("character", 3))

# add to the model
betaregression$parameters <- prm

# grid search, this is the default grid search, user can specify otherwise
# creates 54 separate models, so if looking to speed up try fewer params in grid
betaGrid  <- function(x, y, len=NULL, search="grid"){
  if(search == "grid"){
    out <- expand.grid(link=c("logit", "probit", "cloglog", "cauchit", "log", "loglog"),
                       type=c("ML", "BC", "BR"),
                       link.phi=c("identity", "log", "sqrt"), stringsAsFactors = F) # here force the strings as character,
    # othewise get error that the model arguments
    # were expecting 'chr' when fitting
  }
  out
}

# add the grid search
betaregression$grid <- betaGrid

# create the fit
betaFit <- function(x, y, wts, param, lev, last, weights, classProbs, ...){
  
  dat <- if(is.data.frame(x)) x else as.data.frame(x)
  dat$.outcome <- y
  
  theDots <- list(...)
  
  modelArgs <- c(list(formula = as.formula(".outcome ~ ."), data = dat, link=param$link, type=param$type), theDots)
  
  out <- do.call(betareg::betareg, modelArgs)
  out$call <- NULL
  out
}

# betaregression fit
betaregression$fit <- betaFit

# predict element
betaPred <- function(modelFit, newdata, preProc=NULL, submodels=NULL){
  if(!is.data.frame(newdata)) newdata <- as.data.frame(newdata)
  betareg::predict(modelFit, newdata)
}

# add the predict method
betaregression$predict <- betaPred

# regression, no probabities calculated
# just assigning NULL didnt work for some reason
# wrapped in a function instead
betaProb <- function(){
  return(NULL)
}
betaregression$prob <- betaProb


# test it on a dataset


# 10 fold cross validation
fitControl <- trainControl(method='cv')

# betaregression, takes a min or so with full grid
betareg <- train(spotTrain[,predictors], spotTrain[,outcomeName],
                 preProcess=c('scale', 'center'), 
                 method=betaregression,
                 trControl = fitControl) 

# look at output
betareg



# MARS model
# http://uc-r.github.io/mars
# create a tuning grid
hyper_grid <- expand.grid(
  degree = 1:3, 
  nprune = seq(2, 100, length.out = 10) %>% floor()
)

# for reproducibiity
set.seed(123)

# cross validated model
tuned_mars <- train(
  spotTrain[,predictors], spotTrain[,outcomeName],
  preProcess=c('scale', 'center'), 
  method = "earth",
  metric = "RMSE",
  trControl = trainControl(method = "cv", number = 10),
  tuneGrid = hyper_grid
)

# best model
tuned_mars$bestTune

# plot results
ggplot(tuned_mars)

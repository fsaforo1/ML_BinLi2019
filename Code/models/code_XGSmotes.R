library(plyr)
modData$target <- revalue(modData$target, c("0"="No", "1"="Yes"))
sparse_matrix <- sparse.model.matrix(target ~ ., data = train)[,-1]
head(sparse_matrix)

output_vector = train[,'target'] == "Yes"

bst <- xgboost(data = sparse_matrix, label = output_vector,
               nthread = 2, nrounds = 70,
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
               eval_metric        = "auc")



importance <- xgb.importance(feature_names = colnames(sparse_matrix), model = bst)
head(importance)


importanceRaw <- xgb.importance(feature_names = colnames(sparse_matrix),
                                model = bst, data = sparse_matrix, label = output_vector)

# Cleaning for better display
importanceClean <- importanceRaw[,`:=`(Cover=NULL, Frequency=NULL)]

head(importanceClean)

xgb.plot.importance(importance_matrix = importance[1:8,])



## Random Forest

#Random ForestT - 1000 trees
bst2 <- xgboost(data = sparse_matrix, label = output_vector, max_depth = 4,
                num_parallel_tree = 1000, subsample = 0.5,
                colsample_bytree =0.5, nrounds = 1, objective = "binary:logistic")

importance2 <- xgb.importance(feature_names = colnames(sparse_matrix), model = bst2)
head(importance2)


importanceRaw2 <- xgb.importance(feature_names = colnames(sparse_matrix),
                                 model = bst2, data = sparse_matrix, label = output_vector)

#  clean important variable data
importanceClean2 <- importanceRaw2[,`:=`(Cover=NULL, Frequency=NULL)]

head(importanceClean2)


xgb.plot.importance(importance_matrix = importance2, top_n = 30)
library(Ckmeans.1d.dp)
xgb.ggplot.importance(importance)

xgb.plot.tree(bst)

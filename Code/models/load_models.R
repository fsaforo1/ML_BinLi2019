library("R.utils")

top5.long <- loadToEnv("./BinLi_Project/Out_1.Rdata")[["top5.long"]]
ctone2 <- loadToEnv("./BinLi_Project/Out_1.Rdata")[["ctone2"]]
tone2 <- loadToEnv("./BinLi_Project/Out_1.Rdata")[["tone2"]]
modData <- loadToEnv("./BinLi_Project/Out_1.Rdata")[["modData"]]
newdata <- loadToEnv("./BinLi_Project/Out_1.Rdata")[["newdata"]]
spotTest <- loadToEnv("./BinLi_Project/Out_2.Rdata")[["spotTest"]]
spotTrain <- loadToEnv("./BinLi_Project/Out_2.Rdata")[["spotTrain"]]
xgb.model <- loadToEnv("./BinLi_Project/Out_1.Rdata")[["xgb.model"]]
xgb_smote.model <- loadToEnv("./BinLi_Project/Out_1.Rdata")[["xgb_smote.model"]]
melted <- loadToEnv("./BinLi_Project/Out_1.Rdata")[["melted"]]
importance <- loadToEnv("./BinLi_Project/Out_1.Rdata")[["importance"]]
train <- loadToEnv("./BinLi_Project/Out_1.Rdata")[["bst"]]
bst <- loadToEnv("./BinLi_Project/Out_1.Rdata")[["bst"]]
sparse_matrix <- loadToEnv("./BinLi_Project/Out_1.Rdata")[["sparse_matrix"]]
#test <- loadToEnv("./BinLi_Project/Out_1.Rdata")[["test"]]
sparse_test <- sparse.model.matrix(target ~ ., data = test)[,-1]

RFE <- loadToEnv("./BinLi_Project/Out_Final2.Rdata")[["Target_Profile"]]
MARS_model <- loadToEnv("./BinLi_Project/Out_Final2.Rdata")[["tuned_mars"]]
xgb_base <- loadToEnv("./BinLi_Project/Out_Final2.Rdata")[["xgb_base"]]
xgb_final <- loadToEnv("./BinLi_Project/Out_Final2.Rdata")[["xgb_finalROC"]]
xgb_tune <- loadToEnv("./BinLi_Project/Out_Final2.Rdata")[["xgb_tune"]]
xgb_tune2 <- loadToEnv("./BinLi_Project/Out_Final2.Rdata")[["xgb_tune2"]]
xgb_tune3 <- loadToEnv("./BinLi_Project/Out_Final2.Rdata")[["xgb_tune3"]]
xgb_tune4 <- loadToEnv("./BinLi_Project/Out_Final2.Rdata")[["xgb_tune4"]]
xgb_tune5 <- loadToEnv("./BinLi_Project/Out_Final2.Rdata")[["xgb_tune5"]]
xgb_finalOther <- loadToEnv("./BinLi_Project/Out_Final2.Rdata")[["xgb_final"]]

TRAIN <- loadToEnv("./BinLi_Project/Out_Final2.Rdata")[["spotTrain"]]
TEST <- loadToEnv("./BinLi_Project/Out_Final2.Rdata")[["spotTest"]]

classTEST <- loadToEnv("./BinLi_Project/Out_Final2.Rdata")[["test"]]
classTRAIN <- loadToEnv("./BinLi_Project/Out_Final2.Rdata")[["train"]]

RFmodel_list <- loadToEnv("jan_code.Rdata")[["model_list"]]
train_trim <- loadToEnv("jan_code.Rdata")[["train_trim"]]

library("R.utils");
top5.long <- loadToEnv("./BinLi_Project/Out_1.Rdata")[["top5.long"]]

top5plot<- ggplot(data=top5.long, aes(x=variable, y=value))+
  geom_bar(aes(y=value, fill=track_name),stat="identity", alpha=0.8 , position="dodge")+
  ylab("Value")+ xlab("Song Variables")+coord_flip()+ggtitle("Top 5 songs in Spotify 2018 ")+
  scale_fill_brewer(palette="Dark2")+
  theme_bw() + theme(legend.position="bottom")

top5plot

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

PopDance <- read.csv("./BinLi_Project/Data/popvsdance.csv",header=T)
PopEnergy <- read.csv("./BinLi_Project/Data/popvsenergy.csv",header=T)
PopValence <- read.csv("./BinLi_Project/Data/popvsvalence.csv",header=T)

ggplot(data=newdata, aes(x = loudness, y = popularity, colour = tone)) + geom_point(size = 2, colour = "black") + geom_point(size = 2)


library(GGally)
a <- ggparcoord(modData[1000:2000, -c(6,9,12)], columns = 1:10, groupColumn = "target")
a <- a + theme(plot.title = element_text(hjust=0.5), 
          axis.text.x=element_text(size=15, angle=90,hjust=0.95,vjust=0.2))
a

library(mlr)

var_imp <- generateFilterValuesData(train.task, method = c("information.gain"))
plotFilterValues(var_imp,feat.type.cols = TRUE)


# helper function for the plots
tuneplot <- function(x, probs = .90) {
  ggplot(x) +
    coord_cartesian(ylim = c(quantile(x$results$ROC, probs = probs), min(x$results$ROC))) +
    theme_bw()
}


test_roc <- function(model, data){
  roc(data$target,
      predict(model, data, type="prob")[, "Yes"])
}

XGB_models <- list(default = xgb_tune,
                   SMOTE = xgb_finalOther)

XGB_list_roc <- XGB_models %>%
  map(test_roc, data = classTEST)

XGB_list_TRAIN_roc <- XGB_models %>%
  map(test_roc, data = classTRAIN)

XGB_list_roc %>%
  map(auc)

RFmodel_list <- list(original = orig_fit,
                   weighted = weighted_fit,
                   down = down_fit,
                   up = up_fit,
                   SMOTE = smote_fit)

RF_list_TRAIN_roc <- RFmodel_list %>%
  map(test_roc, data = train_trim)

xgb_smote.model$evaluation_log[
  xgb_smote.model$evaluation_log$cv_auc==max(xgb_smote.model$evaluation_log$cv_auc),]

library(rmarkdown); render('./BinLi_Project/Final_Prez2.Rmd')




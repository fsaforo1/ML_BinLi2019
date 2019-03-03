# Checking for Variation
fullTrain <- spotTrain[, -c(1,2,3,17)]
fullTest <- spotTest[, -c(1,2,3,17)]

library(mlr)
train.task <- makeClassifTask(data = fullTrain, target = "target")
test.task <- makeClassifTask(data = fullTest, target = "target")
var_imp <- generateFilterValuesData(train.task, method = c("information.gain"))
plotFilterValues(var_imp,feat.type.cols = TRUE)


library(corrplot)
X <- newnewdata[, sapply(newnewdata, class) != "factor"]
X <- X[, sapply(X, class) != "character"]
M <- cor(X)


## Correlation Matrix
corrplot(M, type = "upper")
#corrplot.mixed(M)


library(GGally)
ggpairs(newdata[sample(1:dim(newdata)[1], 1000), 
                c("loudness","instrumentalness","duration_ms","danceability","popularity")])

coordData <- modData[(sample(1:dim(modData)[1], 200)), c(1, 2, 3, 5, 8, 14)]
a <- ggparcoord(coordData, columns = 1:5, 
                groupColumn = "target")
a <- a + theme(plot.title = element_text(hjust=0.5), 
               axis.text.x=element_text(size=15, angle=90,hjust=0.95,vjust=0.2))
a

library(car)
library(RColorBrewer)
# Make the plot
my_colors <- brewer.pal(nlevels((newdata$tone)), "Set2")
scatterplotMatrix(~loudness+instrumentalness+duration_ms+danceability+popularity|tone, 
                  data=newdata[sample(1:dim(newdata)[1], 1000),] , 
                  reg.line=" " , smoother=" ", col=my_colors , 
                  smoother.args=list(col="grey") , cex=1.5 , 
                  pch=c(15,16,17) , main="Scatter plot with by tone")

library(wordcloud)
wordcloud(newdata$track_name, max.words=100 ,random.order=FALSE,rot.per=0.35,colors=brewer.pal(4, "Dark2"), main="Title")

layout(matrix(c(1,2), 1, 2, byrow = TRUE))
plotFilterValues(var_imp,feat.type.cols = TRUE)
corrplot(M, type = "upper")

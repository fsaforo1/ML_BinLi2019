## Dataset

ipak <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

# use function to install and load packages
packages <- c("AppliedPredictiveModeling","caret","data.table","ggplot2","lattice"
              ,"ggplot2","tidyr", "fpp", "Metric")
ipak(packages)
getwd()
spotify <- read.csv("./BinLi_Project/Data/SpotifyAudioFeaturesNov2018.csv",header=T)
str(spotify)

spotify[!complete.cases(spotify), ]
sum(is.na(spotify))
spotify$target <- as.factor(ifelse(spotify$popularity <=50, 0, 1))
prop.table(table(spotify$target))



cols <- c("key", "mode")
spotify[cols] <- lapply(spotify[cols], factor)
str(spotify)

dim(spotify)
prop.table(table(spotify$target))


library(pryr)
object_size(spotify)

trainIndex <- createDataPartition(spotify$popularity, p = .70, 
                                  list = FALSE, 
                                  times = 1)

spotTrain <- spotify[trainIndex, ]
spotTest <- spotify[-trainIndex, ]
object_size(spotTrain)
object_size(spotTest)



##Visualization
music <- spotTrain
music$danceability<- spotTrain$danceability*100
music$energy<- spotTrain$energy*100
music$speechiness<- spotTrain$speechiness*100
music$acousticness<- spotTrain$acousticness*100
music$instrumentalness<- spotTrain$instrumentalness*100
music$liveness<- spotTrain$liveness*100
music$valence<- spotTrain$valence*100
object_size(music)


# Top 5 Songs
library(reshape)
library(RColorBrewer)
newdata <- spotify[order(-spotify$popularity),] 
top5 <- newdata[c(1:5),]
top5<- top5[, c("track_name","acousticness", "danceability", "energy", "liveness", "valence", "speechiness")] 
top5<- as.data.frame(top5)
top5.long <- melt(top5, id.vars="track_name")

top5plot<- ggplot(data=top5.long, aes(x=variable, y=value))+
  geom_bar(aes(y=value, fill=track_name),stat="identity", alpha=0.8 , position="dodge")+
  ylab("Value")+ xlab("Song Variables")+coord_flip()+ggtitle("Top 5 songs in Spotify 2018 ")+
  scale_fill_brewer(palette="Dark2")+
  theme_bw() + theme(legend.position="bottom")

top5plot


## Key signatures
library(dplyr)
library(treemap)
top100 <- newdata[c(1:100),]

tone1 <- group_by(top100, keylabel)
tone2 <- dplyr::summarise(tone1,  count=n())
tone2 <- arrange(tone2, desc(count))


# Tonality treemap
tone_key <- treemap(tone2, index="keylabel", vSize="count", type="index", 
        palette="Pastel2", title="Top 100 Songs Key charactersics and Emotion", fontsize.title=11)
tone_key

# library(d3Tree)
# hey <- treemap(tone2, index="keylabel", vSize="count", type="index", 
#                palette="Pastel2", title="Top 100 Songs Key charactersics and Emotion", fontsize.title=11)
# d3tree(hey)

ctone1 <- group_by(newdata, keys )
ctone2 <- dplyr::summarise(ctone1,  count=n())
ctone2 <- arrange(ctone2, desc(count))


# Tonality treemap - Minor/Major
tone_index <- treemap(ctone2, index="keys", vSize="count", type="index", 
        palette="Pastel2", title="Top 100 Songs Key charactersics", fontsize.title=12)

tone_index






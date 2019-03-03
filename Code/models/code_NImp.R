set.seed(194)
spotify <- read.csv("./BinLi_Project//Data/SpotifyAudioFeaturesNov2018.csv",header=T)
cols <- c("key", "mode")
spotify[cols] <- lapply(spotify[cols], factor)
spotify <- spotify[,-c(1,2,3)]
spotify <- subset(spotify, popularity>=10)
spotify$popularity <- spotify$popularity/100
spotify <- subset(spotify, popularity<=0.9)
trainIndex <- createDataPartition(spotify$popularity, p = .70,
                                  list = FALSE,
                                  times = 1)
spotTrain <- spotify[trainIndex, ]
spotTest <- spotify[-trainIndex, ]


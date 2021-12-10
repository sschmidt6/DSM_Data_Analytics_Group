library(gdata)
library(readxl)
library(dplyr)
library(tidyverse)
library(cluster)
library(factoextra)
library(gridExtra)

mycsv <- read.csv("members.csv")

mydata <- read.csv("members.csv")

mydata$MemberKey <- as.character(mydata$MemberKey)
row.names(mydata) <- mydata$MemberKey

mydata <- subset(mydata, select = c(TotalAttendance, Went, NoShow, DidntGo, WentPct, NoShowPct, DidntGoPct))

# Prepare Data
mydata <- na.omit(mydata) # listwise deletion of missing
mydata <- scale(mydata) # standardize variables

kmeans2 <- kmeans(mydata, centers = 2, nstart = 25)
str(kmeans2)

kmeans3 <- kmeans(mydata, centers = 3, nstart = 25)  #DataFlair
kmeans4 <- kmeans(mydata, centers = 4, nstart = 25)  
kmeans5 <- kmeans(mydata, centers = 5, nstart = 25)  
kmeans6 <- kmeans(mydata, centers = 6, nstart = 25)
kmeans10 <- kmeans(mydata, centers = 10, nstart = 25)
#Comparing the Plots
plot1 <- fviz_cluster(kmeans2, geom = "point", data = mydata) + ggtitle("k = 2")
plot2 <- fviz_cluster(kmeans3, geom = "point", data = mydata) + ggtitle("k = 3")
plot3 <- fviz_cluster(kmeans4, geom = "point", data = mydata) + ggtitle("k = 4")
plot4 <- fviz_cluster(kmeans5, geom = "point", data = mydata) + ggtitle("k = 5")
plot5 <- fviz_cluster(kmeans6, geom = "point", data = mydata) + ggtitle("k = 6")
plot10 <- fviz_cluster(kmeans10, geom = "point", data = mydata) + ggtitle("k = 10")
grid.arrange(plot2, plot3, plot4, plot5, nrow = 2)


# Determine number of clusters
wss <- (nrow(mydata)-1)*sum(apply(mydata,2,var))
for (i in 2:20) wss[i] <- sum(kmeans(mydata,
                                     centers=i)$withinss)
plot(1:20, wss, type="b", xlab="Number of Clusters",
     ylab="Within group sum of squares")

# K-Means Cluster Analysis
fit <- kmeans(mydata, 6) # 6 cluster solution
# get cluster means
aggregate(mydata,by=list(fit$cluster),FUN=mean)
# append cluster assignment
mydata <- data.frame(mydata, fit$cluster)

# calculate silhouette
library(cluster)   
sil <- silhouette(fit$cluster, dist(mydata))
sil3 <- silhouette(kmeans3$cluster, dist(mydata))
sil4 <- silhouette(kmeans4$cluster, dist(mydata))
sil5 <- silhouette(kmeans5$cluster, dist(mydata))
sil10 <- silhouette(kmeans10$cluster, dist(mydata))


# plot silhouette
library(factoextra)
fviz_silhouette(sil)

write.csv(mydata,'clusters.csv')

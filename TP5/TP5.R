#################################
### Algorithme des k moyennes ###
#################################

### Exercice 1 : Introduction
#############################

iris <- read.csv("iris.csv", header = T)
iris <- iris[,-1]

# normaliser les colonnes [2,5]
normalize <- function(v) {
  max <- max(v)
  min <- min(v)
  v <- (v - min) / (max - min)
  return(v)
}

irisN <- as.data.frame(lapply(iris[,c(1:4)],normalize))
irisN <- cbind(irisN, iris$Species)
colnames(irisN) <- c("SepalLength", "SepalWidth", "PetalLength", "PetalWidth", "Species")
head(irisN)

kmeans(irisN [,1:4], 3)
iris.3means <-kmeans (irisN [,1:4], 3)
iris.3means$cluster
iris.3means$centers
iris.3means$withinss
iris.3means$tot.withinss
iris.3means$betweenss
iris.3means$size

plot (irisN$PetalLength, irisN$PetalWidth)

sdb1 <- subset(irisN, irisN$Species=="Iris-setosa")
sdb2 <- subset(irisN, irisN$Species=="Iris-versicolor")
sdb3 <- subset(irisN, irisN$Species=="Iris-virginica")

plot(sdb1$PetalLength,sdb1$PetalWidth,xlim=c(0,1),ylim = c(0,1), col="black")
points(sdb2$PetalLength,sdb2$PetalWidth,xlim=c(0,1),ylim = c(0,1), col="red")
points(sdb3$PetalLength,sdb3$PetalWidth,xlim=c(0,1),ylim = c(0,1), col="blue")

afficher <- function(x,y)
{
  plot(sdb1[,x], sdb1[,y], xlim=c(0,1),ylim = c(0,1), main="", ylab=y, xlab=x, col=1, pch=19);
  points(sdb2[,x], sdb1[,y], col=2, pch=19);
  points(sdb3[,x], sdb1[,y], col=3, pch=19);
}

afficher("PetalWidth","PetalLength")
afficher("PetalWidth","SepalWidth")
afficher("PetalWidth","SepalLength")
afficher("SepalWidth","SepalLength")
afficher("SepalWidth","PetalLength")
afficher("SepalWidth","PetalWidth")

subdata1 <- subset(irisN, iris.3means$cluster == 1)
subdata2 <- subset(irisN, iris.3means$cluster == 2)
subdata3 <- subset(irisN, iris.3means$cluster == 3)



afficherClusters <- function(data, k, cluster, x, y)
{
  min.x = min(data[,x])
  max.x = max(data[,x])
  min.y = min(data[,y])
  max.y = max(data[,y])
  
  plot(data[,x], data[,y], xlim = c(min.x,max.x), ylim = c(min.y,max.y), main="",ylab=y, xlab=x, col=1, pch=19)
  
  for (i in 1:k) {
    subdata <- subset(data, cluster == i)
    points(subdata[,x],subdata[,y],col=i,pch=19)
  }
}

afficher("PetalWidth","PetalLength")
afficherClusters(irisN,3,iris.3means$cluster,"PetalWidth","PetalLength")

labels.cluster <- rep("", length(iris.3means$cluster))

labels.cluster[iris.3means$cluster == 1] <- "Iris-setosa"
labels.cluster[iris.3means$cluster == 2] <- "Iris-versicolor"
labels.cluster[iris.3means$cluster == 3] <- "Iris-virginica"

# Table de confusion
table(labels.cluster,irisN$Species)

iris.kmeans <- kmeans(irisN [, 1:4], centers=8)
iris.kmeans$tot.withinss
iris.kmeans$betweenss
avg <- integer(9)
for(k in 2:10){
  tmp <- integer(30)
  for(i in 1:30){
    iris.kmeans <- kmeans(iris[,1:4],centers=k)
    tmp[i] <- iris.kmeans$tot.withinss
  }
  avg[k-1] <- mean(tmp)
}

avg
plot(2:10, avg, type="b", main="Withinss by various K",ylab="Average Total Within Sum of Squares",xlab="valeur de k")

####################
### TP7 : DBSCAN ###
####################

# Ce TP nécessite l'instalation du package suivant :
# install.packages("dbscan")

# Chargement du jeu de données "multishapes.txt"
library(dbscan)
data.multishapes<- read.table("multishapes.txt", header = TRUE)
plot(data.multishapes$x, data.multishapes$y)

# Fonction permettant de visualiser les différents clusters
afficherClusters <- function(data, k, cluster, x, y)
{
  min.x = min(data[,x])
  max.x = max(data[,x])
  min.y = min(data[,y])
  max.y = max(data[,y])
  
  plot(data[,x], data[,y], xlim = c(min.x,max.x), ylim = c(min.y,max.y), main="",ylab=y, xlab=x, col=1, pch=19)
  
  for (i in 1:k) {
    subdata <- subset(data, cluster == i)
    points(subdata[,x],subdata[,y],col=i+1,pch=19)
  }
}

# On affichage notre dataframe et ses differants clusters
db <- dbscan(data.multishapes, eps = 0.14, minPts = 5)
db
afficherClusters(data.multishapes,length(db$cluster),db$cluster,"x","y")


# On détermine le coude de la courbe qui correspond au paramètre optimal d'eps.
kNNdist(data.multishapes, k=5)
kNNdistplot(data.multishapes, k =  5)
locator(n = 1, type = "n")

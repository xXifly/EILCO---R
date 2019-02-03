#########################################
### lustering hi ́erarchique ascendant ###
#########################################


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


dist(irisN [,1:4])

iris.hclust <- hclust (dist (irisN [,1:4]))
plot (iris.hclust)


cutree(iris.hclust, 3)


plot (iris.hclust)
rect.hclust(iris.hclust,5)
rect.hclust(iris.hclust, 5, border = c ("blue", "green",
                                        "red", "pink", "black"))
rect.hclust(iris.hclust, 7, which = c (2,5))

plot (iris.hclust)
iris.hclust.id <- identify (iris.hclust)
iris.hclust.id

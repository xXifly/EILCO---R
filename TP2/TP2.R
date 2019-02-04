##########################################
#####  TP2 : K plus proches voisins  #####
##########################################


# Chargement et analyse de la base
##################################

# On charge le tableau de données
data <- read.csv("iris.csv", header=TRUE)
# On affiche la première partie du tableau
head(data)
# On analyse ce tableau (min, max, moyenne, etc )
summary(data)
str(data)
table(data$Species)

# On mélange la base
n <- length(data$Id)
v <- round(runif(n,0,1), digits=2)
dataR <- data[order(v),]

head(dataR)
summary(dataR)
str(dataR)

# Normaliser
############

normalize <- function(v) {
  v <- ( v - min(v) ) / ( max(v) - min(v) )
  return(v)
}

dataN <- lapply(dataR[,c(2:5)],normalize)
dataN <- as.data.frame(dataN) 

head(dataN)
summary(dataN)

# Apprentissage avec KNN
########################

dataTrain <- dataN[c(1:100),]
dataTest <- dataN[c(101:150),]

labelTrain <- dataR[c(1:100),c(6)]
labelTest <- dataR[c(101:150),c(6)]

library(class)

model <- knn(train=dataTrain, test=dataTest, cl=labelTrain, k=3)
# model contient la prédiction de dataTest
model
table(model)
table(labelTest)
conf<-table(model,labelTest)
conf

#calcul de l'erreur
calcErr <- function(v) {
  sumdiag <- 0
  for (i in c(1:length(v[,1]))) {
    sumdiag <- sumdiag + v[i,i]
  }
  err <- (1 - (sumdiag / sum(v)) ) * 100
  return(err)
}

calcErr(conf)






### Exercice  2  :  Reconnaissance  de  caract`eres  manuscrits

Dtrain <- read.csv("mnist_train.csv", header=F)
Dtest <- read.csv("mnist_test.csv", header=F)
dim(Dtrain)

for (i in c(1:5)) {
  im <- matrix(Dtrain[i, -1], nrow=28, ncol=28)
  im <- im[,order(28:1)]
  im_numbers <- apply(im, 2, as.numeric)
  image(1:28, 1:28, im_numbers, col=gray((0:255)/255))
}

set.seed(11)

ech <- sample(1:nrow(Dtrain),10000)
SubDtrain <- Dtrain[ech,c(2:785)]
SubLabelTrain <- as.factor(Dtrain[ech,1])

ech <- sample(1:nrow(Dtest),1000)
SubDtest <- Dtest[ech,c(2:785)]
SubLabelTest <- as.factor(Dtest[ech,1])

mnist_model <- knn(train=SubDtrain, test=SubDtest, cl=SubLabelTrain, k=2)
table(mnist_model)
table(SubLabelTest)
mnist_conf<-table(mnist_model,SubLabelTest)
mnist_conf
calcErr(mnist_conf)

# On affiche les indices des images sur lesquelles il y a eu des erreurs
for (i in 1:1000) {
  if(mnist_model[i] != SubLabelTest[i]){
    print(i)
  }
}


im <- matrix(Dtrain[774, -1], nrow=28, ncol=28)
im <- im[,order(28:1)]
im_numbers <- apply(im, 2, as.numeric)
image(1:28, 1:28, im_numbers, col=gray((0:255)/255))

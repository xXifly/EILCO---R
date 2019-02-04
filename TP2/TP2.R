##########################################
#####  TP2 : K plus proches voisins  #####
##########################################




### Exercice 1 :
################


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

# On normalise nos données afin d'obtenir une base de valeurs entre 0 et 1
dataN <- lapply(dataR[,c(2:5)],normalize)
dataN <- as.data.frame(dataN) 

head(dataN)
summary(dataN)

# Apprentissage avec KNN
########################

library(class)

# On divise notre base normalisée en deux partie "train" et "test"
dataTrain <- dataN[c(1:100),]
dataTest <- dataN[c(101:150),]

# On récupère l'espèce à laquelle appartient chaque donnée dans la base non normalisée.
labelTrain <- dataR[c(1:100),c(6)]
labelTest <- dataR[c(101:150),c(6)]

# On applique la fonction KNN
model <- knn(train=dataTrain, test=dataTest, cl=labelTrain, k=3)
# On affiche le résultat obtenu
table(model)
# On affiche le resultat souhaité
table(labelTest)
# On calcule la matrice de confusion
conf<-table(model,labelTest)
conf

calcErr <- function(v) {
  sumdiag <- 0
  for (i in c(1:length(v[,1]))) {
    sumdiag <- sumdiag + v[i,i]
  }
  err <- (1 - (sumdiag / sum(v)) ) * 100
  
  print(paste0("Taux d'erreur : ", err, "%"))
}

# On calcule le taux d'erreur de l'algorithme KNN
calcErr(conf)




### Exercice  2  :  Reconnaissance  de  caractères  manuscrits
##############################################################

# On récupère directement nos bases "test" et "train" déjà pré-traitées
Dtrain <- read.csv("mnist_train.csv", header=F)
Dtest <- read.csv("mnist_test.csv", header=F)
dim(Dtrain)

# Permet d'afficher les 5 premières images de la base "train"
for (i in c(1:5)) {
  im <- matrix(Dtrain[i, -1], nrow=28, ncol=28)
  im <- im[,order(28:1)]
  im_numbers <- apply(im, 2, as.numeric)
  image(1:28, 1:28, im_numbers, col=gray((0:255)/255))
}

set.seed(11)

# On créé un échantillon de 10000 photos sur les 60000 de la base "train"
ech <- sample(1:nrow(Dtrain),10000)
SubDtrain <- Dtrain[ech,c(2:785)]
SubLabelTrain <- as.factor(Dtrain[ech,1])
 
# On créé un échantillon de 1000 photos sur les 10000 de la base "test"
ech <- sample(1:nrow(Dtest),1000)
SubDtest <- Dtest[ech,c(2:785)]
SubLabelTest <- as.factor(Dtest[ech,1])

# On applique la fonction KNN
mnist_model <- knn(train=SubDtrain, test=SubDtest, cl=SubLabelTrain, k=2)
# On affiche le résultat obtenu
table(mnist_model)
# On affiche le resultat souhaité
table(SubLabelTest)

# On calcule la matrice de confusion
mnist_conf<-table(mnist_model,SubLabelTest)
mnist_conf

# On calcule le taux d'erreur de l'algorithme KNN
calcErr(mnist_conf)

# On affiche les indices des images sur lesquelles il y a eu des erreurs
for (i in 1:1000) {
  if(mnist_model[i] != SubLabelTest[i]){
    
    # On affiche les images sur lesquelles il y a eu des erreurs
    im <- matrix(Dtrain[i-1, -1], nrow=28, ncol=28)
    im <- im[,order(28:1)]
    im_numbers <- apply(im, 2, as.numeric)
    image(1:28, 1:28, im_numbers, col=gray((0:255)/255))
    
    # On affiche l'indice de l'image ainsi que le résultat obtenu et celui souhaité
    print(paste0("Image ", i, " : ", SubLabelTest[i], " au lieu de ", mnist_model[i]))
    
  }
}


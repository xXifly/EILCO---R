#################################
### TP4 : Réseaux de neurones ###
#################################

library(neuralnet)

### Exercice 1 : Exemple introductif
####################################

# On génère 50 nombres aléatoires entre 1 et 100
train.input <- as.data.frame(runif(50, min=0, max=100))

# On passe à la racine ces 50 nombres
train.output <- sqrt(train.input)

# On créé un tableau de données avec ces valeurs
data.train <- cbind(train.input, train.output)
colnames(data.train) <- c("Input","Output")

# On réalise un réseau de neurones
net.sqrt <- neuralnet(Output ~ Input, data=data.train, hidden=10, threshold=0.01)
net.sqrt$result.matrix
plot(net.sqrt)

data.test <- as.data.frame((1:10)^2)
prediction <- compute(net.sqrt, data.test)

ls(prediction)
print(prediction$net.result)

result <- cbind(data.test, sqrt(data.test), prediction$net.result)
colnames(result) <- c("Entrée","Sortie attendue", "Sortie du réseau neuronnal")


### Exercice 2 : Réseau de neuronne pour la classification
##########################################################

# On charge le tableau de données
data <- read.csv("iris.csv", header=TRUE)

# On mélange la base
n <- length(data$Id)
v <- round(runif(n,0,1), digits=2)
dataR <- data[order(v),]

normalize <- function(v) {
  v <- ( v - min(v) ) / ( max(v) - min(v) )
  return(v)
}

# On normalise nos données afin d'obtenir une base de valeurs entre 0 et 1
dataN <- lapply(dataR[,c(2:5)],normalize)
dataN <- as.data.frame(dataN) 
dataN <- cbind(dataN, dataR$Species)
colnames(dataN) <- c("SepalLength", "SepalWidth", "PetalLength", "PetalWidth", "Species")

# On divise notre base normalisée en deux partie "train" et "test"
iris.app <- dataN[c(1:105),]
iris.test <- dataN[c(106:150),]

# On binarise la colonne "Species"
iris.app$setosa <- dataR$Species[1:105] == "Iris-setosa"
iris.app$virginica <- dataR$Species[1:105] == "Iris-virginica"
iris.app$versicolor <- dataR$Species[1:105] == "Iris-versicolor"
iris.app$Species <- NULL

# On réalise un réseau de neurones
net.iris <- neuralnet(setosa + virginica + versicolor  ~ SepalLength + SepalWidth + PetalLength + PetalWidth , data=iris.app, hidden=3, threshold=0.01)
plot(net.iris)
net.iris$result.matrix

names(which.max(prediction$net.result[1,]))

prediction <- compute(net.iris, iris.test)

ls(prediction)
print(prediction$net.result)

labels.predicted <- rep(0,45)

for(i in 1:45){
    labels.predicted[i]<-names(which.max(prediction$net.result[i,]))
}

labels.predicted
labels.predicted<-as.factor(labels.predicted)

labels.predicted
label.test
table(iris.test[,5],labels.predicted)


### Exercice 3 : réseau de neurones pour le régression
######################################################

data <- read.csv("gasoline.csv",header=TRUE)

# On mélange la base
n <- length(data$consumption)
v <- round(runif(n,0,1), digits=2)
dataR <- data[order(v),]

# On normalise nos données afin d'obtenir une base de valeurs entre 0 et 1
dataN <- lapply(dataR,normalize)
dataN <- as.data.frame(dataN) 

# On divise notre base normalisée en deux partie "train" et "test"
gasoline.app <- dataN[c(1:30),]
gasoline.test <- dataN[c(31:40),]

# On réalise un réseau de neurones
net.gasoline <- neuralnet(consumption ~ capacity + gasoline + hours, data=gasoline.app, hidden=c(2,1), linear.output = TRUE, threshold=0.01)
net.gasoline$reslut.matrix
plot(net.gasoline)



###################################
##### TP3 : Arbre de décision #####
###################################

##### Exercice 1 :
##################

library(rpart)

data.tennis <- read.csv("tennis.csv")

# On construit un arbre de prédiction sur la classe Jouer en fonction du Ciel, de la temperature, de l'humidité et du vent.
tree.tennis <- rpart( Jouer ~ Ciel + Temperature + Humidite + Vent, data.tennis )
# On affiche le modèle
tree.tennis

# n=14 
###   nombre d'observations
# 1) root 14 5 Oui (0.3571429 0.6428571) *
###   5 erreurs sur 14, oui avec 64% de chance de réussite

# On change les paramètres de rpart
tennis.cnt <- rpart.control(minsplit = 5)
# On recréé l'arbre
tree.tennis <- rpart( Jouer ~ Ciel + Temperature + Humidite + Vent, data.tennis, control = tennis.cnt )
tree.tennis

# On affiche l'arbre
plot(tree.tennis, branch=.2, uniform=T, compress=T)
text(tree.tennis, all=T, use.n=T, fancy=T)

# On réalise la prédiction
predict(tree.tennis, data.tennis)
# On créé la matrice de confusion
conf <- table(predict(tree.tennis, data.tennis,"class"),data.tennis$Jouer)

# On réutilise notre fonction de calcul d'erreur
calcErr <- function(v) {
  sumdiag <- 0
  for (i in c(1:length(v[,1]))) {
    sumdiag <- sumdiag + v[i,i]
  }
  err <- (1 - (sumdiag / sum(v)) ) * 100

  print(paste0("Taux d'erreur : ", err, "%"))
}

calcErr(conf)


##### Exercice 2 : Elagage
##########################

data("car.test.frame")
data.cars <- car.test.frame

head(data.cars)

str(data.cars)

### Type attribut
# Price = vectorint
# Country = factor
# Reliability = vectorint
# Mileage = vectorint
# Type = factor
# Weight = vectorint
# Disp. = vectorint
# HP = vectorint

data.cars["Nissan Maxima V6",]

plot(table(data.cars$Type))
barplot(table(data.cars$Type))

plot(data.cars$Weight,data.cars$Disp.)

cars.cnt <- rpart.control(minsplit = 1)
tree.cars <- rpart(Type ~ Price + Country + Reliability + Mileage + Weight+ Disp. + HP, data.cars, control = cars.cnt )
tree.cars
plot(tree.cars, branch=.2, uniform=T, compress=T)
text(tree.cars)

tree.cars.pruned <- prune(tree.cars,cp=0.02)
tree.cars.pruned
plot(tree.cars.pruned, uniform=T, compress=T)
text(tree.cars.pruned)
tree.cars$cptable

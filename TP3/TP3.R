###################################
##### TP3 : Arbre de décision #####
###################################

##### Exercice 1 :
##################

library(rpart)

# On charge le tableau de donn�es
data.tennis <- read.csv("tennis.csv")

# On construit un arbre de pr�diction
# Sur "Jouer" en fonction de "Temperature", "Humidit�" et "Vent"
tree.tennis <- rpart( Jouer ~ Ciel + Temperature + Humidite + Vent, data.tennis )

# On affiche le mod�le
tree.tennis

### Analyse du r�sultat obtenu :
# n=14 
###   nombre d'observations
# 1) root 14 5 Oui (0.3571429 0.6428571) *
###   5 erreurs sur 14, oui avec 64% de chance de r�ussite

# On change les param�tres de rpart
tennis.cnt <- rpart.control(minsplit = 5)

# On recr�� l'arbre avec ce fichier de controle
tree.tennis <- rpart( Jouer ~ Ciel + Temperature + Humidite + Vent, data.tennis, control = tennis.cnt )
tree.tennis

# On affiche l'arbre
plot(tree.tennis, branch=.2, uniform=T, compress=T)
text(tree.tennis, all=T, use.n=T, fancy=T)

# On r�alise la pr�diction
predict(tree.tennis, data.tennis)
# On cr�� la matrice de confusion
conf <- table(predict(tree.tennis, data.tennis,"class"),data.tennis$Jouer)

# On r�utilise notre fonction de calcul d'erreur
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

# On charge le tableau de donn�es
data("car.test.frame")
data.cars <- car.test.frame

# On observe le tableau obtenu
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

# Permet d'afficher uniquement une ligne du tableau
data.cars["Nissan Maxima V6",]

# Permet d'observer le nombre de voiture en fonction de leur type
plot(table(data.cars$Type))
barplot(table(data.cars$Type))

# Permet d'observer un nuage de point de weight en fonction de disp
plot(data.cars$Weight,data.cars$Disp.)

# On cr�� un fichier de controle
cars.cnt <- rpart.control(minsplit = 1)

# On cr�� notre arbre de pr�diction
tree.cars <- rpart(Type ~ Price + Country + Reliability + Mileage + Weight+ Disp. + HP, data.cars, control = cars.cnt )
tree.cars
plot(tree.cars, branch=.2, uniform=T, compress=T)
text(tree.cars)

tree.cars.pruned <- prune(tree.cars,cp=0.02)
tree.cars.pruned
plot(tree.cars.pruned, uniform=T, compress=T)
text(tree.cars.pruned)
tree.cars$cptable


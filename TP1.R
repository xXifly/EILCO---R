
##########################################
#####  TP1 : Régression polynomiale  #####
##########################################


## Etape 1 : générer l'ensemble d'apprentissage
###############################################

set.seed(0)

generate <- function(n, nmin, nmax)
{
  # Génère n nombres aléatoires entre nmin et nmax
  x <- round(runif(n, min = nmin, max = nmax), digits = 2);
  # Génère un vecteur y des n valeurs précédentes selon notre fonction
  y <- 0.1*x^3 - 0.5*x^2 - x + 10 + rnorm(n, mean = 0.5, sd = 1);
  # Génère une matrice de n lignes et 2 colonnes a partir de ces 2 vecteurs
  mat <- matrix(c(x,y), nrow=n, ncol=2);
  # Retourne cette matrice
  return(mat);
}

# Génère un ensemble d'apprentissage de 15 points
n <- 15;
mat <- generate(n=n, nmin=-10, nmax=10)
x <- mat[,1];
y <- mat[,2];

# Visualise les points dans un plot
plot(x,y);


## Etape 2 : réaliser un modèle de prédiction
#############################################

# Génère des régressions polynomiales de degré 1, 2, 3, 6 et 12
model1 <- lm(y ~ poly(x,1));
model2 <- lm(y ~ poly(x,2));
model3 <- lm(y ~ poly(x,3));
model6 <- lm(y ~ poly(x,6));
model12 <- lm(y ~ poly(x,12));

# Génère un vecteur de 300 valeurs allant de -10 à 10
z = seq(from = -10, to = 10, length.out = 300);

# Réalise des prédictions sur z sur chacun des 5 modèles précédants
predict(model1, data.frame(x = z));
predict(model2, data.frame(x = z));
predict(model3, data.frame(x = z));
predict(model6, data.frame(x = z));
predict(model12, data.frame(x = z));

# Affiche chacune des prédictions sur le plot
lines(z, predict(model1, data.frame(x = z)), col="green", lty=1);
lines(z, predict(model2, data.frame(x = z)), col="red", lty=1);
lines(z, predict(model3, data.frame(x = z)), col="blue", lty=1);
lines(z, predict(model6, data.frame(x = z)), col="yellow", lty=1);
lines(z, predict(model12, data.frame(x = z)), col="pink", lty=1);


## Etape 3 : sélection du modèle
################################

# Génère un ensemble de test de 1000 points
nTest <- 1000;
test <- generate(n=nTest, nmin=-10, nmax=10);
xTest <- test[,1];
yTest <- test[,2];

# Affiche ces points dans un plot
plot(xTest,yTest);

v <- seq(1,14);
EQMT <- rep (0,14);
EQMA <- rep (0,14);


for (i in v) {
  
  # Génère des régressions polynomiales de degré 1 à 14
  model <- lm(y ~ poly(x,i));
  
  # Génère l'erreur quadratique moyenne sur l'ensemble test
  gTest <- predict(model, data.frame(x = xTest));
  EQMT[i] <- sqrt(sum( (yTest - gTest )^2 ) / nTest );
  
  # Génère l'erreur quadratique moyenne sur l'ensemble d'apprentissage
  g <- predict(model, data.frame(x = x));
  EQMA[i] <- sqrt(sum( (y - g)^2 ) / n );
  
}

# On affiche nos taux d'erreurs
plot(v, EQMT, ylim=c(1,500), col="red", pch=19);
points(v, EQMA, ylim=c(1,500), col="blue", pch=19);
lines(EQMT, col="red");
lines(EQMA, col="blue");

# min(EQMT) est obtenu au degré 3
# On choisit donc une régression polynomiale de degré 3

getwd()
setwd(dir = "L:/BUT/SD/Promo 2025/smhamdi/R")
getwd()
bodies_karts = read.csv(file = "bodies_karts.csv", header = TRUE, sep = ";", dec = ",")
tires = read.csv(file = "tires.csv", header = TRUE, sep = "\t", dec = ",")
gliders = read.csv(file = "gliders.csv", header = TRUE, sep = "|", dec = ".")
drivers = read.csv(file = "drivers.csv", header = TRUE, sep = ";", dec = ",")
dim(bodies_karts)
dim(tires)
dim(gliders)
dim(drivers)
summary(bodies_karts)
summary(tires)
summary(gliders)
summary(drivers)
plot(x = drivers$Weight, y = drivers$Acceleration,
     main = "liens statistiques de drivers : taille / acceleration ")
coeffcor = cor(x = drivers$Weight, y = drivers$Acceleration) #-1 a 1 prcq l'influence negative
cov(x = drivers$Weight, y = drivers$Acceleration)
sx = sd(drivers$Weight)
sy = sd(drivers$Acceleration)
coefdeter = coeffcor^2
print(coefdeter)
matricecor = cor(drivers[ , -1])
matricecor = round(matricecor,2)
View(matriceCor)
install.packages("corrplot")
library(corrplot)
corrplot(matricecor, method="circle")
matricecor = round(cor(tires[ , - 1]),1)
#sert a montrer une matrice specifique graphique de correlation
corrplot(matricecor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "pink", # Ajout du coefficient de corrélation
         tl.col="purple", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE )
corrplot(matricecor, method="circle")
matricecor = round(cor(bodies_karts[ , - 1]),1)
corrplot(matricecor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "red", # Ajout du coefficient de corrélation
         tl.col="purple", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE )
corrplot(matricecor, method="circle")
matricecor = round(cor(gliders[ , - 1]),1)
corrplot(matricecor, method="color",  
         type="upper", order="hclust", 
         addCoef.col = "pink", # Ajout du coefficient de corrélation
         tl.col="purple", tl.srt=45, #Rotation des étiquettes de textes
         # Cacher les coefficients de corrélation sur la diagonale
         diag=FALSE )
#exercice 3
resultat = drivers[ , c("Driver", "Weight")]
View(resultat)
resultat = drivers[ 1:10, c("Driver", "Weight")]
View(resultat)
resultat = drivers[ , c(5,7,9)]
View(resultat)
resultat = drivers[ , -c("Weight","Acceleration")] #cela fonctionne uniquement sur des index numériques.
resultat = drivers[ , -c(2,3)]
View(resultat)
resultat = drivers[ , c("Driver", "Acceleration", "Weight")]
View(resultat)
#Les colonnes sont dans l'ordre défini par le vecteur.
resultat = drivers[ c(3,12,32) , ]
View(resultat)
resultat = drivers[ c(32,3,12) , ]
View(resultat)
#Les lignes sont dans l'ordre défini par le vecteur.
rang = order(drivers$Weight)
resultat = drivers[ rang  , c("Driver", "Weight") ]
View(resultat)
rang = order(drivers$Acceleration, decreasing = TRUE)
resultat = drivers[ rang  , c("Driver", "Acceleration") ]
View(resultat)
rang = order(drivers$Acceleration, drivers$Weight, decreasing = c(TRUE,FALSE))
resultat = drivers[ rang  , c("Driver", "Acceleration","Weight") ]
View(resultat)
#Exercice 4 - goat
help(subset)
topDriver = subset(x = drivers,
                   subset = Acceleration == max(Acceleration), 
                   select = c("Driver","Acceleration"))
topGlider = subset(x = gliders,
                   subset = Acceleration == max(Acceleration), 
                   select = c("Glider","Acceleration"))
topTires = subset(x = tires,
                  subset = Acceleration == max(Acceleration), 
                  select = c("Tire","Acceleration"))
topBody = subset(x = bodies_karts,
                 subset = Acceleration == max(Acceleration), 
                 select = c("Body","Acceleration"))

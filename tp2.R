df = read.csv("fao.csv", sep=";", dec=",", header = TRUE)
#exo1
dim(df)
#ou
nrow(df)
summary(df)
#exo2
mean(df$Dispo_alim)
sum(df$Population, na.rm=TRUE)
sd(df$Export_viande, na.rm=TRUE)
sd(df$Import_viande, na.rm=TRUE)
median(df$Prod_viande, na.rm=TRUE)
quantile(df$Dispo_alim, na.rm=TRUE)
quantile(df$Import_viande, seq(0,1,0.1))
#exercice 3 
rang = order(df$Population)
resultat = head(df[ rang , ], n = 5)
View(resultat)
rang = order(df$Population, decreasing = TRUE )
resultat = head(df[ rang , ], n = 5)
View(resultat)
rang = order(df$Prod_viande, decreasing = TRUE)
resultat = head(df[ rang, ], n = 5)
View(resultat)
rang = order(df$Import_viande, decreasing = TRUE)
resultat = head(df[ rang, ], n = 5)
View(resultat)
resultat = subset(df, (Dispo_alim > 3500) & (Import_viande < 1000))
View(resultat)
resultat = subset(df , Nom %in% c("France","Belgique"))
View(resultat)
#exercice 4
df$Part_export = df$Export_viande/df$Prod_viande
df$Dispo_alim_pays = df$Dispo_alim*df$Population
write.table(x = df, file = "ExportTp2.csv")
dispo_alim_mondiale = sum(df$Dispo_alim_pays, na.rm=TRUE)
dispo_alim_mondiale
dispo_alim_mondiale/2300
#exercice 5
plot( x = df$Prod_viande, y = df$Export_viande, main = "Pays : Prod_viande / Export_viande" )
coefcor = cor( x= df$Prod_viande , y = df$Export_viande, use="complete.obs")
matriceCor = cor(df[ , - 1] , use = "complete.obs")
matriceCor = round(matriceCor , 2)
View(matriceCor)
install.packages("corrplot")
library(corrplot) 
corrplot(matriceCor, method="circle")

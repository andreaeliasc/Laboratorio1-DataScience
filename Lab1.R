#Andrea Elias
#Diego Estrada 

library(dplyr)
library(tidyr)
library(rpart)
library(caret)
library(tree)
library(rpart.plot)
library(randomForest)
library(dplyr)
library(tidyr)
library(cluster)
library(e1071)
library(mclust)
library(fpc)
library(NbClust)
library(factoextra)
library(cluster)
library(e1071)
library(mclust)
library(fpc)
library(NbClust)
library(factoextra)
library(rpart)
library(rela)
library(psych)
library(FactoMineR)
library(arules)

# Analisis Exploratorio
train<- read.csv("train.csv", stringsAsFactors = FALSE)
test<- read.csv("test.csv", stringsAsFactors = FALSE)
train<-train[1:1460,]

glimpse(train[1:10,])

#graficas de correlacion de las variables que se utilizan vs el precio de compra
scatter.smooth(train$LotFrontage, train$SalePrice)
scatter.smooth(train$LotArea, train$SalePrice)
scatter.smooth(train$GrLivArea, train$SalePrice)
scatter.smooth(train$YearBuilt, train$SalePrice)
scatter.smooth(train$BsmtUnfSF, train$SalePrice)
scatter.smooth(train$TotalBsmtSF, train$SalePrice)
scatter.smooth(train$X1stFlrSF, train$SalePrice)
scatter.smooth(train$GarageYrBlt, train$SalePrice)
scatter.smooth(train$GarageArea, train$SalePrice)
scatter.smooth(train$YearRemodAdd, train$SalePrice)

scatter.smooth(train$TotRmsAbvGrd, train$SalePrice)
scatter.smooth(train$MoSold, train$SalePrice)
scatter.smooth(train$OverallQual, train$SalePrice)


#Diagramas de cajas y bigotes
boxplot(train$LotFrontage)
boxplot(train$LotArea)
boxplot(train$OverallQual)
boxplot(train$GrLivArea)
boxplot(train$YearBuilt)
boxplot(train$BsmtUnfSF)
boxplot(train$TotalBsmtSF)
boxplot(train$TotRmsAbvGrd)
boxplot(train$GarageYrBlt)
boxplot(train$GarageArea)
boxplot(train$MoSold)
boxplot(train$YearRemodAdd)
boxplot(train$SalePrice)

#Histogramas
hist(train$BsmtUnfSF, main="Variable BsmtUnfSF")
hist(train$TotalBsmtSF, main="Variable TotalBsmtSF")
hist(train$TotRmsAbvGrd, main="Variable TotRmsAbvGrd")
hist(train$GarageYrBlt, main="Variable GarageYrBlt")
hist(train$GarageArea, main="Variable GarageArea")
hist(train$MoSold, main="Variable MoSold")
hist(train$YearRemodAdd, main="Variable YearRemodAdd")
hist(train$SalePrice, main="Variable SalePrice")

#Correlacion
matriz_cor <- cor(trainFilteredData[,-1],use = "pairwise.complete.obs")
matriz_cor
corrplot(matriz_cor) #mapa de calor

#Tablas de frecuencias absolutas, proporción y barras.

View(table(train$MoSold))
View(table(train$MoSold)/length(train$MoSold))
plot(x = table(train$MoSold), main = "Gráfica de MoSold",
     xlab = "Valor de MoSold", ylab = "Frecuencia", 
     col = c("red", "blue", "grey", "purple"))

View(table(train$OverallQual))
View(table(train$OverallQual)/length(train$OverallQual))
plot(x = table(train$OverallQual), main = "Gráfica de OverallQual",
     xlab = "Valor de OverallQual", ylab = "Frecuencia", 
     col = c("red", "blue", "grey", "purple"))

View(table(train$YearBuilt))
View(table(train$YearBuilt)/length(train$YearBuilt))
plot(x = table(train$YearBuilt), main = "Gráfica de YearBuilt",
     xlab = "Valor de YearBuilt", ylab = "Frecuencia", 
     col = c("red", "blue", "grey", "purple"))

View(table(train$GarageYrBlt))
View(table(train$GarageYrBlt)/length(train$GarageYrBlt))
plot(x = table(train$GarageYrBlt), main = "Gráfica de GarageYrBlt",
     xlab = "Valor de GarageYrBlt", ylab = "Frecuencia", 
     col = c("red", "blue", "grey", "purple"))

View(table(train$YearRemodAdd))
View(table(train$YearRemodAdd)/length(train$YearRemodAdd))
plot(x = table(train$YearRemodAdd), main = "Gráfica de YearRemodAdd",
     xlab = "Valor de YearRemodAdd", ylab = "Frecuencia", 
     col = c("red", "blue", "grey", "purple"))



pafDatos$KMO
pafDatos$Bartlett
summary(pafDatos)

cor(train[,-1],use = "pairwise.complete.obs")

compPrinc<-prcomp(train[,1:14], scale = T)
compPrinc
summary(compPrinc)

compPrincPCA<-PCA(train[,-1],ncp=ncol(train[,-1]), scale.unit = T)

summary(compPrincPCA)


setCualitativa<-(datos[,c("OverallQual","YearBuilt","GarageYrBlt","MoSold","YearRemodAdd")])
dataframe_train<-as(setCualitativa,"transactions")
itemsets<-apriori(data=dataframe_train, parameter = list(
  minlen=2,
  target="rules",
  confidence=0.85
))

summary(itemsets)
inspect(itemsets)

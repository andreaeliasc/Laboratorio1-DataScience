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

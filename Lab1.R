# Project: Laboratorio 1 Data Science
# Authors: Abril Palencia 18198, Cesar Rodas 16776
# Date: 2021-07-23

library("corrplot")
library("arules")

#leer data train.csv
dataSet<-read.csv("./data/train.csv")
#imprimir dataSet
str(dataSet)
#resumen del dataSet
summary(dataSet)

#se genera el codebook del dataset para el analisis de las variables
library(dataMaid)
#makeCodebook(dataSet) #para ejecutar este comando hay que borrar el archivo de word del repo

# variables cuantitativas
cuantitativas<-data.frame(
  "Id" = dataSet$Id,
  "LotFrontage" = dataSet$LotFrontage,
  "LotArea" = dataSet$LotArea,
  "OverallQual" = dataSet$OverallQual,
  "OverallCond" = dataSet$OverallCond,
  "YearBuilt" = dataSet$YearBuilt,
  "YearRemodAdd" = dataSet$YearRemodAdd,
  "MasVnrArea" = dataSet$MasVnrArea,
  "BsmtFinSF1" = dataSet$BsmtFinSF1,
  "BsmtFinSF2" = dataSet$BsmtFinSF2,
  "BsmtUnfSF" = dataSet$BsmtUnfSF,
  "TotalBsmtSF" = dataSet$TotalBsmtSF,
  "X1stFlrSF" = dataSet$X1stFlrSF,
  "X2ndFlrSF" = dataSet$X2ndFlrSF, 
  "GrLivArea" = dataSet$GrLivArea,
  "BsmtFullBath" = dataSet$BsmtFullBath,
  "BsmtHalfBath" = dataSet$BsmtHalfBath,
  "FullBath" = dataSet$FullBath,
  "HalfBath" = dataSet$HalfBath,
  "BedroomAbvGr" = dataSet$BedroomAbvGr,
  "KitchenAbvGr" = dataSet$KitchenAbvGr, 
  "TotRmsAbvGrd" = dataSet$TotRmsAbvGrd,
  "Fireplaces" = dataSet$Fireplaces,
  "GarageYrBlt" = dataSet$GarageYrBlt,
  "GarageCars" = dataSet$GarageCars,
  "GarageArea" = dataSet$GarageArea,
  "WoodDeckSF" = dataSet$WoodDeckSF,
  "OpenPorchSF" = dataSet$OpenPorchSF ,
  "EnclosedPorch" = dataSet$EnclosedPorch,
  "X3SsnPorch" = dataSet$X3SsnPorch,
  "ScreenPorch" = dataSet$ScreenPorch,
  "PoolArea" = dataSet$PoolArea,
  "MiscVal" = dataSet$MiscVal,
  "YrSold" = dataSet$YrSold,
  "SalePrice" = dataSet$SalePrice
)

# correlación de variables cuantitativas
cor(cuantitativas)

# matriz de correlación
variables <- na.omit(cuantitativas)
variables <-   cor(variables,method  ="pearson");
round (variables, digits=2)
corrplot(variables,tl.col = "black",tl.srt = 45)

# PCA
library(rela)
library(psych)
library(FactoMineR)
library(fpc)
library(factoextra)

pafVariables <- paf(as.matrix(variables))
pafVariables$KMO 

#Reglas de asociacion
# El mínimo nivel de soporte y confianza aceptados
reglas<- apriori(variables, parameter = list(support = 0.5, confidence = 0.70, target = "rules"))

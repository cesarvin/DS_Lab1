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

# variables cualitativas 

dataSet$MSZoning <- as.factor(dataSet$MSZoning)
dataSet$Street <- as.factor(dataSet$Street)
dataSet$Alley <- as.factor(dataSet$Alley)
dataSet$LotShape <- as.factor(dataSet$LotShape)
dataSet$LandContour <- as.factor(dataSet$LandContour)
dataSet$Utilities <- as.factor(dataSet$Utilities)
dataSet$LotConfig <- as.factor(dataSet$LotConfig)
dataSet$LandSlope <- as.factor(dataSet$LandSlope)
dataSet$Neighborhood <- as.factor(dataSet$Neighborhood)
dataSet$Condition1 <- as.factor(dataSet$Condition1)
dataSet$Condition2 <- as.factor(dataSet$Condition2)
dataSet$BldgType <- as.factor(dataSet$BldgType)
dataSet$HouseStyle <- as.factor(dataSet$HouseStyle)
dataSet$RoofStyle <- as.factor(dataSet$RoofStyle)
dataSet$RoofMatl <- as.factor(dataSet$RoofMatl)
dataSet$Exterior1st <- as.factor(dataSet$Exterior1st)
dataSet$Exterior2nd <- as.factor(dataSet$Exterior2nd)
dataSet$MasVnrType <- as.factor(dataSet$MasVnrType)
dataSet$ExterQual <- as.factor(dataSet$ExterQual)
dataSet$ExterCond <- as.factor(dataSet$ExterCond)
dataSet$Foundation <- as.factor(dataSet$Foundation)
dataSet$BsmtQual <- as.factor(dataSet$BsmtQual)
dataSet$BsmtCond <- as.factor(dataSet$BsmtCond)
dataSet$BsmtExposure <- as.factor(dataSet$BsmtExposure)
dataSet$BsmtFinType1 <- as.factor(dataSet$BsmtFinType1)
dataSet$BsmtFinType2 <- as.factor(dataSet$BsmtFinType2)
dataSet$Heating <- as.factor(dataSet$Heating)
dataSet$HeatingQC <- as.factor(dataSet$HeatingQC)
dataSet$CentralAir <- as.factor(dataSet$CentralAir)
dataSet$Electrical <- as.factor(dataSet$Electrical)
dataSet$KitchenQual <- as.factor(dataSet$KitchenQual)
dataSet$Functional <- as.factor(dataSet$Functional)
dataSet$FireplaceQu <- as.factor(dataSet$FireplaceQu)
dataSet$GarageType <- as.factor(dataSet$GarageType)
dataSet$GarageYrBlt <- as.factor(dataSet$GarageYrBlt)
dataSet$GarageFinish <- as.factor(dataSet$GarageFinish)
dataSet$GarageQual <- as.factor(dataSet$GarageQual)
dataSet$GarageCond <- as.factor(dataSet$GarageCond)
dataSet$PavedDrive <- as.factor(dataSet$PavedDrive)
dataSet$PoolQC <- as.factor(dataSet$PoolQC)
dataSet$Fence <- as.factor(dataSet$Fence)
dataSet$MiscFeature <- as.factor(dataSet$MiscFeature)
dataSet$SaleType <- as.factor(dataSet$SaleType)
dataSet$SaleCondition <- as.factor(dataSet$SaleCondition)

categoricas <- data.frame(
  "MSZoning" = dataSet$MSZoning,
  "Street" = dataSet$Street,
  "Alley" = dataSet$Alley,
  "LotShape" = dataSet$LotShape,
  "LandContour" = dataSet$LandContour,
  "Utilities" = dataSet$Utilities,
  "LotConfig" = dataSet$LotConfig,
  "LandSlope" = dataSet$LandSlope,
  "Neighborhood" = dataSet$Neighborhood,
  "Condition1" = dataSet$Condition1,
  "Condition2" = dataSet$Condition2,
  "BldgType" = dataSet$BldgType,
  "HouseStyle" = dataSet$HouseStyle,
  "RoofStyle" = dataSet$RoofStyle,
  "RoofMatl" = dataSet$RoofMatl,
  "Exterior1st" = dataSet$Exterior1st,
  "Exterior2nd" = dataSet$Exterior2nd,
  "MasVnrType" = dataSet$MasVnrType,
  "ExterQual" = dataSet$ExterQual,
  "ExterCond" = dataSet$ExterCond,
  "Foundation" = dataSet$Foundation,
  "BsmtQual" = dataSet$BsmtQual,
  "BsmtCond" = dataSet$BsmtCond,
  "BsmtExposure" = dataSet$BsmtExposure,
  "BsmtFinType1" = dataSet$BsmtFinType1,
  "BsmtFinType2" = dataSet$BsmtFinType2,
  "Heating" = dataSet$Heating,
  "HeatingQC" = dataSet$HeatingQC,
  "CentralAir" = dataSet$CentralAir,
  "Electrical" = dataSet$Electrical,
  "KitchenQual" = dataSet$KitchenQual,
  "Functional" = dataSet$Functional,
  "FireplaceQu" = dataSet$FireplaceQu,
  "GarageType" = dataSet$GarageType,
  "GarageYrBlt" = dataSet$GarageYrBlt,
  "GarageFinish" = dataSet$GarageFinish,
  "GarageQual" = dataSet$GarageQual,
  "GarageCond" = dataSet$GarageCond,
  "PavedDrive" = dataSet$PavedDrive,
  "PoolQC" = dataSet$PoolQC,
  "Fence" = dataSet$Fence,
  "MiscFeature" = dataSet$MiscFeature,
  "SaleType" = dataSet$SaleType,
  "SaleCondition" = dataSet$SaleCondition
)

reglas<- apriori(categoricas, parameter = list(support = 0.2, confidence = 0.70, target = "rules"))

inspect(reglas)

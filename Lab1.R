# Project: Laboratorio 1 Data Science
# Authors: Abril Palencia 18198, Cesar Rodas 16776
# Date: 2021-07-23

#leer data train.csv
dataSet<-read.csv("./data/train.csv")
#imprimir dataSet
str(dataSet)
#resumen del dataSet
summary(dataSet)

#codebool del dataSet
library(dataMaid)
makeCodebook(dataSet)
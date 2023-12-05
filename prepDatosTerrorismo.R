# Preprocesamiento de datos del dataset. Da un nuevo conjunto de datos
library(missForest)
library(dplyr)
library(mice)
library(Amelia)


data <- read.csv("/content/ruta1.csv", header=T, na.strings=c("","NA"))
View(data)
dim(data)
names <- colnames(data)

#Revisamos cuales columnas tienen mayor número de outliers
for (x in 1:ncol(data)) {
  if(is.numeric(data[, x])){ 
    cat(x, names[x])
    boxplot(data[,x])
  }
}


#Revisamos outliers por columna y eliminamos en base a eso
outliers <- c(4, 5, 8, 10, 22, 27, 38)
for (x in outliers) {
  print(x)
  if(is.numeric(data[, x])){ 
    # Obtenemos los cuartiles de cada columna
    Q1 <- quantile(data[, x], .25)
    Q3 <- quantile(data[, x], .75)
    IQR <- IQR(data[, x])  

    # Eliminamos outliers según cuartiles
    no_outliers <- subset(data, data[, x]> (Q1 - 1.5*IQR) & data[, x]< (Q3 + 1.5*IQR))
  }
}
View(no_outliers)
dim(data)
dim(no_outliers) 

write.csv(no_outliers, "datosPrepTerrorismo.csv", row.names=TRUE)

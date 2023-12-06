# Preprocesamiento de datos del dataset. Da un nuevo conjunto de datos
#Los archivos que escribimos estan comentados para evitar multiples copias, o no obtenerlas en caso de nos ser necesarias
#Pero las mantenemos para el reporte

library(missForest)
library(dplyr)
library(mice)
library(Amelia)


# Imputacion de datos
#### Preprocesamiento
library(missForest)
library(dplyr)
library(mice)
library(Amelia)
data <- read.csv("/content/cleanTerrorismV2.csv", header=T, na.strings=c("","NA"))


## Para el preprocesamiento, iniciamos con la imputacion de valores.
num <- select_if(data, is.numeric)
names <- colnames(num)


# Eliminamos summary debido a que no podemos inputar en esta columna
datos <- subset(data, select = -c(summary))
# Realizamos la imputación
db_imputados <- amelia(datos, m=1, parallel = "multicore", idvars =  c("country_txt", "region_txt", "provstate", "city", "alternative_txt", "attacktype1_txt", "targtype1_txt", "targsubtype1_txt", "corp1", "target1", "natlty1_txt", "gname", "weaptype1_txt") )
db_imputados
# Para tener acceso a las salidas imputadas
db_imputados$imputations[[1]]
#db_imputados$imputations[[2]]
#db_imputados$imputations[[3]]
#db_imputados$imputations[[4]]
#db_imputados$imputations[[5]]

# Para verificar una columna en particular en un conjunto de datos, use los siguientes
# comandos

#db_imputados$imputations[[5]]

#mice_plot <- aggr(db_imputados$imputations[[1]], col=c('navyblue','yellow'),
 #                 numbers=TRUE, sortVars=TRUE,
  #                labels=names(datos ), cex.axis=.6,cex.numbers=0.5,
   #               gap=3, ylab=c("Datos perdidos","Patrón"))

# Exportar la salida en un archivo CSV
#write.amelia(db_imputados, file.stem = "ruta")

#Seleccionamos la primera version de datos imputados
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

#write.csv(no_outliers, "datosPrepTerrorismo.csv", row.names=TRUE)


## Delimitamos el proyecto a los objetivos preestablecidos 
data <- read.csv("/content/no_outliers.csv", header=T, na.strings=c("","NA"))
afgData <- subset(data, data$country_txt == "Afghanistan")
#View(afgData)

#Debido a los multiples subsets realizazdos, obtuvimos columnas de indices extra, por lo que las
#eliminamos
afgData <- afgData[,-1]
colnames(afgData)
dim(afgData)
afgData <- afgData[,-1]
colnames(afgData)
dim(afgData)
afgData <- afgData[,-1]
colnames(afgData)
dim(afgData)


#Ya que el proyecto fue delimitado a afghanistasn y los años 2000 en adelante, 
#eliminamos aslgunas columnas redundantes y las filas que no nos interesan
afgData <- afgData[,-which( colnames(afgData)=="country_txt" )]
dim(afgData)
colnames(afgData)
afgData <- afgData[,-which( colnames(afgData)=="country" )]
dim(afgData)
colnames(afgData)

afgData <- afgData[,-which( colnames(afgData)=="region" )]
dim(afgData)
colnames(afgData)
afgData <- afgData[,-which( colnames(afgData)=="region_txt" )]
dim(afgData)
colnames(afgData)

afgData <- subset(afgData, iyear > 2000)
dim(afgData)


#write.csv(afgData, "afgData.csv", row.names=TRUE)

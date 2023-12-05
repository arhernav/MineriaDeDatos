# Analisis exploratorio de datos
library(readr)
library(vroom)
library(dplyr)
library(xtable)
library(ggplot2)



########### Conocimiento de datos
###########
###########

# Por recomendacion, previo a iniciar el proyecto reducimos el número de columnas del dataset

#Leemos el dataset de forma que los valores vacios pasen a ser NA
data <- read.csv("/content/globalterrorismdb_0718dist.csv", header=T, na.strings=c("","NA"))

#Obtenemos los nombres de las columnas para revisar cuales son importantes
#Por cada columna en el subconjunto de trabajo, obtenemos su nombre y porcentaje de valores faltantes
for (x in length(colnames(data))) {
    print(paste(x, names[x], mean(is.na(workSub[, x]))))
    print("############")
}

#Decidimos eliminar las siguientes columnas debido a su cantidad de valores perdidos
#Columanas 5, 7, 14, 15, 18, 31:34
#Columnas subset(data, select = -c(43:58, 60:65,67))
#Columnas neo_data <- data[, -c(68:82, 84:98, 100, 101)]
#Columnas subset(workSub, select = -c(# [104:135] )) ->
cleanData <- subset(data, select = -c(5, 7, 14, 15, 18, 31:34,43:58, 60:65,67,68:82, 84:98, 100, 101,104:135))
names <- colnames(cleanData)
print(names) Columnas restantes

View(cleanData)
print(nrow(cleanData))

cleanData <- subset(cleanData, iyear > 1996) #Ademas, por recomendacion, reducimos los años que abarcamos
print(nrow(cleanData))
#write.csv(cleanData, "cleanTerrorismV2.csv", row.names=TRUE)

#Para continuar con la exploracion de datos, obtenemos información del conjunto restante

data <- read.csv("/content/cleanTerrorismV2.csv", header=T, na.strings=c("","NA"))
names <- colnames(data)



#' columna categorica
#' En caso de ser una columna categorica, imprime información importante
#' Imprime categorias, número de categorias,
#' porcentaje de apariciones por categoria, y da la tabla en latex
#' @param colNumber Indice de la columna de la que se imprimer la información
columnaCategorica <- function(colNumber){
  print(names[colNumber])
  tempFact = factor(data[, colNumber])
  print(levels(tempFact))
  print(length(levels(tempFact)))
  barplot(prop.table(table(tempFact)))
  frequencys <- table(tempFact)/sum(table(tempFact))
  print(as.table(frequencys))
  xt <- xtable(frequencys)
  print.xtable(xt)
  print(sum(frequencys))
  frequencys
}

#' frequencyTable
#' Devuelve la tabla de freceuncias de las categorias de una columna
#' Imprime la barplot con el número de apariciones de cada categoria
#' @param colNumber Indice de la columna de la que se imprimer la información
frequencyTable <- function(colNumber){
  frequency_table <- table(data[, colNumber])
  barplot(frequency_table,
       col = "blue",
       xlab = "Category",
       ylab = "Frequency",
       main = "Frequency of Categories")
}



sep="\n\n" # Necesario para poder imrpimir separaciones
for (x in 1:length(names)) {
  cat(sep)
  # Espacio para imprimir
  cat(x, names[x], "\n") #nombre de la columna y número de columna
  cat(class(data[, x]), "\n") #Tipo de la columna
  cat("Valores perdidos (%)", mean(is.na(data[, x])), "\n") #Porcentaje de valores perdidos

  if(is.numeric(data[, x])){  #Valores que aplican solo a columnas númericas
    print("numeric")
    cat("\t", "min: ", min(data[, x], na.rm =T), "\n")
    cat("\t", "Max: ", max(data[, x], na.rm =T), "\n")
    cat("\t", "Media: ", mean(data[, x], na.rm =T), "\n")
    cat("\t", "Desciación estandar:", sd(data[, x], na.rm =T), "\n")
    cat("\t", "Valores atipicos: ", "\n")
    print(hist(data[, x]))
    cat("\t", "Tipo de distribución: \n")
    boxplot(data[, x], horizontal = T)
  }

  if(class(data[, x]) == "character" ){
    print(cat("Revisar si es columna categorica", x))
  }


 ##
  print("##########")
  cat(sep)
}

#Para obtener las correlaciones entre columnas, obtenemos todas las númericas y 
#sacamos la matriz de correlacion

num <- select_if(data, is.numeric)
names <- colnames(num)
cor_matrix_df <- cor(num)
cor_matrix_df
xt <- xtable(cor_matrix_df)
print.xtable(xt)

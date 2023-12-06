# Minado de datos - agrupación 

data <- read.csv("afgData.csv", header=T, na.strings=c("","NA"))

# Instalar los paquetes si no están instalados previamente
install.packages(c("readxl", "tibble", "tidyverse", "cluster", "factoextra", "NbClust", "tidyr", "textshape"))

# Cargar los paquetes
library(readxl)
library(tibble)
library(tidyverse)
library(cluster)
library(factoextra)
library(NbClust)
library(tidyr)
library(textshape)
library(dplyr)

# Seleccionar solo las columnas numéricas
data_numeric <- data %>% select_if(is.numeric)

# Normalizar 
data_scaled <- scale(data_numeric)

# Mostrar las cabeceras del dataframe normalizado
head(data_scaled)

#estimar el número de clústers
fviz_nbclust(data_scaled, kmeans, method = "silhouette")

#con el resultado anterior observamos que el mejor candidato es 4
#por lo tanto, calculamos a 4 clústers
k4 <- kmeans(data_scaled, centers = 4, nstart = 25)
k4
str(k4)

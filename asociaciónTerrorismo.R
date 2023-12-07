# Minado de datos - Asociación
#install.packages("plyr")
#install.packages("sf")
#install.packages("arulesViz")
library(sf) #Simplemente para obtener el %>%
library(plyr) #Permite preprocesar el set para obtener la forma de 
library(arules) #Paqueteria con reglas
library(arulesViz) #Visualización de las reglas. No se usa en el script

# Leemos el archivo con la información a usar
data <- read.csv("/content/afgData.csv",  header=TRUE)

# Ya que necesiatamos realizar un preprocesamiento para usar apriori, sacamos el conjunto de columnas 
# a usar
supp <- subset(data, select=c('gname', 'weaptype1_txt'))
#supp

#Damos la forma de "basket" para las transacciones
listaApriori <- supp %>% mutate(apriori = paste(gname,weaptype1_txt, sep = ","))
listaApriori <- subset(listaApriori, select=c('apriori')) 
#listaApriori


#Extraemos para obtener el formato de transacciones y poder leerlo con read.transactions
write.csv(listaApriori, "aprioiri.csv", quote=FALSE, row.names=FALSE)
transacciones <- read.transactions("/content/aprioiri.csv", format = "basket", sep = ",", header=TRUE, rm.duplicates=TRUE)
inspect(transacciones[1:100])
itemFrequencyPlot(transacciones,topN = 20, type = "relative", main="Grafica de frecuencia relativa de items")

#Damos un soporte bajo para poder obtener un mayor número de reglas y sacamos las de mas confianza
# 0.5 es la mayor confianza que podemos encontrar con asociación
reglas <- apriori(transacciones, parameter =list(supp= 0.01, conf= 0.5) )
reglas <- sort(reglas, by="confidence", decreasing = TRUE)

#Verificamos la estructura de las reglas obtenidas
str(reglas)
inspect(reglas)

print("Duplicadas:")
duplicated(reglas)#Podemos ver que no hay reglas duplicadas

print("Redundantes:")
redundantes <- is.redundant(reglas)
redundantes #Podemos ver que no hay reglas redundantes



#Realizazmos la evaluación de las reglas obtenidas para obtener su lift y coverage
metricas <- interestMeasure(reglas, measure = c("coverage", "fishersExactTest"),
                            transactions = transacciones)

metricas <- interestMeasure(reglas, measure = c("fishersExactTest"),
                            transactions = transacciones)
metricas



quality(reglas) <- cbind(quality(reglas), metricas)
# inspect(sort(x = reglas, decreasing = TRUE, by = "confidence"))
df_reglas <- as(reglas, Class = "data.frame") 
df_reglas #Resultado final de las reglas obtenidas

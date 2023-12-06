# Minado de datos - Asociación

#Paquetes para instalar
install.packages("tidyverse")
install.packages("arules")
install.packages("arulesViz")
install.packages("RColorBrewer")

#Cargamos los datos para el ejemplo
library(tidyverse)
file.choose()
datDis <- read.csv( "C:\\Users\\Francisco\\Videos\\arbolVino\\afgDataDisc.csv")
datDis <- datDis[,-1]

head(datDis)

# Cada l?nea del archivo contiene la informaci?n de un item y el identificador de la 
# transacci?n a la que pertenece.

#Analicemos una transacci?n
datDis %>% filter(guncertain1 == 1) %>% pull(nwound)



library(arules)
transactionK <- read.transactions(file =  "C:\\Users\\Francisco\\Videos\\arbolVino\\afgDataDisc.csv",
                                   format = "single",
                                   sep = ",",
                                   header = TRUE,
                                   cols = c("gname", "nwound"),
                                   rm.duplicates = TRUE)
transactionK

#Observemos el formato de informaci?n que hemos cargado:
colnames(transactionK)[1:5]
rownames(transactionK)[1:5]
# ==============================================================================
#EXPLORACIÓN DE ITEMS
# ==============================================================================

inspect(transactionK[1:5])

# Tambi?n es posible mostrar los resultados en formato de dataframe con la funci?n
# DATAFRAME() o con as(transacciones, "dataframe")

df_transacciones <- as(transactionK, Class = "data.frame")

# Para que el tama?o de la tabla se ajuste mejor, se convierte el dataframe a tibble
as_tibble(df_transacciones) %>% head()

#Para extraer el tama?o de cada transacci?n se emplea la funci?n size().
tamano <- size(transactionK)
summary(tamano)

#Vamos a graficar el tama?o de las transacciones:
data.frame(tamano) %>%
  ggplot(aes(x = tamano)) +
  geom_histogram() +
  labs(title = "Distribucion del tamaño de las transacciones",
       x = "Tamaño") +
  theme_bw()

quantile(tamano,prob = seq(0,1,0.1))


frecuencia_items <- itemFrequency(x = transactionK, type = "relative")

# Podemos generar una gr?fica de barras de frecuencia, para ver la distribuci?n de
# objetos: cuantas veces aparece un producto en comparaci?n con otros.
library(RColorBrewer)
itemFrequencyPlot(transactionK,topN = 20, type = "relative",
                  col = brewer.pal(8,'Pastel2'),
                  main="Grafica de frecuencia relativa de items")

frecuencia_items %>% sort(decreasing = TRUE) %>% head(5)

# Si se indica el argumento type = "absolute", la funci?n itemFrequency() devuelve el
# n?mero de transacciones en las que aparece cada item.

frecuencia_items <- itemFrequency(x = transactionK, type = "absolute")

# Podemos generar una gr?fica de barras de frecuencia, para ver la distribuci?n de
# objetos
itemFrequencyPlot(transactionK,topN = 20, type = "absolute",
                  col = brewer.pal(8,'Pastel2'),
                  main="Grafica de frecuencia absoluta de items")

frecuencia_items %>% sort(decreasing = TRUE) %>% head(5)


# ==============================================================================
#ITEMSETS
# ==============================================================================
soporte <- 10 / dim(transactionK)[1]
soporte
itemsets <- apriori(data = transactionK,
                    parameter = list(support = soporte,
                                     minlen = 1,
                                     maxlen = 20,
                                     target = "frequent itemset"))
summary(itemsets)

# En el siguiente listado se muestran los 20 itemsets con mayor soporte que, son
# los formados por items individuales (los itemsets de menor tama?o).

# Se muestran los top 20 itemsets de mayor a menor soporte
top_2_itemsets <- sort(itemsets, by = "support", decreasing = TRUE)[1:2]
inspect(top_2_itemsets)

# Para representarlos con ggplot se convierte a dataframe 
as(top_2_itemsets, Class = "data.frame") %>%
  ggplot(aes(x = reorder(items, support), y = support)) +
  geom_col() +
  coord_flip() +
  labs(title = "Itemsets m?s frecuentes", x = "itemsets") +
  theme_bw()
# Se muestran los 20 itemsets m?s frecuentes formados por m?s de un item.
inspect(sort(itemsets[size(itemsets) > 0], decreasing = TRUE)[1:2])


# Puede observarse que muchos itemsets est?n a su vez contenidos en itemsets de orden
# superior, es decir, existen itemsets que son subsets de otros. Para identificar cu?les
# son, o cuales no lo son, se puede emplear la funci?n is.subset().

# Para encontrar los subsets dentro de un conjunto de itemsets, se compara el
# conjunto de itemsets con sigo mismo.
subsets <- is.subset(x = itemsets, y = itemsets, sparse = FALSE)

# Para conocer el total de itemsets que son subsets de otros itemsets se cuenta el n?mero
# total de TRUE en la matriz resultante.

# La suma de una matriz l?gica devuelve el n?mero de TRUEs
sum(subsets)

# ==============================================================================
# REGLAS DE ASOCIACIóN
# ==============================================================================

# Se cre? una lista de posibles valores de niveles de Apoyo y Confianza.
soporteLev <- c( 0.1, 0.25)
confianzaLev <- c(0.9, 0.8, 0.7, 0.6, 0.5, 0.4, 0.3, 0.2, 0.1)

# Se cre? una lista vac?a de n?meros enteros donde se almacenar?a la cantidad de 
# reglas creadas por cada valor de soporte y confianza anterior.
rules_sup10 <- integer(length=9)
rules_sup25 <- integer(length=9)

# Se cre? el algoritmo a priori para cada uno de los valores de soporte y nivel de
# confianza y se almacenaron esos valores en las listas vac?as creadas en el paso 
# 2 anterior. Se utiliz? un ciclo for para este proceso.

# Algoritmo Apriori con soporte del 10%
for (i in 1:length(confianzaLev)) {
  
  rules_sup10[i] <- length(apriori(transactionK, 
                                   parameter=list(sup=soporteLev[1],
                                                  conf=confianzaLev[i], 
                                                  target="rules")))
  
}

# Algoritmo Apriori con soporte del 25%
for (i in 1:length(confianzaLev)){
  
  rules_sup25[i] <- length(apriori(transactionK, 
                                   parameter=list(sup=soporteLev[2],
                                                  conf=confianzaLev[i], 
                                                  target="rules")))
  
}
# Se cre? una trama para cada uno de los escenarios. Los gr?ficos mostraron el 
# n?mero de reglas frente al nivel de confianza para cada valor de soporte
library(gridExtra) # provides a number of user-level functions to work with "grid" graphics

# N?mero de reglas encontradas con un soporte del 10%
plot1 <- qplot(confianzaLev, rules_sup10, geom=c("point", "line"), 
               xlab="Nivel de Confianza", ylab="N?mero de reglas encontradas", 
               main="Apriori con soporte del 10%") +
  theme_bw()
# N?mero de reglas encontradas con un soporte del 25%
plot2 <- qplot(confianzaLev, rules_sup25, geom=c("point", "line"), 
               xlab="Nivel de Confianza", ylab="N?mero de reglas encontradas", 
               main="Apriori con soporte del 25%") + 
  theme_bw()

# Subplot
grid.arrange(plot1,plot2 )

# Estos puntos se trazaron en un gr?fico de l?neas m?ltiples. Cada l?nea 
# representaba un valor de soporte.

# Data frame
num_reglas <- data.frame(rules_sup10, rules_sup25 ,confianzaLev)

# N?mero de reglas encontradas con soportes del 0.3%, 0.4%, 0.7%, 1%, 10%, 25%
ggplot(data=num_reglas, aes(x=confianzaLev)) +
  
  # GRaficar l?neas y puntos (soporte del 0.3%)
  geom_line(aes(y=rules_sup10, colour="Soporte del 10%")) + 
  geom_point(aes(y=rules_sup10, colour="Soporte del 10%")) +
  # GRaficar l?neas y puntos (soporte del 25%)
  geom_line(aes(y=rules_sup25, colour="Soporte del 25%")) + 
  geom_point(aes(y=rules_sup25, colour="Soporte del 25%")) +
  # Labs and theme
  labs(x="Niveles de Confianza", y="N?mero de reglas encontradas", 
       title="Algoritmo Apriori con diferentes niveles de Soporte") +
  theme_bw() +
  theme(legend.title=element_blank())

# Podemos apoyarnos en la gr?fica anterior, para determinar los niveles de confianza
# y soporte que permitan analizar una cantidad razonable de reglas. 
# Por lo tanto, para el modelo final se utiliz? un nivel de soporte del 0.7% y un 
# nivel de confianza del 50%.

# Para crear las reglas de asociaci?n se sigue el mismo proceso que para obtener itemsets
# frecuentes pero, adem?s de especificar un soporte m?nimo, se tiene que establecer una
# confianza m?nima para que una regla se incluya en los resultados. En este caso, se
# emplea una confianza m?nima del 50%.

reglas <- apriori(data = transactionK,
                  parameter = list(support = soporteLev[1],
                                   confidence = confianzaLev[1],
                                   # Se especifica que se creen reglas
                                   target = "rules"))

summary(reglas)

r <- as_tibble(as(reglas, Class = "data.frame"))
r

# Inspeccionamos las reglas, ordenando por confianza
inspect(sort(x = reglas, decreasing = TRUE, by = "confidence"))

# Podemos ordenar el tibble
r %>% arrange(desc(confidence))
#Evaluacion
metricas <- interestMeasure(reglas, measure = c("coverage", "fishersExactTest"),
                            transactions = transacciones)

metricas <- interestMeasure(reglas, measure = c("fishersExactTest"),
                            transactions = transacciones)
metricas

#Estas nuevas m?tricas pueden a?adirse al objeto que contiene las reglas.

quality(reglas) <- cbind(quality(reglas), metricas)
# inspect(sort(x = reglas, decreasing = TRUE, by = "confidence"))
df_reglas <- as(reglas, Class = "data.frame") 
df_reglas %>% as.tibble() %>% arrange(desc(confidence)) %>% head()

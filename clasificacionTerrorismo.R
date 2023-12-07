# Minado de datos - Clasificación


###Arbol de clasificacion CART
#install.packages("dplyr")
library(dplyr)
#install.packages("ggplot2")
library(ggplot2)
#install.packages("tree")
library(tree)
#install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)



data = read.csv("/content/afgData.csv", header=T, na.strings=c("","NA"))


#Tabla de dispersión.
#qplot(targtype1, targsubtype1, data = train,colour = iyear, size = I(4))

#Visualizar columnas del dataset.
attach(data)
#Creación del arbol
tre<- tree(iyear ~ targtype1 + targsubtype1, data = data)
#Resumen del arbol.
summary(tre)
#Representación del arbol.
plot(tre)
text(tre)

# Creamos el diagrama de dispersión

graf <- qplot(targtype1,targsubtype1, data = data,colour = iyear, size = I(4))
graf
#Graficas correspondientes a las particiones creadas.
graf + geom_hline(aes(yintercept = 93.0181))


graf +   geom_hline(aes(yintercept = 93.0181))+
  geom_hline(aes(yintercept = 21.8781))

graf +  geom_hline(aes(yintercept = 93.0181))+
  geom_hline(aes(yintercept = 21.8781))+
  geom_vline(aes(xintercept = 36.5)) 


graf + geom_hline(aes(yintercept = 93.0181))+
  geom_hline(aes(yintercept = 21.8781))+
  geom_vline(aes(xintercept = 36.5)) +
  geom_vline(aes(xintercept = 12.5))
  
tre
#Arbol con rpart
data.rpart <- rpart(iyear ~ targtype1+ targsubtype1,data = data)
data.rpart

#Visualización del arbol.
rpart.plot(data.rpart,main = "data")

#Realizamos una prueba de predicción del arbol
supp <-subset(data, select=c("iyear", "targtype1", "targsubtype1") )
supp

new <- data.frame(targsubtype1=c(21), targtype1=c(2))
print("Predicción de targsubtype = 21 y targtype = 2")
predict(tre, newdata=new)

## Red neuronal

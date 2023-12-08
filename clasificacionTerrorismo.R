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

##Realizamos un segundo arbol para ir a la par de la red neuronal
qplot(crit3,doubtterr, data = data,colour = doubtterr, size = I(4))

#Visualizar columnas del dataset.
attach(data)
#Creación del arbol
tre<- tree(crit3 ~  doubtterr, data = data)
#Resumen del arbol.
summary(tre)
#Representación del arbol.
plot(tre)
text(tre)

# Creamos el diagrama de dispersión

graf <- qplot(crit3,doubtterr, data = data,colour = doubtterr, size = I(4))
graf
#Graficas correspondientes a las particiones creadas.
graf + geom_vline(aes(xintercept = 0.5))

tre
#Arbol con rpart
data.rpart <- rpart(crit3 ~ doubtterr,data = data)
data.rpart

#Visualización del arbol.
rpart.plot(data.rpart,main = "data")



##### Red neuronal
data <- read.csv("~/Documents/Mining/afgData.csv")
data$crit3 <- as.factor(data$crit3)
library(caTools)
set.seed(123)

spl <- sample.split(data$X, SplitRatio = .75)
training = subset(data, spl == TRUE)
testing = subset(data, spl == FALSE)

cols <- names(data)
cols <- cols[-which(names(data) %in% c("X","eventid", "iyear", "imonth", "iday", 
                                       "provstate", "city", "alternative_txt",
                                       "attacktype1_txt", "targtype1_txt", 
                                       "targsubtype1_txt", "corp1", "target1", 
                                       "natlty1_txt", "gname", "weaptype1_txt",
                                       "extended", "specificity", "vicinity", 
                                       "alternative", "multiple", 
                                       "natlty1", "guncertain1", "nwoundus",
                                       "nkill", "nwound", "targsubtype1", 
                                       "crit1", "crit2", "crit3", "success", 
                                       "suicide", "attacktype1", "targtype1"))]

form <- paste(cols,collapse=' + ')
form <- paste('crit3 ~',form)
form <- as.formula(form)

library(neuralnet)

sigmoid = function(x) {1/(1 + exp(-x))}
red <- neuralnet(form,training,hidden=c(2,2),linear.output=FALSE)

out <- cbind(red$covariate, + red$net.result[[1]])

library(dplyr)
prediction <- predict(red,testing)

check = as.numeric(testing$crit3) == max.col(prediction)
accuracy = (sum(check)/nrow(testing))*100
print(accuracy)







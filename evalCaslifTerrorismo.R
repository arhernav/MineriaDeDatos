# Minado de datos - Evaluación de modelos de clasificación

### Evaluación del arbol previamente creado
mean(doubterr == tre)

confMat <- table(data$doubterr,tre)
accuracy <- sum(diag(confMat))/sum(confMat)



### Evaluación de la red previamente creada
library(dplyr)
prediction <- predict(red,testing)

check = as.numeric(testing$crit3) == max.col(prediction)
accuracy = (sum(check)/nrow(testing))*100
print(accuracy)

###############################
#### Tecnicas de regresion ####
###############################

# Maquinas de vectores de soporte
rm(list = ls())
library(MASS)
library(dplyr)
library(kernlab)
library(LiblineaR)

data(Boston)
datos = Boston[,c('medv','rm')]

train <- sample(1:nrow(datos), size = nrow(datos)/2)
datos_train = datos[train,]

modelo <- ksvm(medv~rm, 
              data =datos_train, 
              kernel = "vanilladot", 
              C = 10, scale = F)
modelo
datos_train <- arrange(datos_train,medv)

plot(datos_train$medv,datos_train$rm,
     type="l", 
     main="Ejemplo de SVM para regresión", 
     xlab="Tiempo",ylab="DAX")

predicciones <- predict(modelo, newdata = datos[-train,])
test_mse     <- mean((predicciones - Boston[-train, "medv"])^2)
paste("Error de test (mse) del árbol de regresión tras podado:", (round(test_mse,2)))



## Un ejemplo con datos del mercado de valores

#Se cargan los datos
data("EuStockMarkets")

#Se toma solo solo el indicador DAX y se construye 
#una variable para el tiempo, t

tiempo = 1:length(EuStockMarkets[,1])
datos = data.frame(EuStockMarkets[,1],tiempo)
colnames(datos)<-c("DAX","tiempo")

#Se construye el modelo con SVM
modelo_svm = ksvm( DAX~tiempo, 
                   data = datos, scale = F)

#Se hace la gráfica del modelo
plot(datos$tiempo, datos$DAX, type="l", 
     main="Ejemplo de SVM para regresión",
     xlab="Tiempo",ylab="DAX")

lines(datos$tiempo,predict(modelo_svm,datos$tiempo),col="blue")


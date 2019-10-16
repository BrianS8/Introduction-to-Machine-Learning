###############################
#### Tecnicas de regresion ####
###############################

rm(list = ls())
# En esta sección veremos un conjunto de técnicas para arboles de decision, random forest y boosting

# Librerías
library(tree) # Funciones para arboles de regresion y clasificacion
library(rpart) # Funciones para arboles de regresion y clasificacion
library(rpart.plot) # Graficas relacionadas a rpart
library(C50) # Algoritmos para arboles de clasificacion
library(MASS)
library(ggplot2)
library(ggpubr)
library(randomForest)
library(tidyverse)
library(dplyr)

data(Boston)

## Bagging

# Estimar el precio de la vivienda nueva, dado el conjunto de datos disponibles
data("Boston")
ncol(Boston)

set.seed(1)
train <- sample(1:nrow(Boston), size = nrow(Boston)/2)

modelo_bagging <- randomForest(medv ~ ., data = Boston,
                               subset = train, mtry = 13)
modelo_bagging

predicciones <- predict(object = modelo_bagging, newdata = Boston[-train,])
test_mse     <- mean((predicciones - Boston[-train, "medv"])^2)
paste("Error de test (mse) del modelo obtenido por bagging es:",
      (round(test_mse,2)))

# Importancia
modelo_bagging <- randomForest(medv ~ ., data = Boston,
                               mtry = 13, ntree = 500,
                               importance = TRUE)

importancia_pred <- as.data.frame(importance(modelo_bagging,
                                             scale = TRUE))
importancia_pred <- rownames_to_column(importancia_pred,
                                       var = "variable")

p1 <- ggplot(data = importancia_pred, 
             aes(x = reorder(variable, `%IncMSE`),
                 y = `%IncMSE`,fill = `%IncMSE`)) +
  labs(x = "variable", title = "Reducción de MSE") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

p2 <- ggplot(data = importancia_pred, 
             aes(x = reorder(variable, IncNodePurity),
                 y = IncNodePurity,
                 fill = IncNodePurity)) +
  labs(x = "variable", title = "Reducción de pureza") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
ggarrange(p1, p2)


## Random forest


tuning_rf_nodesize <- function(df, y, size = NULL, ntree = 500){
  # Esta función devuelve el out-of-bag-MSE de un modelo randomForest en función del tamaño mínimo de los nodos terminales (nodesize).
  
  # Argumentos:
  #   df = data frame con los predictores y variable respuesta
  #   y  = nombre de la variable respuesta
  #   sizes = tamaños evaluados
  #   ntree = número de árboles creados en el modelo randomForest
  
  if (is.null(size)){
    size <- seq(from = 1, to = nrow(df), by = 5)
  }
  oob_mse <- rep(NA, length(size))
  for (i in seq_along(size)) {
    set.seed(123)
    f <- formula(paste(y,"~ ."))
    modelo_rf <- randomForest(formula = f, data = df, mtry = 5,
                              ntree = ntree, nodesize = i)
    
    oob_mse[i] <- tail(modelo_rf$mse, n = 1)
  }
  results <- data_frame(size, oob_mse)
  return(results)
}


hiperparametro_nodesize <-  tuning_rf_nodesize(df = Boston, y = "medv",
                                               size = c(1:20))

hiperparametro_nodesize %>% arrange(oob_mse)

ggplot(data = hiperparametro_nodesize, aes(x = size, y = oob_mse)) +
  scale_x_continuous(breaks = hiperparametro_nodesize$size) +
  geom_line() +
  geom_point() +
  geom_point(data = hiperparametro_nodesize %>% arrange(oob_mse) %>% head(1),
             color = "red") +
  labs(title = "Evolución del out-of-bag-error vs nodesize",
       x = "nº observaciones en nodos terminales") +
  theme_bw()

# Al identificar el minimo tamano de nodos terminales, tenemos:

modelo_randomforest <- randomForest(medv ~ ., data = Boston, 
                                    subset = train, mtry = 5 , 
                                    ntree = 500, nodesize = 5,
                                    importance = TRUE)

oob_mse <- data.frame(oob_mse = modelo_randomforest$mse,
                      arboles = seq_along(modelo_randomforest$mse))

ggplot(data = oob_mse, aes(x = arboles, y = oob_mse )) +
  geom_line() +
  labs(title = "Evolución del out-of-bag-error vs número árboles",
       x = "nº árboles") +
  theme_bw()

# Ajuste final

predicciones <- predict(object = modelo_randomforest,
                        newdata = Boston[-train, ])

test_mse <- mean((predicciones - Boston[-train, "medv"])^2)

paste("Error de test (mse) del modelo:", round(test_mse, 2))

# Empleando todas las observaciones en el proceso de randomforest
set.seed(123)

modelo_randomforest <- randomForest(medv ~ ., data = Boston, mtry = 5, ntree = 100,nodesize = 3, importance = TRUE)

modelo_randomforest

# Importancia de las variables

importancia_pred <- as.data.frame(importance(modelo_randomforest, scale = TRUE))
importancia_pred <- rownames_to_column(importancia_pred, var = "variable")
p1 <- ggplot(data = importancia_pred, aes(x = reorder(variable, `%IncMSE`),
                                          y = `%IncMSE`,
                                          fill = `%IncMSE`)) +
  labs(x = "variable", title = "Reducción de MSE") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

p2 <- ggplot(data = importancia_pred, aes(x = reorder(variable, IncNodePurity),
                                          y = IncNodePurity,
                                          fill = IncNodePurity)) +
  labs(x = "variable", title = "Reducción de pureza") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")
ggarrange(p1, p2)


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
library(gbm)
library(caret)
data(Boston)

### Boosting

set.seed(1)
train <- sample(1:nrow(Boston), size = nrow(Boston)/2)

cv_error  <- vector("numeric")
n_arboles <- vector("numeric")
shrinkage <- vector("numeric")

# Learning rate (lambda/shrinkage)

for (i in c(0.001, 0.01, 0.1)) {
  set.seed(123)
  arbol_boosting <- gbm(medv ~ ., data = Boston[train, ],
                        distribution = "gaussian",
                        n.trees = 20000,
                        interaction.depth = 1,
                        shrinkage = i,
                        n.minobsinnode = 10,
                        bag.fraction = 0.5,
                        cv.folds = 5)
  cv_error  <- c(cv_error, arbol_boosting$cv.error)
  n_arboles <- c(n_arboles, seq_along(arbol_boosting$cv.error))
  shrinkage <- c(shrinkage, rep(i, length(arbol_boosting$cv.error)))
}
error <- data.frame(cv_error, n_arboles, shrinkage)

ggplot(data = error, aes(x = n_arboles, y = cv_error,
                         color = as.factor(shrinkage))) +
  geom_smooth() +
  labs(title = "Evolución del cv-error", color = "shrinkage") + 
  theme_bw() +
  theme(legend.position = "bottom")

## Complejidad de los arboles (Número de divisiones)

cv_error  <- vector("numeric")
n_arboles <- vector("numeric")
interaction.depth <- vector("numeric")
for (i in c(1, 3, 5, 10)) {
  set.seed(123)
  arbol_boosting <- gbm(medv ~ ., data = Boston[train, ],
                        distribution = "gaussian",
                        n.trees = 5000,
                        interaction.depth = i,
                        shrinkage = 0.01,
                        n.minobsinnode = 10,
                        bag.fraction = 0.5,
                        cv.folds = 5)
  cv_error  <- c(cv_error, arbol_boosting$cv.error)
  n_arboles <- c(n_arboles, seq_along(arbol_boosting$cv.error))
  interaction.depth <- c(interaction.depth,
                         rep(i, length(arbol_boosting$cv.error)))
}
error <- data.frame(cv_error, n_arboles, interaction.depth)

ggplot(data = error, aes(x = n_arboles, y = cv_error,
                         color = as.factor(interaction.depth))) +
  geom_smooth() +
  labs(title = "Evolución del cv-error", color = "interaction.depth") + 
  theme_bw() +
  theme(legend.position = "bottom")

## Minimo numero de observaciones por nodo

cv_error  <- vector("numeric")
n_arboles <- vector("numeric")
n.minobsinnode <- vector("numeric")
for (i in c(1, 5, 10, 20)) {
  arbol_boosting <- gbm(medv ~ ., data = Boston[train, ],
                        distribution = "gaussian",
                        n.trees = 5000,
                        interaction.depth = 5,
                        shrinkage = 0.01,
                        n.minobsinnode = i,
                        bag.fraction = 0.5,
                        cv.folds = 5)
  cv_error  <- c(cv_error, arbol_boosting$cv.error)
  n_arboles <- c(n_arboles, seq_along(arbol_boosting$cv.error))
  n.minobsinnode <- c(n.minobsinnode,
                      rep(i, length(arbol_boosting$cv.error)))
}
error <- data.frame(cv_error, n_arboles, n.minobsinnode)

ggplot(data = error, aes(x = n_arboles, y = cv_error,
                         color = as.factor(n.minobsinnode))) +
  geom_smooth() +
  labs(title = "Evolución del cv-error", color = "n.minobsinnode") + 
  theme_bw() +
  theme(legend.position = "bottom")

# Numero de arboles

set.seed(123)
arbol_boosting <- gbm(medv ~ ., data = Boston[train, ],
                      distribution = "gaussian",
                      n.trees = 10000,
                      interaction.depth = 5,
                      shrinkage = 0.01,
                      n.minobsinnode = 1,
                      bag.fraction = 0.5,
                      cv.folds = 5)
error <- data.frame(cv_error = arbol_boosting$cv.error,
                    n_arboles = seq_along(arbol_boosting$cv.error))
ggplot(data = error, aes(x = n_arboles, y = cv_error)) +
  geom_line(color = "black") +
  geom_point(data = error[which.min(error$cv_error),], color = "red") +
  labs(title = "Evolución del cv-error") + 
  theme_bw() 

error[which.min(error$cv_error),]

# Se reajusta el modelo final con los hiperparámetro óptimos
set.seed(123)
arbol_boosting <- gbm(medv ~ ., data = Boston[train, ],
                      distribution = "gaussian",
                      n.trees = 2791,
                      interaction.depth = 5,
                      shrinkage = 0.01,
                      n.minobsinnode = 1,
                      bag.fraction = 0.5)


# Hiperparametros optimos con caret

set.seed(123)
validacion <- trainControl(## 10-fold CV
  method = "cv",
  number = 10)

tuning_grid <-  expand.grid(interaction.depth = c(1, 5, 9), 
                            n.trees = c(100, 1000, 2000, 3000), 
                            shrinkage = c(0.1, 0.01, 0.001),
                            n.minobsinnode = c(1, 10, 20))

set.seed(123)
mejor_modelo <- train(medv ~ ., data = Boston[train, ], 
                      method = "gbm", 
                      trControl = validacion, 
                      verbose = FALSE, 
                      tuneGrid = tuning_grid)

# Se muestran los hiperparámetros del mejor modelo 
mejor_modelo$bestTune


rbol_boosting <- gbm(medv ~ ., data = Boston[train, ],
                     distribution = "gaussian",
                     n.trees = 3000,
                     interaction.depth = 5,
                     shrinkage = 0.01,
                     n.minobsinnode = 1,
                     bag.fraction = 0.5)

## Importancia de cada variable

importancia_pred <- summary(arbol_boosting, plotit = FALSE)

ggplot(data = importancia_pred,
       aes(x = reorder(var, rel.inf), y = rel.inf,fill = rel.inf)) +
  labs(x = "variable", title = "Reducción de MSE") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

par(mfrow = c(1,2))

## Influencia de algunos preductores sobre la variable y
plot(arbol_boosting, i.var = "rm", col = "blue")
plot(arbol_boosting, i.var = "lstat", col = "firebrick")


## Predicciones
predicciones <- predict(object = arbol_boosting, 
                        newdata = Boston[-train,],
                        n.trees = 1447)

test_mse <- mean((predicciones - Boston[-train, "medv"])^2)
paste("Error de test (mse) del modelo:", round(test_mse, 2))




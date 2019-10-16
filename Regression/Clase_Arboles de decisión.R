###############################
#### Tecnicas de regresion ####
###############################

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

### Ejemplo arbol de decisión
data("Boston")
head(Boston)
train <- sample(1:nrow(Boston), size = nrow(Boston)/2)
arbol_regresion <- tree(formula = medv ~ ., data = Boston, subset = train,
                        split = "deviance")
plot(x = arbol_regresion, type = "proportional")
text(x = arbol_regresion, splits = TRUE, pretty = 0,
     cex = 0.8, col = "firebrick")

## Por defecto tenemos:
# mincut = 5: número mínimo de observaciones que debe de tener al menos uno de los nodos hijos para que se produzca la división.
# minsize = 10: número mínimo de observaciones que debe de tener un nodo para que pueda dividirse.
# deepth = 31: profundidad máxima que puede alcanzar el árbol.


# Podado

set.seed(3)
cv_arbol <- cv.tree(arbol_regresion, K = 10)
cv_arbol



resultados_cv <- data.frame(n_nodos = cv_arbol$size,
                            deviance = cv_arbol$dev,
                            alpha = cv_arbol$k)

p1 <- ggplot(data = resultados_cv, aes(x = n_nodos, y = deviance)) +
  geom_line() + 
  geom_point() +
  labs(title = "Error vs tamaño del árbol") + theme_bw() 


p2 <- ggplot(data = resultados_cv, aes(x = alpha, y = deviance)) +
  geom_line() + 
  geom_point() +
  labs(title = "Error vs hiperparámetro alpha") + theme_bw() 

ggarrange(p1, p2)


arbol_pruning <- prune.tree(tree = arbol_regresion, best = 8)
plot(x = arbol_pruning, type = "proportional")
text(x = arbol_pruning, splits = TRUE, pretty = 0,
     cex = 0.8, col = "firebrick")

predicciones <- predict(arbol_pruning, newdata = Boston[-train,])
test_mse     <- mean((predicciones - Boston[-train, "medv"])^2)
paste("Error de test (mse) del árbol de regresión tras podado:", (round(test_mse,2)))




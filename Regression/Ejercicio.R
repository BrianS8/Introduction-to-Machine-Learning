rm(list = ls())
library(dplyr)
library(psych)
library(GGally)
library(ggplot2)
library(gridExtra)
library(lmtest)
library(corrplot)
library(car)
library(ISLR)
require(knitr)
library(AER)
library(stargazer)
library(scales)
library(MASS)
library(mvtnorm)
library(rsample)
library(e1071)


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



options(scipen=999)

data(state)
datos <-  force(state.x77)

colnames(datos) <- c("habitantes","ingresos","analfabetismo", "esp_vida","asesinatos","universitarios", "heladas", "area")
datos <- data.frame(datos)
datos$densidad_pobl <- datos$habitantes * 1000 / datos$area

# Datos de entrenamiento y de validación

data   <- initial_split(datos, prop = 0.7)
dtrain <- training(data)
dtest  <- testing(data)

# Analisis grafico de las variables
# a <- round(cor(x = dtrain, method = "pearson"), 3)
# 
# multi.hist(x = dtrain, dcol = c("blue", "red"), dlty = c("dotted", "solid"),main = "")

ggpairs(dtrain, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")

# pairs(dtrain)

modelo <- lm(esp_vida ~ habitantes + ingresos + analfabetismo + asesinatos + universitarios + heladas + area + densidad_pobl, data = dtrain )

summary(modelo)

# Selección del mejor modelo dado el criterio AIC
step(object = modelo, direction = "both", trace = 1)

modelo <- lm(formula = esp_vida ~ asesinatos + universitarios + heladas, data = dtrain)
summary(modelo)

plot(modelo)

### Pruebas de hipotesis
# Normalidad
shapiro.test(modelo$residuals)

#Homocedasticidad
bptest(modelo)

# No multicolinealidad
corrplot(cor(dplyr::select(datos, habitantes, asesinatos,universitarios,heladas)), method = "number", tl.col = "black")

# Analisis de inflacion de la varianza

vif(modelo)

# Autocorrelación
dwt(modelo, alternative = "two.sided")

# Grafico de influencias
influencePlot(modelo)


predicciones <- predict(object = modelo, newdata = dtest)
test_mse     <- mean((predicciones - dtest[,"esp_vida" ])^2)
paste("Error de test (mse) del modelo obtenido por OLS es:",
      (round(test_mse,2)))



#### SVM

modelo_svm = svm(formula = esp_vida ~ habitantes + asesinatos +
                  universitarios + heladas,
                data = dtrain,
                type = 'eps-regression')

# Predicting a new result
predicciones <- predict(object = modelo_svm, newdata = dtest)
test_mse     <- mean((predicciones - dtest[,"esp_vida" ])^2)
paste("Error de test (mse) del modelo obtenido por SVM es:",
      (round(test_mse,2)))



## Arbol de decision

arbol_regresion <- tree(formula = esp_vida ~ habitantes + asesinatos +
                          universitarios + heladas, data = dtrain, 
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


arbol_pruning <- prune.tree(tree = arbol_regresion, best = 5)
# plot(x = arbol_pruning, type = "proportional")
# text(x = arbol_pruning, splits = TRUE, pretty = 0,
#      cex = 0.8, col = "firebrick")

predicciones <- predict(arbol_pruning, newdata = dtrain)
test_mse     <- mean((predicciones - dtest[,"esp_vida" ])^2)
paste("Error de test (mse) del árbol de regresión tras podado:", (round(test_mse,2)))


## Random Forest

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

hiperparametro_nodesize <-  tuning_rf_nodesize(df = dtrain, y = "esp_vida",
                                               size = c(1:20))

hiperparametro_nodesize %>% arrange(oob_mse)

ggplot(data = hiperparametro_nodesize, aes(x = size, y = oob_mse)) +
  scale_x_continuous(breaks = hiperparametro_nodesize$size) +
  geom_line() +
  geom_point() +
  geom_point(data = hiperparametro_nodesize %>% arrange(oob_mse) %>% head(1),) +
  labs(title = "Evolución del out-of-bag-error vs nodesize",
       x = "nº observaciones en nodos terminales") +
  theme_bw()

# Al identificar el minimo tamano de nodos terminales, tenemos:

modelo_randomforest <- randomForest(esp_vida ~ habitantes + asesinatos +
                                      universitarios + heladas, data = dtrain, mtry = 5, 
  ntree = 500, nodesize = 3,
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


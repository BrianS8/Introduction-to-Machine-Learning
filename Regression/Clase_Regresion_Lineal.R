## Clase 5 alterna

## Modelo de regresión lineal multiple
# Objetivo: Predecir la esperanza de vida de una población
# Variables: Habitantes, analfabetismo, ingresos, asesinatos,
# universitarios, heladas y densidad poblacional

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

options(scipen=999)

data(state)
datos <-  force(state.x77)

colnames(datos) <- c("habitantes","ingresos","analfabetismo", "esp_vida","asesinatos","universitarios", "heladas", "area")
datos <- data.frame(datos)
datos$densidad_pobl <- datos$habitantes * 1000 / datos$area


# Ejercicio 1
y <- as.matrix(datos[,c(4)])
x <- as.matrix(datos[,])

betas = solve(t(x)%*%x)
betas = betas%*%t(x)
betas = betas%*%y

# Y estimado
y_estimado = x%*%betas
error = y - y_estimado
plot(error)
hist(error)
mean(error)

# Analisis grafico de las variables
a <- round(cor(x = datos, method = "pearson"), 3)

multi.hist(x = datos, dcol = c("blue", "red"), dlty = c("dotted", "solid"),main = "")
ggpairs(datos, lower = list(continuous = "smooth"),
        diag = list(continuous = "bar"), axisLabels = "none")

pairs(datos)

# Generemos el modelo con todas las variables

modelo <- lm(esp_vida ~ habitantes + ingresos + analfabetismo + asesinatos + universitarios + heladas + area + densidad_pobl, data = datos )
summary(modelo)

# Selección del mejor modelo dado el criterio AIC
step(object = modelo, direction = "both", trace = 1)

modelo <- (lm(formula = esp_vida ~ habitantes + asesinatos + universitarios + heladas, data = datos))
summary(modelo)


confint(lm(formula = esp_vida ~ habitantes + asesinatos + universitarios + heladas, data = datos))

# Comprobacion de las hipotesis


plot1 <- ggplot(data = datos, aes(habitantes, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot2 <- ggplot(data = datos, aes(asesinatos, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot3 <- ggplot(data = datos, aes(universitarios, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
plot4 <- ggplot(data = datos, aes(heladas, modelo$residuals)) +
  geom_point() + geom_smooth(color = "firebrick") + geom_hline(yintercept = 0) +
  theme_bw()
grid.arrange(plot1, plot2, plot3, plot4)

plot(modelo)
# Normalidad
qqnorm(modelo$residuals)
qqline(modelo$residuals)

shapiro.test(modelo$residuals)

#Homocedasticidad
ggplot(data = datos, aes(modelo$fitted.values, modelo$residuals)) +
  geom_point() +
  geom_smooth(color = "firebrick", se = FALSE) +
  geom_hline(yintercept = 0) +
  theme_bw()

# Breusch-Pagan test
bptest(modelo)

# No multicolinealidad

corrplot(cor(dplyr::select(datos, habitantes, asesinatos,universitarios,heladas)),
         method = "number", tl.col = "black")

# Analisis de inflacion de la varianza

vif(modelo)

# Autocorrelación
dwt(modelo, alternative = "two.sided")

# Grafico de influencias

influencePlot(modelo)

##################
## Validaciones ##
##################

data(Auto)
kable(head(Auto, n = 3), align = "c")

set.seed(1)
train <- sample(x = 1:392, 196)

modelo <- lm(mpg~horsepower, data = Auto, subset = train)
summary(modelo)


predicciones <- predict(object = modelo, newdata = Auto[-train, ])

mean((Auto$mpg[-train] - predicciones)^2)


# Graficas iterativas
cv_MSE <- rep(NA,100)
for (i in 1:100) {
  train <- sample(x = 1:392, 196)
  modelo <- lm(mpg ~ horsepower, data = Auto, subset = train)
  predicciones <- predict(object = modelo,newdata = Auto[-train, ])
  cv_MSE[i] <- mean((Auto$mpg[-train] - predicciones)^2)
}
p1 <- ggplot(data = data.frame(cv_MSE = cv_MSE), aes(x = 1, y = cv_MSE)) +
  geom_boxplot(outlier.shape = NA) +
  geom_jitter(colour = c("firebrick3"), width = 0.1) +
  theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank())

p2 <- ggplot(data = data.frame(cv_MSE = cv_MSE), aes(cv_MSE)) +
  geom_histogram(colour = "firebrick3") +
  theme_bw()
grid.arrange(p1, p2, ncol = 2)



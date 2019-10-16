## Regresion polinomica

library(MASS)
library(ISLR)

attach(Boston)

# Regresion lineal simple
modelo_simple <- lm(data = Boston,formula = medv ~ lstat)

par(mfrow = c(1,1))
plot(x = lstat, y = medv, main = "medv vs lstat", pch = 20, col = "grey30")
abline(modelo_simple, lwd = 3, col = "red")

par(mfrow = c(2,2))
plot(modelo_simple)
par(mfrow = c(1,1))

modelo_pol2 <- lm(formula = medv ~ poly(lstat, 2), data = Boston)
summary(modelo_pol2)
par(mfrow = c(2,2))
plot(modelo_pol2)
par(mfrow = c(1,1))

# Los datos no se distribuyen de forma normal, ni su varianza es constante
# La distribucion de los resuduos no es normal
# Hay presencia de valores atipicos
# Presencia de valores influyentes

attach(Boston)
plot(x = lstat, y = medv, main = "medv vs lstat", pch = 20, col = "grey30")
points(lstat, fitted(modelo_pol2), col = 'red', pch = 20)

# Test ANOVA
anova(modelo_simple, modelo_pol2)[6]

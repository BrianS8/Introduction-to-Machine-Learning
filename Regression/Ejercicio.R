
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



## Ejercicio 2

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


# Importing the dataset
dataset = read.csv('50_Startups.csv')
# Encoding categorical data
dataset$State = factor(dataset$State,
                       levels = c('New York', 'California', 'Florida'),
                       labels = c(1, 2, 3))
dataset$State <- as.numeric(as.character(dataset$State))


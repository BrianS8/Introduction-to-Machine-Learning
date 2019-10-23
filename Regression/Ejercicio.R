rm(list=ls())
library(AmesHousing)
library(rsample)

AmesHousing = make_ames()
set.seed(123)
ames_split <- initial_split(AmesHousing::make_ames(), prop = .7)
ames_train <- training(ames_split)
ames_test  <- testing(ames_split)









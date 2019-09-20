### Clase 4 - Manejo de bases de datos

# Librerías
library(xlsx)
library(dplyr)
library(reshape)
library(stringr)
setwd("/Users/bsmith/Documents/Introduction Machine Learning/Course/Introduction-to-Machine-Learning/Data Pre processing")


df <- read.csv('movie.csv', sep = ";")
head(df)

# Eliminar duplicados y valores perdidos
df <- distinct(df)
df <- na.omit(df)
summary(df)

# Agrupar por

gdf <- group_by(df, release_year)
summarise(gdf,num = n())
gdf <- aggregate(df[,'ratingdescription'],by = list(df$release_year),FUN = mean)

# Split

ndf<-str_split_fixed(df$group, ":", 2)
df <- cbind(df,ndf)
df[,-which(names(df) %in% c('ratingdescription','release_year'))]

# Reshape
df <- read.csv('jugadores_nba.csv', sep = ";")
df_melt = melt(df,id_vars =c('Name', 'Team'))
head(df_melt)

# Subset

gdf = subset(df, Team == 'Boston Celtics' )
gdf
gdf = subset(df, Team == 'Boston Celtics' & Position == 'PG')

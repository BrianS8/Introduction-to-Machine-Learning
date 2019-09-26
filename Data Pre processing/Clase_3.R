### Clase 3 - Manejo de bases de datos
# Librerías
library(xlsx)
library(dplyr)
setwd("/Users/bsmith/Documents/Introduction Machine Learning/Course/Introduction-to-Machine-Learning/Data Pre processing")

# Importar 
df = read.xlsx('mpg.xlsx', sheetName='HojaDatos')
data = read.csv('mpg.csv', sep = ";", stringsAsFactors = F)
head(df)
sapply(df,class)


# Convertir variables
df$horsepower <- as.numeric(as.character(df$horsepower))
df$horsepower <- as.factor(as.character(df$horsepower))

## Limpieza de Bases de datos
# Missing values
df = read.xlsx('mpg_clean.xlsx', sheetIndex = 1, stringsAsFactors = F)
is.na.data.frame(df)
colSums(is.na.data.frame(df))

# Identificar los missing values
df[df=="na"] <- NA
df[df=="--"] <- NA
df[df=="n/a"] <- NA
df[df=="NaN"] <- NA
colSums(is.na.data.frame(df))

# Tratamiento: En R la clase de las columnas cambia, por ello hay que modificarla para que realicemos las interpolaciones
sapply(df,class)
df[c(1,3,4,5,6,7,8)]<- lapply(df[c(1,3,4,5,6,7,8)], as.numeric)
sapply(df,class)


getmode <- function(v) {
  uniqv <- unique(v)
  uniqv[which.max(tabulate(match(v, uniqv)))]
}

null1 = mean(df[,c('displacement')], na.rm = TRUE)
null2 = median(df[,c('horsepower')], na.rm = TRUE)
null3 = median(df[,c('weight')], na.rm = TRUE)
null4 = mean(df[,c('model.year')], na.rm = TRUE)
null5 = median(df[,c('acceleration')], na.rm = TRUE)
null6 = getmode(df[,c('origin')])


df$displacement[is.na(df$displacement)] <- null1
df$horsepower[is.na(df$horsepower)] <- null2
df$weight[is.na(df$weight)] <- null3
df$model.year[is.na(df$model.year)] <- null4
df$acceleration[is.na(df$acceleration)] <- null5
df$origin[is.na(df$origin)] <- null6

colSums(is.na.data.frame(df))

df$cylinders <- as.numeric(as.character(gsub("[[:alpha:]]","",df$cylinders)))
df$car.name<- as.character(gsub("\"","",df$car.name))

df <- df[-c(19,25),]

## Consultas
summary(df)
unique(df$car.name)


# Ordenar
df <- arrange(df,car.name)
# Eliminar duplicados
df <- distinct(df)
df2 <- distinct(df,mpg,.keep_all= TRUE)
sapply(df, function(x) length(unique(x)))


## Combinar y unir

# Combinar datasets

df = read.xlsx('mpg.xlsx',sheetName ='HojaDatos', stringsAsFactors = F)

# Separar en subconjuntos de índices
df1 = df[0:199,]
df2 = df[200:298,]
# Separar en columnas
df3 = df[0:10,c(1,2,3,4,9)]
df4 = df[0:13,c(5,6,7,8,9)]

df5 = rbind(df1,df2)

# FULL OUTER JOIN
union1 = merge(x = df3, y = df4, by = "car.name", all = TRUE)
# LEFT JOIN
union2 = merge(x = df3, y = df4, by = "car.name", all.x = TRUE)
# RIGHT JOIN
union3 = merge(x = df3, y = df4, by = "car.name", all.y = TRUE)
# INNER JOIN
union4 = merge(x = df3, y = df4, by = "car.name")
# CROSS
union5 = merge(x = df3, y = df4, by = NULL)

#write.csv()
#write.table()
#write.xlsx()

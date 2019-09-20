## Clase 1
# Data Frame
data = data.frame(Nombre = c('Andres', 'Camilo', 'Laura', 'Daniela'), Edad = c(20, 21, 19, 18))
sapply(data, class)

head(data)

colnames(data)
row.names(data)

## Otro data frame
df = data.frame(
  Nombre= c('Maria', 'Carlos', 'Juan', 'Lorena'),
  Edad = c(27, 24, 22, 32),
  Ciudad = c('Bogota', 'Cali', 'Bucaramanga', NA),
  Sexo = c('F', 'M', 'M', 'F'))

## Transformemos el campo sexo a dato categórico
df$Sexo = as.character(df$Sexo)
sapply(data, class)

# Manipular nombres de filas y columnas
colnames(df) <- c('Name','Age','City','Sex')
row.names(df) <- seq(2,5)
colnames(df)
row.names(df)
row.names(df) <- c('Maria', 'Carlos', 'Juan', 'Lorena')
row.names(df)
df

# Indexar columnas y filas
df$Age
df[,c('Age')]
df[1,]
df[c('Juan'),]

## Agregar columnas y filas
df = rbind(df,df[2,])
df
df[,c('Hijos')] =c(1,2,4,1,0)
df




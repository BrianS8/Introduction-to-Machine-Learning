### Manipulación y procesamiento de datos

# Eliminación de columnas y filas

df = data.frame(
  Nombre= c('Maria', 'Carlos', 'Juan', 'Lorena'),
  Edad = c(27, 24, 22, 32),
  Ciudad = c('Bogota', 'Cali', 'Bucaramanga', NA),
  Sexo = c('F', 'M', 'M', 'F'))

df2 = df

# Eliminemos la columna por su etiqueta                 
df2[,-which(names(df) %in% c("Ciudad"))]
# Eliminemos la columna por su posición
df2[,-3]
df2[-which(row.names(df) %in% c("2")),]
df2[-2,]

# Indexar elementos de un dataframe

df[3,4]
df["3",'Sexo']
df[3,'Sexo']
df["3",4]

## Reemplazar valores de una columna

df$Edad <- seq(20,32,dim(df)[1])
# Miremos el siguiente caso

df['Calificación'] = c('+3.5A','-2.4v','*1.5d','%5.0B')

# Eliminemos aquellos caracteres que hacen ruido
df$Calificación <- as.character(gsub('\\+',"",df$Calificación))
df$Calificación <- as.character(gsub('\\*',"",df$Calificación))
df$Calificación <- as.character(gsub('%',"",df$Calificación))
df$Calificación <- as.character(gsub('-',"",df$Calificación))

df$Calificación <- as.character(gsub('A',"",df$Calificación))
df$Calificación <- as.character(gsub('v',"",df$Calificación))
df$Calificación <- as.character(gsub('d',"",df$Calificación))
df$Calificación <- as.character(gsub('B',"",df$Calificación))

df['Calificación'] = c('+3.5A','-2.4v','*1.5d','%5.0B')
df$Calificación <- as.character(gsub('[[:alpha:]]',"",df$Calificación))
df


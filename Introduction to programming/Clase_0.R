##### Introducción a R
#En está sesión realizaremos una introducción a R Este lenguaje se caracteriza por estar orientado a la estadística, lo cual permite desarrollar distinto tipos de tareas, ser de alto nivel por el número de paquetes, es dinámico y sencillo de aprender.
  
  
## Los paquetes y los modulos

# Al igual que Python, este lenguaje se caracteriza principalmente porque se soporta en librerías, las cuales complementan sus funciones, las librerías que por lo general trabajaremos en el curso son:
# * dpylr

install.packages("dplyr")
library(dplyr)

## Tipos de elementos en R
# Numeros
class(1)
class(1.0)
# String
class('Hello world!')
# Factor - Categoricos
class(factor(1))
# Booleanos
TRUE
FALSE

### Ejercicio 1 

## En la siguiente linea, tipeen los siguientes elementos defina de que tipo son:
# "Hello world!!"
# 1908
# 1087.188893
# "Hello world" + 1 + "My name is:"
## Transforme números a string, y luego de manera inversa

## Vectores
# A diferencia de Python, en un elemento plano no podemos tener distintos atributos de distintas clases
c(1,2,3,4,'letra')
 # En este caso a es un vector string, que si lo transformamos en 1, tendríamos:
as.numeric(c(1,2,3,4,'letra'))
## Listas
# La estructura de las listas en R es muy diferente a Python, pues en este caso las podemos entender como un conjunto de cajas que almacenan distintos tipos de elementos

b = list(c(1,2,3,4,'letra'),c(1,2,3,4))


### Operaciones algebraicas

# En R tambien es posible realizar las siguientes operaciones
1+1
2-1
2*4
2**3
8/2

### Variables
#Las variables son objetos que contienen distintos tipos de datos, tales como números, strings, listas, vectores y matrices. Existen reglas básicas en las convenciones de definir los nombres de las variable, por ejemplo:
# * En R las mayúsculas y minúsculas se entienden como 2 elementos distintos
# * Los nombres no pueden comenzar por números o caracteres no alfabéticos, estos últimos son utilizados para aplicar ciertas operaciones

a = 1
A = "Hello world!"
b = c(1,2,3,4,'ee')
B = c(1,2,3,4)

### Vectores y matrices
## Vectores:
# En R podemos generar vectores numericos con la siguiente manera
vector1 = c(2,4,6,8,10)
vector2 = 2:2:10
vector3 = array(B)
vector4 = rep(1,5)

print(vector1)
print(vector2)
print(vector3)
print(vector4)
# Verifique que son vectores

## Matrices
# En R podemos generar matrices de la siguiente manera
 matriz1 = matrix(vector3, nrow = 2, ncol = 2)

 ### Indexacion
 # En varias ocasiones, es necesario manipular los elementos de las listas, los vectores, las matrices y demás objetos, para rebanar, adicionar y cambiar. En el caso de R, un objeto que posee 10 elementos, su indexación va de 1 a 10, es decir la posición del primer elemento es 1, y esto porque este lenguaje utiliza la indexación en base-1. 
 
 # Podemos tomar un vector y hacer distintos tipos de indexación:
listaN = seq(1,20)

# ¿Cual es el quinto elemento de la lista?
listaN[5]

# Tomemos los 10 primeros elementos

listaN[1:10]

# Tomemos el ultimo elemento

listaN[length(listaN)]

# Tomemos los numeros impares

listaN[seq(1,19,2)]

# Reemplacemos un elemento
listaN[2] = 500

listaN[2]

# Seleccionemos los valores mayores a 10
listaN[listaN > 10]

# Adicionemos un elemento
listaN[21] = 21

# Eliminemos el segundo elemento
listaN = listaN[-2]


### Matrices y vectores

matriz2 = matrix(c(34,5,677,4,6,3,33,3,55), nrow = 3, ncol = 3)
vector2 = c(2,7,8,5,0,2)
vector3 = c(4,76,87,45,44,33)


# Suma
print(vector2 + vector3)


# Resta
print(vector2 - vector3)

# Producto escalar
print(vector2 %*% vector3)


# Calcular la norma de un vector
print(sqrt(vector2 %*% vector2))

# Vectores ortogonales
v1 = c(3, 4)
v2 = c(4, -3)
print(v1 %*% v2)

# Creando una matriz identidad de 2x2
I = diag(3)
print(I)
# Multiplicar una matriz por la identidad nos da la misma matriz
B = matriz2 %*% I
print(B)

# Calculando el determinante de la matriz A
print(det(matriz2))

# Calculando la inversa de A.
matriz2_inv = solve(matriz2)

# A x A_inv nos da como resultado I.
print(matriz2 %*% matriz2_inv)

# Trasponiendo una matriz
matriz4 = t(matriz2)
print(matriz4)


### Funciones
#Las funciones en R son objetos que siguen una serie de sentencias para obtener un valor/resultado deseado. En la mayoría de los casos requieren unos elementos de entrada y que este objeto sea invocado

#Se puede utilizar en:
# * Reutilización: Es utilizada en diferentes ejercicios
# * Modulación: Es utilizada para fragmentar un código extenso y complejo
# En las siguientes lineas desarrollaremos unos ejemplos de funciones:

hola = function(nombre){
  # Este es un saludo
  print(paste('Hola',nombre,'Cómo estás hoy?'))
}
hola(nombre = "Ernesto")


# Otra funcion

hola("Ernesto")


newMatrix = function(f,c,n){
  matriz = matrix(n, nrow=f, ncol = c)
  return(matriz)
}

newMatrix(3, 5, 1)

### Sentencias condicionales

# Existen diversos tipos de condicionales que permiten evaluar y comparar unos resultados para depurar un código

# Entre ellas tenemos:
# * if
# * elif
# * else

# Ejemplos de este tipo de sentencias son los siguientes:

x = 40
if(x < 30){
  print("El valor de x es menor a 30")
}

if(x < 30){
  print("El valor de x es menor a 30")
} else{
  print("El valor de x es mayor o igual a 30")
}
  

if (x > 10 & x < 20){
  print("X se encuentra entre 10 y 20")
}else if(x == 40){
  print("X es igual a 40")
}
  

### Iteración

#Es habitual efectuar actualizaciones a los valores de las variables, para ello existen diversos tipos de sentencias de asignación que utilizan las dimensiones de un objetos y/o sentencias condicionales aplicados a los mismos para obtener un resultado.

### Bucles _for_ y _while_

#Los bucles _for_ y _while_ son sentencias utilizadas para recorrer los elementos que posee un objeto y se contruyen de la siguiente manera:
# * Se inicia con un objeto, a veces vacío, antes de ejecutar la sentencia
# * Se efectuan algún tipo de funciones, combinaciones, evaluaciones dentro del cuerpo del bucle
# * Se debe revisar las variables resultantes cuando el bucle se completa
 

print("Listos para el despegue!!")

n = 10
while (n > 0){
  print(n)
  n = n-1
}
print('¡Despegue!')

n = 10

# Bucle infinito - Ojo mire como detener una ejecucion en R
while(TRUE){
  print(n)
  n = n-1
}
print('¡Terminado!')

# For
contador = 0
for (valor in listaN){
  contador = contador + 1
}
print(paste('Num. elementos: ', contador))

mayor = 0
print('Antes:', mayor)

for (valor in c(3, 41, 12, 9, 74, 15)){
  if(mayor == 0 | valor > mayor){
    mayor = valor
  }
  print(paste('Bucle:',valor,mayor))
}
print(paste('Mayor:', mayor))

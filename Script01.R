##-------------------------------------------------##
##-----------      Modelos Linelaes     -----------##
##-----------        Trabajo 01         -----------##
##-- Nombre: Cristian David Pachacama


# 2.1 Leer el archivo de datos data.txt, y analizar de que estructura de datos se trata.
# Utilice la función read.table()

dir<-"C:/Users/TOSHIBA/Desktop"
setwd(dir)
list.files()

data<-read.table("data.txt",header=TRUE,dec=",",sep="\t")
str(data)

# 2.2 Calcular el mínimo, la media, el máximo de la variable Edad.
# Utilice las funciones min(), mean(), max(), de ser necesario utilice 
# el parámetro na.rm = TRUE


mean(data[,"Edad"],na.rm=TRUE)
min(data[,"Edad"],na.rm=TRUE)
max(data[,"Edad"],na.rm=TRUE)

# 2.3 Para la variable Genero, contar cuantos sujetos son de Genero: Femenino.
# Utilice la función table()

dat_femenino<-subset(data,subset=data[,"Genero"]=="Femenino")
table(dat_femenino[,"Genero"])

# 2.4 Encontrar la Edad mínima, media, máxima de los sujetos que Si son dependientes.

data_depend<-subset(data,subset=data[,"Dependiente"]=="Si")
mean(data_depend[,"Edad"],na.rm=TRUE)
min(data_depend[,"Edad"],na.rm=TRUE)
max(data_depend[,"Edad"],na.rm=TRUE)

# 2.5 Identificar el tipo de elementos que contiene cada variable.
# Utilice la función typeof()
tipos<-numeric(ncol(data))
for(i in 1:ncol(data)){
  tipos[i]<-typeof(data[,i])
}
tipos

# 2.6 Identificar la clase de cada variable (columna).
# Utilice la función class()
clase<-numeric(ncol(data))
for(i in 1:ncol(data)){
  clase[i]<-class(data[,i])
}
clase
# 2.7 Calcular la media de todas las variables numéricas (double, integer).
# Recordar que para un factor no es posible obtener la media debido a que 
# éstos representan variables
medias<-numeric(ncol(data))
for(i in 1:ncol(data)){
  if(is.numeric(data[,i])==TRUE){
    medias[i]<-mean(data[,i],na.rm=TRUE)
  }
}
medias

# 2.8 Calcular el porcentaje de valores perdidos que contiene cada variable.
# Utilice la función is.na()

vacios<-numeric(ncol(data))
porcentaje<-numeric(ncol(data))
for(i in 1:ncol(data)){
  vacios[i]<-sum(is.na(data[,i]))
  porcentaje[i]<-vacios[i]/nrow(data)*100
}
porcentaje

# 3. Selecionando sujetos mediante un determinado criterio:
# 3.1 Seleccione los sujetos con una Edad mayor a 40 años.
# Utilice la función subset()

criterio_edad<-subset(data,subset=data[,"Edad"]>40)
nrow(criterio_edad)
#View(criterio_edad)

# 3.2 Seleccione los sujetos que tienen Vivienda Propia.

criterio_vivienda<-subset(data,subset=data[,"Vivienda"]=="Propia")
nrow(criterio_vivienda)
#View(criterio_vivienda)

# 3.3 Seleccione los sujetos que tienen más ($>$) de dos cargas familiatres.

criterio_cargas<-subset(data,subset=data[,"Cargas"]>2)
nrow(criterio_cargas)
#View(criterio_cargas)

# 3.4 Seleccione los sujetos con una Deuda superior o igual a 500 dólares
# y más ($>$) de 8 Dias_Atraso.

criterio_deuda<-subset(data,subset=data[,"Deuda"]>=500)
nrow(criterio_deuda)
#View(criterio_deuda)

# 3.5 Seleccione los sujetos con un Score mayor o igual a 900 puntos, una Edad menor
# o igual a 35 años y con más ($>$) de 3 tarjetas de crédito (Numero_TC).

criterio_varios<-subset(data,subset=data[,"Score"]>=900 & data[,"Edad"]<=35 & data[,"Numero_TC"]>3)
nrow(criterio_varios)
#View(criterio_varios)

# 4. Gráficos:
# 4.1 Realice un histograma de la variable Edad, utilice como color de relleno: red

hist(data[,"Edad"],col="red")

# 4.2 Realice un diagrama de cajas de la variable Edad, utilice como color de relleno: green
# Utilice la función boxplot()

boxplot(data[,"Edad"],main = "Variable Edad",col = "yellow")

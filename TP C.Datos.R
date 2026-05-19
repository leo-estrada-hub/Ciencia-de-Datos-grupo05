#Trabajo grupal Ciencia de Datos
#Integrantes: Ramos 910867 y Estrada 912785
setwd("C:/Users/estra/Desktop/Ciencia de datos 2026/TP ciencia de datos")
getwd()
instub <- 'raw'
outstub <- 'input'
archivo <- 'serie empleo.xlsx'
library(tidyverse)  #para usar múltiples funciones del paquete de librerías  
library(readxl) #leer los excels a utilizar 
library(janitor)  #para limpiar los datos 

empleo<- read_xlsx(archivo ,
          sheet = "T5",
          skip = 1)%>% clean_names() #notar que usando clean names modifica los años con una x por delante de cada una,
                                     #sin embargo, será mas práctico para poder trabajar con dichos años.

sectores <- c(            #defino los sectores para separar luego de las filas provincia 
  "Agricultura, ganadería y pesca",
  "Minería y petróleo",
  "Industria",
  "Comercio",
  "Servicios",
  "Electricidad, gas y agua",
  "Construcción"
)
empleo <- empleo %>%   #modifico la tabla para que distinga provincias de sectores
  mutate(
    es_sector = provincia_en_la_que_declara_empleo %in% sectores,  
    provincia = if_else(         #los que devuelvan false en sectores, es decir, las provincias, se guardaran en esta columna 
      !es_sector,
      provincia_en_la_que_declara_empleo,
      NA_character_
    )
  ) %>%
  fill(provincia)

empleo_long <- empleo %>%
  pivot_longer(
    cols = starts_with("x"),#unifico los años en una sola columna, le asigno el nombre y el valor de cada año lo pongo en una nueva columna
    names_to = "anio",
    values_to = "empleo_total"
  )
empleo_long <- empleo_long %>%
  group_by(provincia, anio) %>%  #agrupo por provincia y año para poder construir el share de empleo 
  mutate(
    share_empleo = empleo_total / first(empleo_total)#utilizo el primer valor ya que en la tabla original, la primer
  )    %>%                                           #fila es la del total de modo que para cada fila del sector por 
  ungroup()                                          #provincia lo divido por el primer valor: el total          
empleo_sector <- empleo_long %>%                     #notar que pierdo observaciones al eliminar las filas de provincias que dan false en es_sector  
  filter(es_sector)  
View(empleo_sector)



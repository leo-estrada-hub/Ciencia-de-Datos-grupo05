library(tidyverse)

base <- readRDS("02_input/tabla_rca.rds")

#1 descargamos base

base_limpia <- base %>%
  rename(empleo = empleo_registrado,
         sector = sector_agregado)


# Cuantos valores tenemos valor = 0?

sum(base_limpia$vab == 0, na.rm = TRUE) #1791 valores con vab igual a 0
mean(base_limpia$vab == 0, na.rm = TRUE)#lo que representa un 10% aprox de la columna vab
sum(base_limpia$empleo == 0, na.rm = TRUE)#3503 valores con empleo igual a 0
mean(base_limpia$empleo == 0, na.rm = TRUE)#lo que representa un 20% de la columna empleo

#Como nos queda la base cuando sacamos estos valores?

#sacando los sectores con valor 0 en empleo y vab queda base de 17092
# 13887/17640= aprox 79% lo que implica depuracion del 21% de la base
# dado que el objetivo es realizar una regresión y ver ventajas comparativas
# valores iguales a 0 no son relevantes, ademas de que la muestra sigue siendo grande
#Lo cual extraerlos posteriormente para el logaritmo no afectaria 
#significativamente la base.

#2 Y si extraemos los servicios?

sectores_no_transables <- c(
  "Construcción",
  "Infraestructura",
  "Enseñanza",
  "Servicios sociales y de salud",
  "Transporte",
  "Comercio mayorista, minorista y reparaciones",
  "Comunicaciones",
  "Servicios financieros",
  "Edición e impresión; reproducción de grabaciones",
  "Servicios de hoteleria y restaurantes"
  )

#3 unimos vector con base
base_limpia <- base_limpia %>%
  filter(!sector %in% sectores_no_transables)
nrow(base_limpia)

# 12600/17640 = aprox 71%, es decir se depura en TOTAL un 29% de la base
# Sin embargo en esta depuracion se eliminan los servicios que ensucian el 
#analisis al desconcentrar el HHI y aumentar empleo de manera 
#artificial, y por ende en la regresion dar siempre un resultado positivo.
#a pesar de la gran depuracion, queda una base grande, lo que genera una
#estadistica certera


unique(base_limpia$sector) #para ver cuantos sectores nos quedaron (de 35 a 25)
saveRDS(base_limpia, "02_input/base_filtrada.rds")

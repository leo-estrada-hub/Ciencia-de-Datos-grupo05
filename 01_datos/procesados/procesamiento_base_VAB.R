#Procesamiento base VAB provincial

library(tidyverse)   
library(readxl) 
library(janitor)   

options(scipen = 999)

instub <- '01_datos/crudos'

ruta_csv <- file.path(instub,'14_Jurisdiccion_52sectores.xlsx')

#LAS VARIABLES ESTAN EN MILLONES DE PESOS A PRECIOS DEL 2004
#creo funcion para que itere con todas las provincias el mismo proceso

procesar_provincia <- function(sheet_name, nombre_provincia){
  read_xlsx(
    ruta_csv,
    sheet = sheet_name,
    skip = 1
  ) %>%
    
#eliminamos primeras 3 y ultimas 4 filas sin datos relevantes   
   
      clean_names() %>%
    slice(-(c(1:3, (n()-3):n()))) %>%
    {
      
#pasamos la primera fila (los anios)      
     
       names(.) <- as.character(unlist(.[1, ]))
      .
    } %>%
    
#eliminamos la fila que trataba como datos a los anios    
    
    slice(-1) %>%
    
#renombramos las ultimas 2 columnas que por ser "preliminares"
#no estan unifomres acorde a las demas    
    
    rename(
      `2023` = `2023 (1)`,
      `2024` = `2024 (2)`
    ) %>%
    
#luego convierto estas columnas de <chr> a <dbl>   
    
    mutate(
      `2023` = as.numeric(`2023`),
      `2024` = as.numeric(`2024`)
    ) %>%
    
#redondeo a 2 decimales todos los datos de la base   
    
    mutate(
      across(where(is.numeric), ~ round(.x, 2))
    ) %>%
    
#creamos una columna que se llame fila que le de numero al numero de fila
#y poder agrupar por numero de fila y no por cada nombre de subsector    
    
    mutate(
      fila = row_number(),
      sector_agregado = case_when(
        fila %in% 1:3   ~ "Agricultura, ganadería y pesca",
        fila %in% 4:5   ~ "Minería y petróleo",
        fila %in% 6:29  ~ "Industria",
        fila %in% 30:32 ~ "Electricidad, gas y agua",
        fila == 33      ~ "Construcción",
        fila == 34      ~ "Comercio",
        fila %in% 35:52 ~ "Servicios"
      )
    ) %>%
    select(-fila) %>%
    
#luego agrupamos (y elimino el VAB total)    
    
    group_by(sector_agregado) %>%
    summarise(
      across(where(is.numeric), sum, na.rm = TRUE)
    ) %>%
    filter(!is.na(sector_agregado)) %>%
  
#agrego un prefijo para cada provincia para que en la tabla 
#pueda ser distinguido
    
    mutate(
      provincia = nombre_provincia,
      .before = 1
    )
}

#uso funcion para cada provincia

vab_caba <- procesar_provincia("Ciudad_de_Buenos_Aires", "CABA")
vab_ba <- procesar_provincia("Buenos_Aires", "Buenos Aires")
vab_cat <- procesar_provincia("Catamarca", "Catamarca")
vab_cha <- procesar_provincia("Chaco", "Chaco")
vab_chu <- procesar_provincia("Chubut", "Chubut")
vab_cor <- procesar_provincia("Cordoba", "Córdoba")
vab_cte <- procesar_provincia("Corrientes", "Corrientes")
vab_er <- procesar_provincia("Entre_Rios", "Entre Ríos")
vab_for <- procesar_provincia("Formosa", "Formosa")
vab_juj <- procesar_provincia("Jujuy", "Jujuy")
vab_lp <- procesar_provincia("La_Pampa", "La Pampa")
vab_lr <- procesar_provincia("La_Rioja", "La Rioja")
vab_mza <- procesar_provincia("Mendoza", "Mendoza")
vab_mis <- procesar_provincia("Misiones", "Misiones")
vab_nqn <- procesar_provincia("Neuquen", "Neuquén")
vab_rn <- procesar_provincia("Rio_Negro", "Río Negro")
vab_sal <- procesar_provincia("Salta", "Salta")
vab_sj <- procesar_provincia("San_Juan", "San Juan")
vab_sl <- procesar_provincia("San_Luis", "San Luis")
vab_sc <- procesar_provincia("Santa_Cruz", "Santa Cruz")
vab_sf <- procesar_provincia("Santa_Fe", "Santa Fe")
vab_sde <- procesar_provincia("Santiago_del_Estero", "Santiago del Estero")
vab_tf <- procesar_provincia("Tierra_del_Fuego", "Tierra del Fuego")
vab_tuc <- procesar_provincia("Tucuman", "Tucumán")

#hago tabla que sume todas las provincias

vab_total <- bind_rows(
  vab_caba, vab_ba, vab_cat, vab_cha, vab_chu,
  vab_cor, vab_cte, vab_er, vab_for, vab_juj,
  vab_lp, vab_lr, vab_mza, vab_mis, vab_nqn,
  vab_rn, vab_sal, vab_sj, vab_sl, vab_sc,
  vab_sf, vab_sde, vab_tf, vab_tuc
)

vab_total_horiz <- vab_total %>% 
  pivot_longer(
    cols = -(c(sector_agregado,provincia)),
    names_to = "anio",
    values_to = "vab"
  )%>%
  mutate(
    anio = as.integer(anio)
  )

#guardo resultado para no correr toda la funcion

saveRDS(vab_total_horiz, "01_datos/procesados/vab_total_horiz.rds")







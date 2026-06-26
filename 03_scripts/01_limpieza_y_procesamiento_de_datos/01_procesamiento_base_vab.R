#Procesamiento base VAB provincial

library(tidyverse)   
library(readxl) 
library(janitor)   

options(scipen = 999)

instub <- '01_raw'

ruta_csv <- file.path(instub,'14_Jurisdiccion_52sectores.xlsx')

#1 creamos funcion para que itere con todas las provincias el mismo proceso

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
    
    #luego se convierte estas columnas de <chr> a <dbl>   
    
    mutate(
      `2023` = as.numeric(`2023`),
      `2024` = as.numeric(`2024`)
    ) %>%
    
    #se redondea a 2 decimales todos los datos de la base   
    
    mutate(
      across(where(is.numeric), ~ round(.x, 2))
    ) %>%
    
    mutate(
      provincia = nombre_provincia,
      .before = 1
    ) %>% filter(
      !`Sector de actividad económica` %in% c("Asociaciones",
                                              "Administración pública y defensa; Planes de seguridad social de afiliación obligatoria",
                                              "VAB a precios básicos")
    ) %>% 
    mutate(
      sector_agregado = case_when(
      `Sector de actividad económica` %in% c("Fabricación de gas ; distribución de combustibles gaseosos por tuberías","Generación captación y distribución de energía eléctrica","Captación , depuración y distribución de agua") 
        ~ "Electricidad, gas y agua",
      `Sector de actividad económica` %in% c( "Extracción de carbón y lignito; extracción de turba. Extracción de petróleo crudo y gas natural; actividades de servicios relacionadas con la extracción de petróleo y gas, excepto las actividades de prospección.", "Extracción de minerales metalíferos. Explotación de  minas y canteras n.c.p.")
      ~ "EXPLOTACION  DE  MINAS  Y  CANTERAS",
      `Sector de actividad económica` %in% c( "Hoteles; campamentos y otros tipos de hospedaje temporal","Restaurantes, bares y cantinas")
      ~ "Servicios de hoteleria y restaurantes",
      `Sector de actividad económica`== "Reparación, mantenimiento e instalación de maquinas y equipos" ~
                                               "Fabricación de maquinaria y equipo n.c.p.",
      `Sector de actividad económica` %in% c( "Propiedad de la vivienda*", "Resto") ~ "Servicios inmobiliarios",
      `Sector de actividad económica` %in% c( "Salud pública", "Salud privada") ~ "Servicios sociales y de salud",
      `Sector de actividad económica` %in% c( "Enseñanza pública", "Enseñanza privada") ~ "Enseñanza",
      `Sector de actividad económica`== "Servicio doméstico*" ~ "Servicios n.c.p.",
        TRUE ~ `Sector de actividad económica`
      )) %>%
  group_by(
    provincia,
    sector_agregado
  ) %>%
    summarise(
      across(where(is.numeric), sum, na.rm = TRUE),
      .groups = "drop"
    )
}


#2 usamos funcion para cada provincia

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

#3 hacemos tabla que sume todas las provincias

vab_total <- bind_rows(
  vab_caba, vab_ba, vab_cat, vab_cha, vab_chu,
  vab_cor, vab_cte, vab_er, vab_for, vab_juj,
  vab_lp, vab_lr, vab_mza, vab_mis, vab_nqn,
  vab_rn, vab_sal, vab_sj, vab_sl, vab_sc,
  vab_sf, vab_sde, vab_tf, vab_tuc
)

vab_total_horiz <- vab_total %>%
  pivot_longer(
    cols = -c(provincia, sector_agregado),
    names_to = "anio",
    values_to = "vab"
  ) %>%
  mutate(anio = as.integer(anio))

#4 cuantas variables hay 
nrow(vab_total_horiz)           #21168  filas
ncol(vab_total_horiz)           #4  columnas
sum(is.na(vab_total_horiz))     #sin NAs
sapply(vab_total_horiz, class)  #provincia y sector_agregado character/ anio integer y vab numeric
glimpse(vab_total_horiz)        #que tiene aprox la tabla


#guardamos resultado para no correr toda la funcion

saveRDS(vab_total_horiz, "02_input/vab_sector.rds")








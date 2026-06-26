#Procesamiento tabla final

library(tidyverse)   

#1 descargo ambas bases
vab_df <- readRDS("02_input/vab_sector.rds")
empleo_df <- readRDS("02_input/empleo_sector.rds")


#2 las unificamos (dado que filas y columnas son las mismas y cumplen mismas caracteristicas)
vab_df <- vab_df %>%
  inner_join(
    empleo_df,
    by = c("provincia", "sector_agregado", "anio")
  )

# Cuantas filas tenemos antes de unificar? 
nrow(vab_df)  #21168

#3 unificamos variables de servicios
vab_df <- vab_df %>%
  mutate(
    sector_agregado = case_when(
     sector_agregado %in% c(
        "Intermediación financiera y otros servicios financieros",
        "Servicios auxiliares a la actividad financiera",
        "Servicios de seguros",
        "Servicios inmobiliarios"
      ) ~ "Servicios financieros",
      
      sector_agregado %in% c(
        "Servicios culturales y deportivos. Otras actividades",
        "Servicios n.c.p.",
        "Servicios sociales y de salud"
      ) ~ "Servicios sociales y de salud",
      
      sector_agregado %in% c(
        "Electricidad, gas y agua",
        "Eliminación de desperdicios y aguas residuales, saneamiento y servicios similares",
        "Reciclamiento"
      ) ~ "Infraestructura",
      
      sector_agregado == "EXPLOTACION  DE  MINAS  Y  CANTERAS" ~
        "Explotacion de minas y canteras",
      TRUE ~ sector_agregado
    )
  )
#4 luego los compactamos en un mismo sector
vab_df <- vab_df %>%
  group_by(provincia, sector_agregado, anio) %>%
  summarise(
    empleo_registrado = sum(empleo_registrado, na.rm = TRUE),
    vab = sum(vab, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(provincia, sector_agregado, anio, vab, empleo_registrado)

#5 con cuantas filas nos quedamos?
nrow(vab_df)          #17640

saveRDS(vab_df, "02_input/tabla_procesados_final.rds")            

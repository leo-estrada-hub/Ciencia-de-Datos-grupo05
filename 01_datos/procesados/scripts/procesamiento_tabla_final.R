#Procesamiento tabla final

library(tidyverse)   
library(readxl) 
library(janitor)   

options(scipen = 999)

#descargo ambas bases
vab_df <- readRDS("01_datos/procesados/vab_total_horiz.rds")
empleo_df <- readRDS("01_datos/procesados/empleo_sector.rds")

vab_df <- vab_df %>%
  inner_join(
    empleo_df,
    by = c("provincia", "sector_agregado", "anio")
  )

saveRDS(vab_df, "01_datos/procesados/tabla_procesados_final.rds")            

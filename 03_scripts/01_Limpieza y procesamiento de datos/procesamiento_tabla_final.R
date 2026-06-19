#Procesamiento tabla final

library(tidyverse)   

#descargo ambas bases
vab_df <- readRDS("input/vab_total_horiz.rds")
empleo_df <- readRDS("input/empleo_sector.rds")

vab_df <- vab_df %>%
  inner_join(
    empleo_df,
    by = c("provincia", "sector_agregado", "anio")
  )

saveRDS(vab_df, "input/tabla_procesados_final.rds")            

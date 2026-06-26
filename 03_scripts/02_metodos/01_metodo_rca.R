library(tidyverse)

tabla_rca <- readRDS("02_input/tabla_procesados_final.rds")

# creamos funcion de rca
calculo_rca <- function(df){
  
  df %>%
    group_by(provincia, anio) %>%
    mutate(
      vab_provincia = sum(vab)    # creo la variable que suma el vab a nivel general de la provincia
    ) %>%
    group_by(sector_agregado, anio) %>%
    mutate(
      vab_sector_nacional = sum(vab) # creo la variable que suma el vab de cada sector a nivel nacional 
    ) %>%
    group_by(anio) %>%
    mutate(
      vab_nacional = sum(vab) # creo la variable que suma el vab a nivel nacional, tanto de cada provincia como de cada sector juntos
    ) %>%
    ungroup() %>%
    mutate(
      rca = (vab / vab_provincia) /
        (vab_sector_nacional / vab_nacional),  # creacion del rca 
      ventaja_comp = if_else(rca > 1, 1, 0)
    )
}

#ahora la tabla contiene el rca y demas variables auxiliares 
tabla_rca <- calculo_rca(tabla_rca)  

saveRDS(tabla_rca,"02_input/tabla_rca.rds")

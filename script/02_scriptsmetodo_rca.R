tabla_rca <- readRDS("01_datos/procesados/rds/tabla_procesados_final.rds")

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
tabla_rca <- calculo_rca(tabla_rca)  #ahora la tabla contiene el rca y demas variables auxiliares 

saveRDS(tabla_rca,"02_scripts/rds/tabla_rca.rds")

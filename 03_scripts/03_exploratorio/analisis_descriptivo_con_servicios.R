
#Para reducir el tamaño script de análisis descriptivo, se hará otro
#igual, con el sufijo "ss" que use la tabla_rca (sin filtro de servicios)

library(tidyverse)
library(gt)
options(scipen = 999)

tabla_rca_ss <- readRDS("02_input/tabla_rca.rds")
nrow(tabla_rca_ss)


###########################################################################
# Y Qué ocurre con el HHI, vab y empleo si agrego los servicios?
###########################################################################

#0 modifico nombres
tabla_rca_ss <- tabla_rca_ss %>% 
  rename(sector = sector_agregado,
         empleo = empleo_registrado)

#0 creamos rca promedio
rca_promedio_ss <- tabla_rca_ss %>% 
  group_by(provincia, sector) %>%
  summarise(
    rca_promedio = mean(rca, na.rm = TRUE),
    .groups = "drop"
  )

#1 creo tabla con sectores con rca promedio>1
sectores_vc_ss <- rca_promedio_ss %>%
  filter(rca_promedio > 1)

#2 creo funcion total sectores
total_sectores_ss <- n_distinct(tabla_rca_ss$sector)

#3 creo tabla con sectores con rca>1 en cada provincia y su porcentaja
#=========================================================================>
sectores_por_provincia_ss <- sectores_vc_ss %>%
  group_by(provincia) %>%
  summarise(cantidad_sectores = n_distinct(sector)) %>% 
  mutate(pct_vc = paste0(round(cantidad_sectores / total_sectores_ss * 100, 2), "%"))
#=========================================================================>
#analisis de distribucion por provincia
sectores_por_provincia_ss %>% 
  summarise(
    media = mean(cantidad_sectores, na.rm = TRUE),
    desvio_estandar = sd(cantidad_sectores, na.rm = TRUE),
    mediana = median(cantidad_sectores, na.rm = TRUE)
  )

############################################################################
#1) Cómo evolucionó el empleo y vab en el sector con RCA>1? 

#1 tabla que de los sectores con rca>1 me de todos los datos

tabla_1_df_ss <- tabla_rca_ss %>%
  semi_join(sectores_vc_ss, by = c("provincia", "sector"))

#2 uso años 2004 y 2024 de cada sector, y creo columnas con empleo y vab de ambos años
#resulta en tabla con variaciones de empleo y vab 

sectores_vc_dif_ss <- tabla_1_df_ss %>%
  arrange(provincia, sector, anio) %>%
  group_by(provincia, sector) %>%
  summarise(
    vab_2004 = first(vab[anio == 2004], default = 0),
    vab_2024 = first(vab[anio == 2024], default = 0),
    empleo_2004 = first(empleo[anio == 2004], default = 0),
    empleo_2024 = first(empleo[anio == 2024], default = 0),
    .groups = "drop"
  ) %>%
  mutate(
    crec_vab = ifelse(
      vab_2004 == 0, 0,paste0(round(100 * (vab_2024 / vab_2004 - 1), 2),"%")),
    crec_empleo = ifelse(empleo_2004 == 0,0,paste0(round(100 * (empleo_2024 / empleo_2004 - 1), 2),"%")))

#3 recreo otra tabla a partir de la unificada para colapsar los sectores en 1
tabla_provincias_ss <- tabla_1_df_ss %>%
  group_by(provincia, anio) %>%
  summarise(
    vab = sum(vab, na.rm = TRUE),
    empleo = sum(empleo, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  group_by(provincia) %>%
  summarise(
    vab_2004 = first(vab[anio == 2004], default = 0),
    vab_2024 = first(vab[anio == 2024], default = 0),
    empleo_2004 = first(empleo[anio == 2004], default = 0),
    empleo_2024 = first(empleo[anio == 2024], default = 0),
    crec_vab = paste0(
      round( 
        ifelse(vab_2004 == 0,0, 100 * (vab_2024 / vab_2004 - 1)),2),"%"),
    crec_empleo = paste0(
      round(
        ifelse(empleo_2004 == 0,0,100 * (empleo_2024 / empleo_2004 - 1)),2),"%"),
    .groups = "drop")

#4 tabla que une los sectores por provincias y las diferencias de empleo y vab

tabla_sec_crec_ss <- sectores_por_provincia_ss %>% 
  left_join(tabla_provincias_ss) %>% 
  select(provincia, cantidad_sectores,pct_vc, crec_vab, crec_empleo)

###############################################################################
#2) Qué tanto cambiaron las concentraciones de los sectores en cada provincia del 2024 al 2004?


#1 vab total 2024

vab_total_ss <- tabla_rca_ss %>% 
  filter(anio == 2024) %>% 
  group_by(provincia) %>% 
  summarise(vab_total = sum(vab, na.rm = TRUE),
            .groups = "drop")

#2 creo hhi 2024

c_hhi_2024_ss <- tabla_rca_ss %>% 
  filter(anio == 2024) %>% 
  left_join(vab_total_ss, by = "provincia") %>% 
  mutate(
    participacion = vab / vab_total
  ) %>% 
  group_by(provincia) %>% 
  summarise(
    hhi_2024 = sum(participacion^2, na.rm = TRUE)*10000,
    .groups = "drop"
  )


#3 vab total 2004

vab_total_ss <- tabla_rca_ss %>% 
  filter(anio == 2004) %>% 
  group_by(provincia) %>% 
  summarise(vab_total = sum(vab, na.rm = TRUE),
            .groups = "drop")

#4 creo hhi 2004

c_hhi_2004_ss <- tabla_rca_ss %>% 
  filter(anio == 2004) %>% 
  left_join(vab_total_ss, by = "provincia") %>% 
  mutate(
    share_vab_2004 = vab / vab_total
  ) %>% 
  group_by(provincia) %>% 
  summarise(
    hhi_2004 = sum(share_vab_2004^2, na.rm = TRUE)*10000,
    .groups = "drop"
  )

#=========================================================================>

#5 diferencia de concentraciones 2024-2004

tabla_hhi_ss <- c_hhi_2004_ss %>%
  left_join(c_hhi_2024_ss, by = "provincia") %>% 
  mutate(
    dif_hhi = paste0(round((hhi_2024 - hhi_2004) / hhi_2004 * 100, 2),"%"))

#=========================================================================>  

###########################################################################
##                 Creamos tabla de descripcion poblacional              ##

#1 unifico tablas para ver cambios en hhi, y en empleo y vab

dif_vs_hhi_ss <- tabla_hhi_ss %>% 
  left_join(tabla_sec_crec_ss, by = "provincia") %>% 
  select(
    provincia,
    cantidad_sectores,
    pct_vc,
    crec_vab,
    crec_empleo,
    dif_hhi
  ) %>%
  rename(
    `Provincia` = provincia,
    `Cant sectores RCA > 1` = cantidad_sectores,
    `% sectores RCA > 1` = pct_vc,
    `Crec VAB` = crec_vab,
    `Crec empleo` = crec_empleo,
    `Var HHI` = dif_hhi
  )
#2 archivo tabla

dif_vs_hhi_ss %>%
  gt() %>%
  tab_header(
    title = "¿Qué tanto cambia agregar servicios? "
  ) %>%
  gtsave(
    filename = "04_output/tablas/tabla_con_servicios_descriptiva_poblacion.png",
    vwidth = 2200,
    vheight = 1000,
    zoom = 2,
    expand = 20
  )
print(dif_vs_hhi_ss, n= Inf)
#############################################################################
##                     Creamos tabla de inferencia                           ##


# Creación de tabla de inferencia (con servicios)

#1 Tests t pareados
tt_vab_ss <- t.test(
  tabla_provincias_ss$vab_2004,
  tabla_provincias_ss$vab_2024,
  paired = TRUE
)

tt_empleo_ss <- t.test(
  tabla_provincias_ss$empleo_2004,
  tabla_provincias_ss$empleo_2024,
  paired = TRUE
)

tt_hhi_ss <- t.test(
  c_hhi_2004_ss$hhi_2004,
  c_hhi_2024_ss$hhi_2024,
  paired = TRUE
)

#2 Tabla resumen
tabla_resumen_ss <- tibble(
  Variable = c(
    "VAB", "VAB", "VAB",
    "Empleo", "Empleo", "Empleo",
    "HHI", "HHI", "HHI"
  ),
  Estadístico = rep(c("Media", "Mediana", "Desvío estándar"), 3),
  `2004` = c(
    round(mean(tabla_provincias_ss$vab_2004, na.rm = TRUE), 2),
    round(median(tabla_provincias_ss$vab_2004, na.rm = TRUE), 2),
    round(sd(tabla_provincias_ss$vab_2004, na.rm = TRUE), 2),
    
    round(mean(tabla_provincias_ss$empleo_2004, na.rm = TRUE), 2),
    round(median(tabla_provincias_ss$empleo_2004, na.rm = TRUE), 2),
    round(sd(tabla_provincias_ss$empleo_2004, na.rm = TRUE), 2),
    
    round(mean(c_hhi_2004_ss$hhi_2004, na.rm = TRUE), 2),
    round(median(c_hhi_2004_ss$hhi_2004, na.rm = TRUE), 2),
    round(sd(c_hhi_2004_ss$hhi_2004, na.rm = TRUE), 2)
  ),
  `2024` = c(
    round(mean(tabla_provincias_ss$vab_2024, na.rm = TRUE), 2),
    round(median(tabla_provincias_ss$vab_2024, na.rm = TRUE), 2),
    round(sd(tabla_provincias_ss$vab_2024, na.rm = TRUE), 2),
    
    round(mean(tabla_provincias_ss$empleo_2024, na.rm = TRUE), 2),
    round(median(tabla_provincias_ss$empleo_2024, na.rm = TRUE), 2),
    round(sd(tabla_provincias_ss$empleo_2024, na.rm = TRUE), 2),
    
    round(mean(c_hhi_2024_ss$hhi_2024, na.rm = TRUE), 2),
    round(median(c_hhi_2024_ss$hhi_2024, na.rm = TRUE), 2),
    round(sd(c_hhi_2024_ss$hhi_2024, na.rm = TRUE), 2)
  ),
  `p-valor` = c(
    round(tt_vab_ss$p.value, 4), "", "",
    round(tt_empleo_ss$p.value, 4), "", "",
    round(tt_hhi_ss$p.value, 4), "", ""
  )
)

#3 Archivar tabla
tabla_resumen_ss %>%
  gt() %>%
  tab_header(
    title = "¿Y qué dice el test pareado?",
    subtitle = "¿Son significativas las variaciones del 2004 al 2024?"
  ) %>%
  gtsave(
    filename = "04_output/tablas/tabla_con_servicios_descriptiva_inferencia.png",
    vwidth = 2200,
    vheight = 1000,
    zoom = 2,
    expand = 20
  )
print(tabla_resumen_ss, n = Inf)











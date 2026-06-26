
#analisis descriptivo

library(tidyverse)
library(gt)
options(scipen = 999)

# descargo base filtrada

tabla_rca <- readRDS("02_input/base_filtrada.rds")
nrow(tabla_rca)

#0 creamos rca promedio

rca_promedio <- tabla_rca %>% 
  group_by(provincia, sector) %>%
  summarise(
    rca_promedio = mean(rca, na.rm = TRUE),
    .groups = "drop"
  )

###########################################################################
#1) Cuantos RCA>1 hay?

#1 creo tabla con sectores con rca promedio>1
sectores_vc <- rca_promedio %>%
  filter(rca_promedio > 1)

#2 creo funcion total sectores
total_sectores <- n_distinct(tabla_rca$sector)

#=============================================================================>

#3 creo tabla con sectores con rca>1 en cada provincia y su porcentaja

sectores_por_provincia <- sectores_vc %>%
  group_by(provincia) %>%
  summarise(cantidad_sectores = n_distinct(sector)) %>% 
  mutate(pct_vc = paste0(round(cantidad_sectores / total_sectores * 100, 2), "%"))

#=============================================================================>

#4 analisis de distribucion de sectores rca>1 por provincia

sectores_por_provincia %>% 
  summarise(
    media = mean(cantidad_sectores, na.rm = TRUE),
    desvio_estandar = sd(cantidad_sectores, na.rm = TRUE),
    mediana = median(cantidad_sectores, na.rm = TRUE)
  )

###########################################################################
#2) Cómo evolucionó el empleo y vab en el sector con RCA>1? 

#1 tabla que de los sectores con rca>1 me de todos los datos

tabla_1_df <- tabla_rca %>%
  semi_join(sectores_vc, by = c("provincia", "sector"))

#2 uso años 2004 y 2024 de cada sector, y creo columnas con empleo y vab de ambos años
#resulta en tabla con variaciones de empleo y vab 

sectores_vc_dif <- tabla_1_df %>%
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
      vab_2004 == 0,0,
      paste0(round(100 * (vab_2024 / vab_2004 - 1), 2),"%")),
    crec_empleo = ifelse(
      empleo_2004 == 0,0,paste0(round(100 * (empleo_2024 / empleo_2004 - 1), 2),"%")))


#3 recreo otra tabla a partir de la unificada para colapsar los sectores en 1
tabla_provincias <- tabla_1_df %>%
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
    crec_vab = paste0(round(ifelse(vab_2004 == 0,0, 100 * (vab_2024 / vab_2004 - 1)),2),"%"),
    crec_empleo = paste0(round(ifelse(empleo_2004 == 0,0, 100 * (empleo_2024 / empleo_2004 - 1)),2),"%"),
    .groups = "drop")

#=============================================================================>

#4 tabla que une los sectores por provincias y las diferencias de empleo y vab

tabla_sec_crec <- sectores_por_provincia %>% 
  left_join(tabla_provincias) %>% 
  select(provincia, cantidad_sectores,pct_vc, crec_vab, crec_empleo)

#=============================================================================>

###########################################################################
#3) Qué tanto cambiaron las concentraciones de los sectores en cada provincia del 2024 al 2004?

#1 vab total 2024

vab_total <- tabla_rca %>% 
  filter(anio == 2024) %>% 
  group_by(provincia) %>% 
  summarise(vab_total = sum(vab, na.rm = TRUE),
            .groups = "drop")

#2 creo hhi 2024

c_hhi_2024 <- tabla_rca %>% 
  filter(anio == 2024) %>% 
  left_join(vab_total, by = "provincia") %>% 
  mutate(participacion = vab / vab_total
  ) %>% 
  group_by(provincia) %>% 
  summarise(
    hhi_2024 = sum(participacion^2, na.rm = TRUE)*10000,
    .groups = "drop")

#3 vab total 2004

vab_total <- tabla_rca %>% 
  filter(anio == 2004) %>% 
  group_by(provincia) %>% 
  summarise(vab_total = sum(vab, na.rm = TRUE),
            .groups = "drop")

#4 creo hhi 2004

c_hhi_2004 <- tabla_rca %>% 
  filter(anio == 2004) %>% 
  left_join(vab_total, by = "provincia") %>% 
  mutate(
    share_vab_2004 = vab / vab_total
  ) %>% 
  group_by(provincia) %>% 
  summarise(
    hhi_2004 = sum(share_vab_2004^2, na.rm = TRUE)*10000,
    .groups = "drop")

#=========================================================================>

#5 diferencia de concentraciones 2024-2004

tabla_hhi <- c_hhi_2004 %>%
  left_join(c_hhi_2024, by = "provincia") %>% 
  mutate(
    dif_hhi = paste0(round((hhi_2024 - hhi_2004) / hhi_2004 * 100, 2),"%"))

#=========================================================================>  

#########################################################################
##                  Creacion de tabla descriptiva                     ##

#1 unifico tablas para ver cambios en hhi, y en empleo y vab

dif_vs_hhi <- tabla_hhi %>% 
  left_join(tabla_sec_crec, by = "provincia") %>% 
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

#2 creo tabla
dif_vs_hhi %>%
  gt() %>%
  tab_header(
    title = "Resumen por provincia"
  ) %>%
  gtsave(
    filename = "04_output/tablas/tabla_descriptiva_poblacion.png",
    vwidth = 2200,
    vheight = 1000,
    zoom = 2,
    expand = 20
  )
print(dif_vs_hhi, n = Inf)
###########################################################################
##                    Creacion de tabla de inferencia                     ##

# 1 tests t pareados
tt_vab <- t.test(
  tabla_provincias$vab_2004,
  tabla_provincias$vab_2024,
  paired = TRUE
)

tt_empleo <- t.test(
  tabla_provincias$empleo_2004,
  tabla_provincias$empleo_2024,
  paired = TRUE
)

tt_hhi <- t.test(
  c_hhi_2004$hhi_2004,
  c_hhi_2024$hhi_2024,
  paired = TRUE
)

#2 creamos tabla resumen
tabla_resumen <- tibble(
  Variable = c(
    "VAB", "VAB", "VAB",
    "Empleo", "Empleo", "Empleo",
    "HHI", "HHI", "HHI"
  ),
  Estadístico = rep(c("Media", "Mediana", "Desvío estándar"), 3),
  `2004` = c(
    round(mean(tabla_provincias$vab_2004, na.rm = TRUE), 2),
    round(median(tabla_provincias$vab_2004, na.rm = TRUE), 2),
    round(sd(tabla_provincias$vab_2004, na.rm = TRUE), 2),
    
    round(mean(tabla_provincias$empleo_2004, na.rm = TRUE), 2),
    round(median(tabla_provincias$empleo_2004, na.rm = TRUE), 2),
    round(sd(tabla_provincias$empleo_2004, na.rm = TRUE), 2),
    
    round(mean(c_hhi_2004$hhi_2004, na.rm = TRUE), 2),
    round(median(c_hhi_2004$hhi_2004, na.rm = TRUE), 2),
    round(sd(c_hhi_2004$hhi_2004, na.rm = TRUE), 2)
  ),
  `2024` = c(
    round(mean(tabla_provincias$vab_2024, na.rm = TRUE), 2),
    round(median(tabla_provincias$vab_2024, na.rm = TRUE), 2),
    round(sd(tabla_provincias$vab_2024, na.rm = TRUE), 2),
    
    round(mean(tabla_provincias$empleo_2024, na.rm = TRUE), 2),
    round(median(tabla_provincias$empleo_2024, na.rm = TRUE), 2),
    round(sd(tabla_provincias$empleo_2024, na.rm = TRUE), 2),
    
    round(mean(c_hhi_2024$hhi_2024, na.rm = TRUE), 2),
    round(median(c_hhi_2024$hhi_2024, na.rm = TRUE), 2),
    round(sd(c_hhi_2024$hhi_2024, na.rm = TRUE), 2)
  ),
  `p-valor` = c(
    round(tt_vab$p.value, 4), "", "",
    round(tt_empleo$p.value, 4), "", "",
    round(tt_hhi$p.value, 4), "", ""
  )
)

#3 archivo tabla

tabla_resumen %>%
  gt() %>%
  tab_header(
    title = "Inferencia de cambio en empleo, VAB y HHI",
    subtitle = "¿Son significativos las variaciones del 2004 al 2024?"
  ) %>%
  gtsave(
    filename = "04_output/tablas/tabla_descriptiva_inferencia.png",
    vwidth = 2200,
    vheight = 1000,
    zoom = 2,
    expand = 20
  )
print(tabla_resumen, n= Inf)

###########################################################################
#4) Cual es el RCA>1 mas alto en cada provincia?

#=========================================================================>
#
rca_max <- rca_promedio %>%
  group_by(provincia) %>%
  filter(rca_promedio == max(rca_promedio, na.rm = TRUE)) %>%
  ungroup()
#=========================================================================>

###########################################################################
#5) Y Cómo evolucionó el empleo en el sector con mas RCA>1?


#uno tabla de rca maximo con tabla rca
#1
tabla_max_empleo_vc <- tabla_rca %>% 
  inner_join(
    rca_max,
    by = c("provincia", "sector")
  ) %>% 
  filter (anio %in% c(2004,2024))

#=========================================================================>

#2
dif_empleo_vc_max <- tabla_max_empleo_vc %>%
  select(provincia, sector, anio, empleo) %>%
  pivot_wider(
    names_from = anio,
    values_from = empleo,
    names_prefix = "empleo_",
    values_fill = 0
  ) %>% 
  mutate(dif_empleo = paste0(round((empleo_2024 - empleo_2004) / empleo_2004 * 100, 2),"%"))

#=========================================================================>

###########################################################################
##                    Creacion de tabla de max RCA>1                    ##

var_max_rca <- rca_max %>% 
  left_join(dif_empleo_vc_max, by = c("provincia", "sector")) %>%
  mutate(
    sector = dplyr::recode(
      sector,
      "Producción de madera y fabricación de productos de madera y corcho, excepto muebles; fabricación de artículos de paja y de materiales trenzables" =
        "Producción de madera y fabricación de productos de madera"
    )
  )%>%
  select(
    provincia,
    sector,
    rca_promedio,
    empleo_2004,
    empleo_2024,
    dif_empleo
  ) %>%
  rename(
    `Provincia` = provincia,
    `Sector` = sector,
    `Max RCA provincia` = rca_promedio,
    `Empleo 2004` = empleo_2004,
    `Empleo 2024` = empleo_2024,
    `Dif de empleo` = dif_empleo
  )
#4 creo tabla
var_max_rca %>%
  gt() %>%
  tab_header(
    title = "RCA>1 promedio max por provincia"
  ) %>%
  gtsave(
    filename = "04_output/tablas/tabla_descriptiva_rca_max.png",
    vwidth = 2200,
    vheight = 1000,
    zoom = 2,
    expand = 20
    )
print(var_max_rca, n = Inf) 













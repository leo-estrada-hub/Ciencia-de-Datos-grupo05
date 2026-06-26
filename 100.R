library(tidyverse)
library(gt)

options(scipen = 999)
tabla_rca_servicios <- readRDS("02_input/tabla_rca.rds")
tabla_rca <- readRDS("02_input/base_filtrada.rds")
tabla_rca_servicios <- tabla_rca_servicios %>%
  rename(
    sector = sector_agregado,
    empleo = empleo_registrado
  )
#========================================================
# FUNCIÓN GENERAL (BASE O SERVICIOS)
#========================================================

analisis_rca <- function(data, label = "base") {
  
  #------------------------------------------------------
  # 1. RCA promedio
  #------------------------------------------------------
  rca_prom <- data %>%
    group_by(provincia, sector) %>%
    summarise(rca_promedio = mean(rca, na.rm = TRUE), .groups = "drop")
  
  sectores_vc <- rca_prom %>%
    filter(rca_promedio > 1)
  
  total_sectores <- n_distinct(data$sector)
  
  sectores_por_prov <- sectores_vc %>%
    group_by(provincia) %>%
    summarise(cantidad_sectores = n_distinct(sector), .groups = "drop") %>%
    mutate(pct_vc = cantidad_sectores / total_sectores * 100)
  
  #------------------------------------------------------
  # 2. Evolución VAB y empleo (2004 vs 2024)
  #------------------------------------------------------
  base_agg <- data %>%
    semi_join(sectores_vc, by = c("provincia", "sector")) %>%
    group_by(provincia, anio) %>%
    summarise(
      vab = sum(vab, na.rm = TRUE),
      empleo = sum(empleo, na.rm = TRUE),
      .groups = "drop"
    ) %>%
    pivot_wider(
      names_from = anio,
      values_from = c(vab, empleo)
    ) %>%
    mutate(
      crec_vab = (vab_2024 / vab_2004 - 1) * 100,
      crec_empleo = (empleo_2024 / empleo_2004 - 1) * 100
    )
  
  #------------------------------------------------------
  # 3. HHI
  #------------------------------------------------------
  hhi <- data %>%
    group_by(provincia, anio) %>%
    mutate(share = vab / sum(vab, na.rm = TRUE)) %>%
    summarise(hhi = sum(share^2) * 10000, .groups = "drop") %>%
    pivot_wider(names_from = anio, values_from = hhi) %>%
    mutate(dif_hhi = ( `2024` / `2004` - 1 ) * 100)
  
  #------------------------------------------------------
  # 4. Tests t
  #------------------------------------------------------
  tt_vab <- t.test(base_agg$vab_2004, base_agg$vab_2024, paired = TRUE)
  tt_emp <- t.test(base_agg$empleo_2004, base_agg$empleo_2024, paired = TRUE)
  tt_hhi <- t.test(hhi$`2004`, hhi$`2024`, paired = TRUE)
  
  #------------------------------------------------------
  # 5. Tabla final
  #------------------------------------------------------
  tabla_final <- sectores_por_prov %>%
    left_join(base_agg, by = "provincia") %>%
    left_join(hhi, by = "provincia") %>%
    transmute(
      Provincia = provincia,
      `Sectores RCA>1` = cantidad_sectores,
      `% RCA>1` = pct_vc,
      `Crec VAB` = crec_vab,
      `Crec Empleo` = crec_empleo,
      `Δ HHI` = dif_hhi
    )
  
  print(
    tabla_final %>%
      gt() %>%
      tab_header(title = paste("Resumen", label))
  )
  
  #------------------------------------------------------
  # 6. Máximo RCA
  #------------------------------------------------------
  rca_max <- rca_prom %>%
    group_by(provincia) %>%
    slice_max(rca_promedio, n = 1)
  
  list(
    tabla = tabla_final,
    rca_max = rca_max,
    tests = list(vab = tt_vab, empleo = tt_emp, hhi = tt_hhi)
  )
}

res_base <- analisis_rca(tabla_rca, "Base")

res_servicios <- analisis_rca(tabla_rca_servicios, "Con servicios")


#creo tabla sin ss
res_base$tabla %>%
  gt() %>%
  tab_header(
    title = "Resumen por provincia"
  ) %>%
  gtsave(
    filename = "04_output/tabla_descriptiva_poblacion.png",
    vwidth = 2200,
    vheight = 1000,
    zoom = 2,
    expand = 20
  )

#creo tabla con ss
res_servicios$tabla %>%
  gt() %>%
  tab_header(
    title = "Resumen por provincia (con servicios)"
  ) %>%
  gtsave(
    filename = "04_output/tabla_descriptiva_poblacion_ss.png",
    vwidth = 2200,
    vheight = 1000,
    zoom = 2,
    expand = 20
  )

#========================================================
# FUNCIÓN DE ANÁLISIS DESCRIPTIVO
#========================================================

analisis_descriptivo <- function(base_agg, hhi, sectores_por_prov){
  
  tt_vab <- t.test(
    base_agg$vab_2004,
    base_agg$vab_2024,
    paired = TRUE
  )
  
  tt_empleo <- t.test(
    base_agg$empleo_2004,
    base_agg$empleo_2024,
    paired = TRUE
  )
  
  tt_hhi <- t.test(
    hhi$`2004`,
    hhi$`2024`,
    paired = TRUE
  )
  
  tabla_descriptiva <- tibble(
    Variable = c(
      "Cantidad sectores RCA>1","Cantidad sectores RCA>1","Cantidad sectores RCA>1",
      "VAB","VAB","VAB",
      "Empleo","Empleo","Empleo",
      "HHI","HHI","HHI"
    ),
    
    Estadístico = rep(c("Media","Mediana","Desvío estándar"),4),
    
    Valor = c(
      mean(sectores_por_prov$cantidad_sectores),
      median(sectores_por_prov$cantidad_sectores),
      sd(sectores_por_prov$cantidad_sectores),
      
      "", "", "",
      "", "", "",
      "", "", ""
    ),
    
    `2004` = c(
      "", "", "",
      
      mean(base_agg$vab_2004),
      median(base_agg$vab_2004),
      sd(base_agg$vab_2004),
      
      mean(base_agg$empleo_2004),
      median(base_agg$empleo_2004),
      sd(base_agg$empleo_2004),
      
      mean(hhi$`2004`),
      median(hhi$`2004`),
      sd(hhi$`2004`)
    ),
    
    `2024` = c(
      "", "", "",
      
      mean(base_agg$vab_2024),
      median(base_agg$vab_2024),
      sd(base_agg$vab_2024),
      
      mean(base_agg$empleo_2024),
      median(base_agg$empleo_2024),
      sd(base_agg$empleo_2024),
      
      mean(hhi$`2024`),
      median(hhi$`2024`),
      sd(hhi$`2024`)
    ),
    
    `p-value` = c(
      "", "", "",
      tt_vab$p.value,"","",
      tt_empleo$p.value,"","",
      tt_hhi$p.value,"",""
    )
  )
  
  list(
    tabla = tabla_descriptiva,
    tests = list(
      vab = tt_vab,
      empleo = tt_empleo,
      hhi = tt_hhi
    )
  )
  
}






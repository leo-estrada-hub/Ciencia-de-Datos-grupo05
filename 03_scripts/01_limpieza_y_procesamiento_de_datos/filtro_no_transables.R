library(tidyverse)

base <- readRDS("02_input/tabla_rca.rds")

#EMPIEZA LIMPIEZA

base <- base %>%
  rename(empleo = empleo_registrado,
         sector = sector_agregado)

#USO EXCLUSIVO SI SE OPERA CON LOG
base_limpia <- base %>%
  filter(
    !is.na(vab),
    !is.na(empleo),
    vab > 0,
    empleo > 0
  )

sectores_no_transables <- c(
  "Construcción",
  "Infraestructura",
  "Enseñanza",
  "Servicios sociales y de salud",
  "Transporte",
  "Comercio mayorista, minorista y reparaciones",
  "Comunicaciones",
  "Servicios financieros",
  "Edición e impresión; reproducción de grabaciones",
  "Servicios de hoteleria y restaurantes"
  )

unique(base_limpia$sector)
base_limpia <- base_limpia %>%
  filter(!sector %in% sectores_no_transables)

saveRDS(base_limpia, "02_input/base_filtrada.rds")

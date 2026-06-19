library(tidyverse)

base <- readRDS("input/tabla_rca.rds")

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
  "Reciclamiento",
  "Electricidad, gas y agua",
  "Captación , depuración y distribución de agua",
  "Construcción",
  "Comercio mayorista, minorista y reparaciones",
  "Servicios de hoteleria y restaurantes",
  "Transporte",
  "Comunicaciones",
  "Eliminación de desperdicios y aguas residuales, saneamiento y servicios similares",
  "Servicios culturales y deportivos. Otras actividades"
)

base_limpia <- base_limpia %>%
  filter(!sector %in% sectores_no_transables)

saveRDS(base_limpia, "input/base_filtrada.rds")
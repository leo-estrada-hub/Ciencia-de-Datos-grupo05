#creación de la variación del empleo 

library(tidyverse)

tabla_rca <- readRDS("02_scripts/rds/tabla_rca.rds") #utilizamos el rds del rca ya construido


rca_promedio <- tabla_rca %>%  #generamos un promedio de los rca a lo largo de los 21 años (2004 a 2024)
  group_by(provincia, sector_agregado) %>%
  summarise(
    rca_promedio = mean(rca, na.rm = TRUE),
    .groups = "drop"
  )
sectores_vc <- rca_promedio %>%
  filter(rca_promedio > 1) #nos quedamos con los promedios mayores a 1 y diremos que tuvieron ventaja comparativa en promedio

empleo_vc <- tabla_rca %>%
  filter(anio %in% c(2004, 2024)) %>% #filtramos por los años extremos de la tabla
  inner_join(
    sectores_vc,
    by = c("provincia", "sector_agregado") #juntamos ambas tablas conservando solo los sectores de la tabla sectores_rca
  )
empleo_mapa <- empleo_vc %>%
  group_by(provincia, anio) %>%
  summarise(
    empleo_total_vc = sum(empleo_registrado, na.rm = TRUE),
    .groups = "drop" #sumamos los empleos de cada sector acorde al año y provincia 
  ) %>%
  pivot_wider(
    names_from = anio,
    values_from = empleo_total_vc #convertimos los años en columnas
  ) %>%
  rename(
    empleo_2004 = `2004`, #renombramos ambas columnas  
    empleo_2024 = `2024`
  ) %>%
  mutate(  variacion_pct = (empleo_2024 / empleo_2004 - 1) * 100)

#creación del mapa

library(tidyverse)
library(sf)
library(geoAr)
library(ggtext)
library(scales)


cap <- "Datos: elaboración propia en base a VAB provincial y empleo registrado.
Para cada provincia se agregan todos los sectores con RCA promedio (2004-2024) > 1.
La variable representada corresponde a la variación del empleo entre 2004 y 2024." 

theme_owid_map <- function(base_size = 13) {
  theme_void(base_size = base_size) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_markdown(face = "bold", size = rel(1.3),
                                       colour = "#1d1d1d", 
                                       hjust = 0.5,   # <- centra
                                       lineheight = 1.2,
                                       margin = margin(b = 4)),
      plot.subtitle = element_markdown(size = rel(0.98), colour = "#5b5b5b",
                                       hjust = 0.5,
                                       margin = margin(b = 14)),
      plot.caption  = element_markdown(hjust = 0, size = rel(0.72),
                                       colour = "#8a8a8a", margin = margin(t = 12)),
      legend.position = "bottom",
      legend.title    = element_text(size = rel(0.8), colour = "#5b5b5b"),
      legend.text     = element_text(face= "bold", size = rel(0.72), colour = "#5b5b5b"),
      plot.margin     = margin(14, 16, 10, 16)
    )
}


# -----------------------------------------------------------------------------
# 1) GEOMETRIA PROVINCIAL  (geoAr + recorte para sacar la Antartida y
#    conservar el continente, Malvinas y Tierra del Fuego)
# -----------------------------------------------------------------------------
arg <- get_geo("ARGENTINA", level = "provincia") %>%
  add_geo_codes() %>%
  st_make_valid()

arg <- st_crop(arg, st_bbox(c(xmin = -74, xmax = -52, ymin = -56, ymax = -21),
                            crs = st_crs(arg)))


mapa_datos <- arg %>%
  left_join (empleo_mapa,
             by = c("name_iso" = "provincia")
  )


titulo_mapa <- "¿Cómo cambió el empleo en los sectores más competitivos de cada provincia?"  

g_mapa <- ggplot(mapa_datos) +
  
  geom_sf(aes(fill = variacion_pct), colour = "white", linewidth = 0.2) +
  scale_fill_gradient(
    name = "Variación del empleo (%)",
    low = "#deebf7",
    high = "#08306b",
    limits = c(0, quantile(mapa_datos$variacion_pct, 0.95, na.rm = TRUE)),
    oob = scales::squish
  )+
  coord_sf(expand = FALSE) +
  labs(title = titulo_mapa,
       subtitle ="Variación del empleo entre 2004 y 2024 de los sectores con RCA promedio mayor a 1",
       caption = cap) +
  theme_owid_map() +
   
  guides(fill = guide_colorsteps(barwidth = 14, barheight = 0.5,
                                 title.position = "top", title.hjust = 0.5))


ggsave("mapa variacion empleo.png", g_mapa,
       width =10,height = 12,dpi = 300, bg = "white")

print(g_mapa)
























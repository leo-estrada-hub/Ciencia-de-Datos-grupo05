#calculo de hhi
hhi <- readRDS("01_datos/procesados/tabla_rca.rds")

tabla_hhi <- tabla_rca %>%
  group_by(provincia, anio) %>%
  mutate(
    share_vab = vab / sum(vab)
  )

calculo_hhi <- tabla_hhi %>%
  group_by(provincia, anio) %>%
  summarise(
    hhi = sum(share_vab^2),
    .groups = "drop"
  ) 

#por si no tenes las librerias 

#install.packages("sf")
#install.packages("geoAR")
#install.packages("ggtext")
#install.packages("scales")

library(tidyverse)
library(sf)
library(geoAr)
library(ggtext)
library(scales)


cap <- "Datos: elaboración propia en base a datos de VAB provincial.\n HHI calculado sobre la participación sectorial del VAB. Año 2024." 

theme_owid_map <- function(base_size = 13) {
  theme_void(base_size = base_size) +
    theme(
      plot.title.position   = "plot",
      plot.caption.position = "plot",
      plot.title    = element_markdown(face = "bold", size = rel(1.3),
                                       colour = "#1d1d1d", lineheight = 1.2,
                                       margin = margin(b = 4)),
      plot.subtitle = element_markdown(size = rel(0.98), colour = "#5b5b5b",
                                       margin = margin(b = 14)),
      plot.caption  = element_markdown(hjust = 0, size = rel(0.72),
                                       colour = "#8a8a8a", margin = margin(t = 12)),
      legend.position = "bottom",
      legend.title    = element_text(size = rel(0.8), colour = "#5b5b5b"),
      legend.text     = element_text(size = rel(0.72), colour = "#5b5b5b"),
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
  left_join( calculo_hhi %>% filter(anio == 2024),
             by = c("name_iso" = "provincia")
  )


titulo_mapa <- "Concentración productiva de las provincias argentinas"  

g_mapa <- ggplot(mapa_datos) +
  
geom_sf(aes(fill = hhi), colour = "white", linewidth = 0.2) +
  
scale_fill_fermenter(palette = "Blues", direction = 1, n.breaks = 5,
                     name = "Indicador HHI") +
  
  coord_sf(expand = FALSE) +
  labs(title = titulo_mapa,
       subtitle ="Indice HHI de concentración productiva.\n Calculado a partir de la composición sectorial del VAB (2024).",
       caption = cap) +
  theme_owid_map() +
  
  guides(fill = guide_colorsteps(barwidth = 14, barheight = 0.5,
                                 title.position = "top", title.hjust = 0))

print(g_mapa)

ggsave("mapa HHI provincial.png", g_mapa,
       width =10,height = 12,dpi = 300, bg = "white")

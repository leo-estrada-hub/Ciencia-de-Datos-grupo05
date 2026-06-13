#Grafico exploratorio
#1
library(tidyverse)
#2
base <- readRDS("02_scripts/rds/tabla_rca.rds")
#3
base_plot <- base %>%
  rename(empleo = empleo_registrado) %>% 
  select(vab, empleo, rca) %>% #trabajamos solo con estas columnas 
  filter(
    !is.na(vab),
    !is.na(empleo),
    vab > 0,
    empleo > 0 #limpiamos la base para luego poder usar logaritmos 
  ) %>%
  mutate(
    dummy_rca = factor(
      if_else(rca > 1, 1, 0),
      levels = c(0, 1),
      labels = c("RCA ≤ 1", "RCA > 1") #creamos la dummie 
    ))
#4
g_rca <- ggplot( #grafico del scatterplot 
  base_plot,
  aes(
    x = log(vab),
    y = log(empleo),
    color = factor(dummy_rca)
  )
) +
  geom_point(alpha = 0.1, size = 1.1) + #puntos
  geom_smooth(method = "lm", se = TRUE, linewidth = 2)+ #rectas
  labs(
    x = "log(VAB)",
    y = "log(Empleo)",
    color = "RCA"
  ) +scale_color_manual(
    values = c("RCA ≤ 1" = "#7A7A7A",  
               "RCA > 1" = "#0072B2")   #modificamos colores de los puntos y las rectas
  ) 
#5
print(g_rca) #vemos el gráfico




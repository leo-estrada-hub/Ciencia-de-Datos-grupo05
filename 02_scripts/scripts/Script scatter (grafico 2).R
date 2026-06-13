#Grafico exploratorio
base <- readRDS("02_scripts/rds/tabla_rca.rds")

base_plot <- base %>%
  rename(empleo = empleo_registrado) %>%
  select(vab, empleo, rca) %>%
  filter(
    !is.na(vab),
    !is.na(empleo),
    vab > 0,
    empleo > 0
  ) %>%
  mutate(
    dummy_rca = factor(
      if_else(rca > 1, 1, 0),
      levels = c(0, 1),
      labels = c("RCA ≤ 1", "RCA > 1")
    )
  )

g_rca <- ggplot(
  base_plot,
  aes(
    x = log(vab),
    y = log(empleo),
    color = factor(dummy_rca)
  )
) +
  geom_point(alpha = 0.5, size = 1.3) +
  geom_smooth(method = "lm", se = FALSE)+
  labs(
    x = "log(VAB)",
    y = "log(Empleo)",
    color = "RCA"
  ) +scale_color_manual(
    values = c("RCA ≤ 1" = "#4D4D4D",  
               "RCA > 1" = "#0072B2")   
  )

ggsave("C:/Users/estra/Desktop/tercera_entrega_5/scatter_vab_empleo.png", plot = g_rca , width = 8, height = 6, dpi = 300)


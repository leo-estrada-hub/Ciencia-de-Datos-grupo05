library(tidyverse)
library(broom)
library(ggplot2)
library(lmtest)     # bptest(), coeftest()
library(sandwich)   # vcovHC() — errores robustos
library(car)        # vif()
library(gt)

options(scipen = 999)
theme_set(theme_minimal(base_size = 12))

base <- readRDS('02_input/base_filtrada.rds')


#renombro por comodidad

base <- base %>%
  rename(empleo = empleo_registrado,
         sector = sector_agregado)


#filtro la base con datos relevantes para regresion

base_regresion <- base %>% 
  select(vab,empleo,rca)

#1.primer vistazo a los datos
base_regresion %>% 
  ggplot(aes(vab,empleo)) +
  geom_point()

#2.vemos que si queremos pasar a log, hay muchos valores que son 0, lo cual
#pierde sentido matematico


#2.Cuantos valores para vab y empleo
sum(is.na(base_regresion$vab))
sum(base_regresion$vab <= 0, na.rm = TRUE)

sum(is.na(base_regresion$empleo))
sum(base_regresion$empleo <= 0, na.rm = TRUE)
#aproximadamente 4000

mean(base_regresion$vab == 0, na.rm = TRUE)
mean(base_regresion$empleo == 0, na.rm = TRUE)
#vemos cuanto representan esos datos en cada variable


#limpio la base
base_regresion_limpia <- base_regresion %>%
  filter(
    !is.na(vab),
    !is.na(empleo),
    vab > 0,
    empleo > 0
  )

#hago regresion
base_regresion_limpia %>% 
  ggplot(aes(vab,empleo)) +
  geom_point()

#3.filtramos los 0 (creemos que dada nuestra base de 20mil extraer datos=0 no
# cambia en la conclusion)
base_regresion_limpia %>% 
  ggplot(aes(log(vab),log(empleo))) +
  geom_point()


# Versión naive: en niveles
mod_simple_nivel <- lm(empleo ~ vab, data = base_regresion_limpia)
summary(mod_simple_nivel)

# Versión con log(pib_pc): elasticidad
mod_simple_log <- lm(log(empleo) ~ log(vab), data = base_regresion_limpia)
summary(mod_simple_log)

#verifico resumen de ambos modelos (sobre todo BIC y AIC)
bind_rows(
  glance(mod_simple_nivel) |> mutate(modelo = "Niveles"),
  glance(mod_simple_log)   |> mutate(modelo = "logaritmica")
) |>
  select(modelo, r.squared, adj.r.squared, sigma, AIC, BIC)


#AÑADO DUMMIE RCA (que es binaria; 0 o 1)

#creo dummy para rca>1 y agrego a base
base_regresion_limpia <- base_regresion_limpia %>%
  mutate(dummy_rca = if_else(rca > 1, 1, 0))


modelo_con_dummie <- lm(log(empleo) ~ log(vab) + dummy_rca,
               data = base_regresion_limpia)
summary(modelo_con_dummie)

#resumen del modelo
bind_rows(
  glance(modelo_con_dummie)   |> mutate(modelo = "con dummie")
) |>
  select(modelo, r.squared, adj.r.squared, sigma, AIC, BIC)
#ajusta mejor por R2 y BIC y AIC

#veo cuanto me da de multicolinealidad
vif(modelo_con_dummie)
#me da baja

#AGREGO VAB*RCA

modelo_completo <- lm(
  log(empleo) ~ log(vab) + dummy_rca + log(vab) * dummy_rca , 
 data = base_regresion_limpia
  )
summary(modelo_completo)
#el resumen demuestra que El efecto del VAB sobre el empleo es 
#aproximadamente 0,14 puntos mayor en los sectores con ventaja 
#comparativa que en los sectores sin ventaja comparativa.


bind_rows(
  glance(modelo_con_dummie)   |> mutate(modelo = "con dummie"),
  glance(modelo_completo)   |> mutate(modelo = "completo")
) |>
  select(modelo, r.squared, adj.r.squared, sigma, AIC, BIC)

#veo cuanto me da de multicolinealidad
vif(modelo_completo)
#claramente el modelo completo es el que mejor ajusta

#Grafico exploratorio
ggplot(
  base_regresion_limpia,
  aes(
    x = log(vab),
    y = log(empleo),
    color = factor(dummy_rca)
  ))+
  geom_point(alpha = 0.15, size= 1) +
               geom_smooth(method = "lm", se = FALSE, linewidth = 1.2) +
               labs(
                 x = "log(VAB)",
                 y = "log(Empleo)",
                 color = "RCA"
               ) + scale_color_manual(
                 values = c("grey70", "#1f77b4"),
                 labels = c("RCA ≤ 1", "RCA > 1")
               )


#Visualización de coeficientescon gt
tidy(modelo_completo) %>%
  gt()%>%
  fmt_number(
    columns = c(estimate, std.error, statistic,p.value),
    decimals = 3
  )

#Visualización de estadística con gt

glance(modelo_completo)%>% #seleccionamos los datos a visualizar
  select(
    nobs,
    r.squared,
    adj.r.squared,
    sigma,
    statistic,
    p.value
  ) %>%
  pivot_longer( #transformamos la tabla horizontal a vertical
    everything(),
    names_to = "Estadística", #renombramos columnas
    values_to = "Valor"
  ) %>%
  mutate(
    Estadística = dplyr::recode( #para que no lea otro recode de otro paquete (ejemplo car)
      Estadística,
      nobs = "Observaciones",
      r.squared = "R²",
      adj.r.squared = "R² ajustado",
      sigma = "Error estándar residual",
      statistic = "Estadístico F",
      p.value = "p-valor"
    )
  )%>%
  gt()%>%
  fmt_number(  #para eliminar los ceros de observaciones y que solo quede el número cardinal de observaciones
    columns = Valor,
    rows = Estadística == "Observaciones",
    decimals = 0
  ) %>%
  fmt_number( #para el resto de valores se les deja hasta 3 decimales
    columns = Valor,
    rows = Estadística != "Observaciones",
    decimals = 3
  )




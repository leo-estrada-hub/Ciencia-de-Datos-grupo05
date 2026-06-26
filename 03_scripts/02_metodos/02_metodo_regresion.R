library(tidyverse)
library(broom)      # resultados de regresion ordenados
library(lmtest)     # bptest(), coeftest()
library(sandwich)   # vcovHC() — errores robustos
library(car)        # vif() — prueba de hipotesis conjunta 
library(gt)         # para mejorar visualizacion de tablas
library(webshot2)   #para la conversion de html a png
options(scipen = 999) #evita notacion cientifica
theme_set(theme_minimal(base_size = 12)) #grafica por defecto para analizar regresiones

base <- readRDS('02_input/base_filtrada.rds')
base_con_ss <- readRDS('02_input/tabla_rca.rds')

#0. filtramos valores =0 para cuando use logaritmo
base <- base %>%
  filter(
    !is.na(vab),
    !is.na(empleo),
    vab > 0,
    empleo > 0
  )   
nrow(base)

base_con_ss <- base_con_ss %>%
  filter(
    !is.na(vab),
    !is.na(empleo_registrado),
    vab > 0,
    empleo_registrado > 0
  )   
nrow(base_con_ss)


#0. filtramos la base con datos relevantes para regresion

base_regresion <- base %>% 
  select(vab,empleo,rca)


#1.primer vistazo a los datos
base_regresion %>% 
  ggplot(aes(vab,empleo)) +
  geom_point()
#el resultado es una forma extraña en niveles.
#si se aplica variaciones en el tiempo (log) nos da la relacion de las variaciones

#2.aplicamos log en ambas variables 
base_regresion %>% 
  ggplot(aes(log(vab),log(empleo))) +
  geom_point()
#graficamente da una relacion de variaciones de empleo y vab mas clara


#3. Comparamos como dan los regresores

#Versión naive: en niveles
mod_simple_nivel <- lm(empleo ~ vab, data = base_regresion)
summary(mod_simple_nivel)

# Versión con log(pib_pc): elasticidad
mod_simple_log <- lm(log(empleo) ~ log(vab), data = base_regresion)
summary(mod_simple_log)

bptest(mod_simple_log)          #evidencia de heterocedasticidad
coeftest(mod_simple_log, vcov = vcovHC(mod_simple_log, type = "HC1"))
#sin embargo los errores casi no se mueven y siguen dando
#significativos

#verificamos resumen de ambos modelos (sobre todo BIC y AIC)
bind_rows(
  glance(mod_simple_nivel) |> mutate(modelo = "Niveles"),
  glance(mod_simple_log)   |> mutate(modelo = "logaritmica")
) |>
  select(modelo, r.squared, adj.r.squared, sigma, AIC, BIC)

#ambos coeficientes dan significativos, pero modelo con log explica aun mas
#R^2 ajustado 0,698 vs 0,751. Ademas AIC y BIC dan mas chicos. Y el 
#error estandar del log da 1.
#por ende elijo modelo con log.

#4.añadimos dummie RCA (que es binaria; 0 o 1)

#creo dummy para rca>1 y agrego a base
base_regresion <- base_regresion %>%
  mutate(dummy_rca = if_else(rca > 1, 1, 0))


modelo_con_dummy <- lm(log(empleo) ~ log(vab) + dummy_rca,
               data = base_regresion)
summary(modelo_con_dummy)

bptest(modelo_con_dummy)          #evidencia de heterocedasticidad
coeftest(modelo_con_dummy, vcov = vcovHC(modelo_con_dummy, type = "HC1"))
#sin embargo los errores casi no se mueven y siguen dando
#significativos

#resumen del modelo
bind_rows(
  glance(modelo_con_dummy)   |> mutate(modelo = "con dummy")
) |>
  select(modelo, r.squared, adj.r.squared, sigma, AIC, BIC)
#ajusta levemente mejor por R^2 y BIC y AIC, ademas que da significativo la dummy

#veo cuanto me da de multicolinealidad
vif(modelo_con_dummy)
#me da baja (1<vif<5 multicolinealidad baja)

#5. agrego interaccion entre dummy

modelo_completo <- lm(
  log(empleo) ~ log(vab) + dummy_rca + log(vab) * dummy_rca , 
 data = base_regresion)
summary(modelo_completo)
#el resumen demuestra que El efecto del VAB sobre el empleo es 
#aproximadamente 0,07 puntos mayor en los sectores con ventaja 
#comparativa que en los sectores sin ventaja comparativa.

bptest(modelo_completo)          #evidencia de heterocedasticidad
coeftest(modelo_completo, vcov = vcovHC(modelo_completo, type = "HC1"))

bind_rows(
  glance(modelo_con_dummy)   |> mutate(modelo = "con dummy"),
  glance(modelo_completo)   |> mutate(modelo = "completo")
) |>
  select(modelo, r.squared, adj.r.squared, sigma, AIC, BIC)

#claramente el modelo completo es el que mejor ajusta

#veo cuanto da de multicolinealidad
vif(modelo_completo)
#la colinealidad se infla fuertemente, lo que es logico al agregar la interaccion.
#no es preocupante porque el coefiente da muy significativo y ademas no se vuela
#el error estandar. Ademas, el modelo sin interaccion daba colinealidad baja
#implicando que por el unico factor que se infla es por la interaccion
#razon por la que no es preocupante esta inflacion.

#Visualización de coeficientes con gt
tabla_regresion <- tidy(modelo_completo) %>%
  select(
    term,
    estimate,
    std.error,
    statistic,
    p.value
  ) %>%
  mutate(
    term = dplyr::recode(     #para que no lea otro recode de otro paquete (ejemplo car)
      term,
      `(Intercept)` = "Intercepto",
      `log(vab)` = "log(VAB)",
      `dummy_rca` = "Dummy RCA > 1",
      `log(vab):dummy_rca` = "log(VAB) × Dummy RCA"
    )
  ) %>%
  rename(
    Término = term,
    Coeficiente = estimate,
    `Error estándar` = std.error,
    `Estadístico t` = statistic,
    `p-valor` = p.value
  ) %>%
  gt() %>%
  fmt_number(
    columns = c(
      Coeficiente,
      `Error estándar`,
      `Estadístico t`,
      `p-valor`
    ),
    decimals = 3
  )
gtsave(
  tabla_regresion,
  "04_output/tablas/tabla_regresion.png"
)

#6 Visualización de estadística con gt

tabla_estadisticas_reg <- glance(modelo_completo)%>% #seleccionamos los datos a visualizar
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

gtsave(tabla_estadisticas_reg,
       "04_output/tablas/tabla_estadisticas_reg.png")
#############################################################################
#A modo de benchmark se utilizará la base con servicios.
#Será con el fin de ver qué tanto cambia con TODOS los sectores de la economía
#############################################################################

#1 creamos y renombro base con servicios
base_con_ss <- base_con_ss %>% 
  rename(empleo = empleo_registrado)

#2 filtramos variables importantes
base_con_ss<- base_con_ss %>% 
  select(vab,empleo,rca)


#3 creamos dummy
base_con_ss <- base_con_ss %>%
  mutate(dummy_rca = if_else(rca > 1, 1, 0))

modelo_completo_con_ss <- lm(
  log(empleo) ~ log(vab) + dummy_rca + log(vab) * dummy_rca , 
  data = base_con_ss)
summary(modelo_completo_con_ss)

#4 comparamos vs modelo sin servicios
bind_rows(
  glance(modelo_completo_con_ss)   |> mutate(modelo = "con servicios"),
  glance(modelo_completo)   |> mutate(modelo = "sin servicios")
) |>
  select(modelo, r.squared, adj.r.squared, sigma, AIC, BIC)

#vemos cuanto me da de multicolinealidad
vif(modelo_completo_con_ss)

#buscamos evidencia de heterocedasticidad
bptest(modelo_completo_con_ss)       
#la hay, se usan errores robustos para ver cuanto se modifica el error
coeftest(modelo_completo_con_ss, vcov = vcovHC(modelo_completo_con_ss, type = "HC1"))
#no cambia significatividad, y apenas cambian errores

#5 visualizamos scatter con servicio
ggplot(
  base_con_ss,
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







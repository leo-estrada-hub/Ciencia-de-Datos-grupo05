# Relación Empleo-VAB: mediante ventajas comparativas

## Integrantes
 
- Sebastián Martín Ramos Bohórquez (910867) 
- Leonel Ivan Estrada (912785)

## Objetivo  
  
Buscar las elasticidades entre sectores con ventaja comparativa y su empleo directo. La hipótesis es comprobar que la relación es positiva y más fuerte que los sectores sin ventaja comparativa; caso contrario las elasticidades crecimiento del vab/crecimiento del empleo serán diferentes. Para esto, se utilizarán las bases del VAB provincial y empleo provincial, ambos con sectores desagregados, desde el año 2004 al 2024.

## Datos 
- **Fuente para datos del VAB provinciales:**
  [PBG Provincial — CEPAL] (https://www.cepal.org/es/publicaciones/47900-desagregacion-provincial-valor-agregado-bruto-la-argentina-base-2004)
- **Fuente para datos del empleo provincial:**
  [Datos trimestrales de empleo - Provinciales — OEDE] (https://www.argentina.gob.ar/trabajo/estadisticas/oede-estadisticas-provinciales)
- **Período:** 2004–2024
- **Unidad de análisis:** vab sectorial provincial anual y empleo sectorial provincial anual

## Análisis realizado 

1. Limpieza y reestructuración de las bases raw hacia un formato que unifique años y variables (formatos heterogéneos entre bases por diferentes desagregaciones y nombres).
2. Construcción de la variable RCA y posterior filtro de servicios para que queden los sectores de producción de bienes transables.
3. Filtro de sectores con ventaja comparativa para análisis descriptivo (HHI, diferencias de crecimiento, casos especiales) e inferencial (regresión, uso de t-test para comparar medias).
4. Visualizaciones del crecimiento en empleo de los sectores con ventaja comparativa en cada provincia; regresión que diferencia relación empleo-vab con y sin ventaja comparativa; y tablas que muestren lo encontrado en el análisis descriptivo y la regresión.


   
## Estructura del repositorio

```
TP-ciencia-de-datos/
├── 01_raw/                                      # Bases originales descargadas de la OCDE y CEPAL
├── 02_input/                                    # Bases procesadas y listas para el análisis
├── 03_script/
│   ├── 01_limpieza_y_procesamiento_de_datos.R   #Scripts que procesan las bases
│   ├── 02_metodos.R                             #Scripts con métodos rca y regresión
│   ├── 03_exploratorio.R                        #Scripts con análisis descriptivos
│   └── 04_graficos.R                            #Scripts con las visualizaciones (mapa y scatter plot)
├── 04_output/
│   ├── tablas/                                  # Tablas de resultados exportadas
│   └── graficos/                                # Visualizaciones generadas
├── 05_docs/                                     # Presentación
└── README.md
```
   
## Reproducción

### Paquetes necesarios

```r
install.packages(c("tidyverse", "readxl", "scales", "janitor", "broom", "lmtest", "sandwich", "car", "gt", "sf", "geoAr", "ggtext"))
```

### Orden de ejecución

#### 1. Limpieza
   1. `03_script/01_limpieza_y_procesamiento_de_datos.R/01_procesamiento_base_vab` — Lee la base en `raw/` del VAB provincial .
   2. `03_script/01_limpieza_y_procesamiento_de_datos.R/02_procesamiento_base_empleo` — Lee la base en `raw/` del empleo provincial (arrancar con paso 1.1. o 1.2. es indiferente).
   3. `03_script/01_limpieza_y_procesamiento_de_datos.R/03_procesamiento_tabla_final` — Lee las bases procesadas de empleo y VAB, unifica servicios y crea tabla unificada
#### 2. Métodos
   1. `03_script/02_metodos/01_metodo_rca.R` — Crea tabla con cálculo de RCA
   2. `03_script/01_limpieza_y_procesamiento_de_datos.R/04_filtro_no_transables.R` — Saca de la tabla todos los servicios (necesita la tabla del método rca)
   3. `03_script/02_metodos/03_metodo_regresion.R` — Análisis de regresión
#### 3. Exploratorio
   1. `03_script/03_exploratorio/analisis_descriptivo.R` — Crea tabla de análisis poblacional, de inferencia, y de máximo RCA
   2. `03_script/03_exploratorio/analisis_descriptivo_con_servicios.R` — Crea tabla de análisis poblacional, de inferencia, con servicios (benchmarck) 
#### 4.Gráficos
   1. `03_script/04_graficos/grafico_mapa.R` — Crea gráfico mapa
   2. `03_script/04_graficos/grafico_scatter_plot.R` — Crea gráfico scatter (indiferente el orden cuando se corre)

## Conclusiones principales
En el análisis presentado se enfatiza en el uso de la regresión, donde se vio que existe una relación positiva entre el crecimiento del VAB y del empleo más fuerte en los sectores con ventaja comparativa provinciales, que en los que no tienen ventaja comparativa, lo que es consistente con la hipótesis inicial. Sin embargo al ser considerado un efecto agregado, se profundizó en el análisis individual de estos sectores con ventaja comparativa. De este análisis se concluye que cada sector provincial sufrió de una evolución diferente, tanto en VAB como en empleo, lo que agrega variabilidad y quita explicatividad de los errores a la regresión. De esta manera, para conocer los detalles en profundidad de cómo afecta la ventaja comparativa al producto y al empleo en cada sector, se requiere hacer un análisis individualizado de estos.

 ```
























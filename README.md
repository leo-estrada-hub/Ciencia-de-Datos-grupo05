# Relación positiva entre crecimiento de sectores con ventaja comparativa y empleo directo

## Integrantes: 

-Sebastián Martín Ramos Bohórquez (910867) 
-Leonel Ivan Estrada (912785)

## Hipótesis:  
  
   i. El crecimiento del valor agregado bruto (VAB) de los sectores con ventaja comparativa revelada (RCA > 1) en las provincias tiene un efecto positivo y mayor sobre el empleo directo que el crecimiento del VAB de los sectores sin ventaja comparativa revelada (RCA ≤ 1).
   ii. El efecto del crecimiento del vab sobre empleo en sectores con rca no es siempre positivo ni mayor que los sectores sin rca.
-> Para lograr encontrar la intensidad de la relación usaremos datos desde el 2004 hasta el 2024 de todas las provincias argentinas. Y en pocas palabras, buscaremos si los sectores con ventaja comparativa en cada provincia tienen un efecto positivo mayor que los sectores sin ventaja comparativa

## Datos: 
- **Fuente para datos del VAB provinciales:**
  [PBG Provincial — CEPAL]((https://www.cepal.org/es/publicaciones/47900-desagregacion-provincial-valor-agregado-bruto-la-argentina-base-2004)
- **Fuente para datos del empleo provincial:**
  [Datos trimestrales de empleo - Provinciales — OEDE](https://www.argentina.gob.ar/trabajo/estadisticas/oede-estadisticas-provinciales)
- **Período:** 2004–2024
- **Unidad de análisis:** vab sectorial provincial anual y empleo sectorial provincial anual

## Análisis realizado: 

1. Proceso de limpieza:
  i. Ya que ambas bases contaban con sectores desagregados diferentes (en cuanto nombres y en cuanto contenido), se unificaron variables en ambas tablas. Este proceso inició con la base del VAB con 52 variables y la base de empleo con 56 variables, y terminó con ambas bases de 42 sectores.
  ii. Mediante el join se unificaron también la cantidad de años (2004-2024), y se realizó un filtro más para unificar los sectores de servicios.
  iii. Luego se creo otra base donde se filtran todos los servicios y esa utilizaremos (riesgo a que sectores con intervención estatal ensucien la estadística)

2. Con esta base filtrada creamos las variable RCA (RCA=share del VAB del sector (proporción del VAB del sector/VAB provincial)/share del sector a escala nacional (VAB nacional de ese sector/VAB total nacional))

3. Del análisis exploratorio al método: 
   i. Se filtró la base con sectores con RCA>1, y se vio que cada provincia tiene un promedio de 6 sectores con ventaja comparativa, y algunas provincias con más de 10 sectores, por ejemplo Buenos Aires con 19.
   ii. Dado el hecho i., se analizó qué tanto cambió el HHI entre 2004-2024 para ver si existió un cambio en las concentraciones (que pueda explicar, por ejemplo, si los sectores con ventaja comparativa crecieron más que el resto en su provincia)->t-test (heterocedastico) spoiler no lo hizo
   iii. Entonces se busca cómo varió el empleo en los sectores con RCA>1 entre 2004-2024 y se ve que en promedio aumenta, al igual que su varianza. Esto último puede explicarse porque entre estos años, los sectores con el mejor promedio de RCA>1, pudieron haber crecido fuerte en empleo y vab, y otros tomarón el camino inverso.
   iv. Para evaluar si existió este efecto inverso, se filtró el sector con más ventaja comparativa promedio de cada provincia. Un extremo es que en la industria del tabaco correntina fue fuerte en los primeros años de la muestra, pero desde el 2022 deja de subir datos de empleo y su producción se encuentra en tendencia bajista. Y el otro extremo es la industria de explotación de minas y canteras en Catamarca donde su empleo sube un 500%. Este hecho da a entender que existen outliers fuertes, y que para descubrir si existe la relación, objetivo del trabajo, hay que usar una regresión.
   v. Entonces para saber qué tan fuerte es esta relación, se crea una regresión con:
   -variable explicada logaritmo de empleo (para conocer las variaciones año a año)
   -variables explicativas el logartimo del VAB, una Dummie (vale 1 si tiene RCA>1, 0 con RCA=<1) que explique ventaja comparativa, y la interacción entre la Dummie y el logaritmo del VAB para reconocer si existe un efecto más fuerte en empleo si tiene ventaja comparativa que si no tuviera.

4. Visualizaciones
   i. Gráfico de mapa de Argentina donde muestra qué tanto creció el empleo en los sectores con ventaja comparativa de cada provincia.
   ii. Scatter plot: muestra la relación entre el logaritmo del empleo y del logaritmo del vab cuando tiene ventaja comparativa y cuando no.
   
## Estructura del repositorio

```
TP-ciencia-de-datos/
├── 01_raw/                                      # Bases originales descargadas de la OCDE y CEPAL
├── 02_input/                                    # Bases procesadas y listas para el análisis
├── 03_script/
│   ├── 01_limpieza_y_procesamiento_de_datos.R   #Scripts que procesan las base
│   ├── 02_metodos.R                             #Scripts con métodos rca y regresión
│   ├── 03_exploratorio.R                        #Script con análisis descriptivo
│   └── 04_graficos.R                            #Scripts con las visualizaciones (mapa y scatter plot)
├── 04_output/
│   ├── tablas/                                  # Tablas de resultados exportadas
│   └── graficos/                                # Visualizaciones generadas
├── 05_docs/
└── README.md
```
   
## Reproducción

### Paquetes necesarios

```r
install.packages(c("tidyverse", "readxl", "scales", "janitor", "broom", "lmtest", "sandwich", "car", "ggplot2", "gt", "sf", "geoAr", "ggtext"))
```

### Orden de ejecución

1. Limpieza
   1.`03_script/01_limpieza_y_procesamiento_de_datos.R/01_procesamiento_base_vab` — Lee la base en `raw/` del VAB provincial .
   2.`03_script/01_limpieza_y_procesamiento_de_datos.R/02_procesamiento_base_empleo` — Lee la base en `raw/` del empleo provincial (arrancar con paso 1.1. o 1.2. es indiferente).
   3.`03_script/01_limpieza_y_procesamiento_de_datos.R/03_procesamiento_tabla_final` — Lee las bases procesadas de empleo y VAB, unifica servicios y crea tabla unificada
2. Metodos
   1.`03_script/02_metodos/01_metodo_rca.R` — Crea tabla con cálculo de RCA
   2.`03_script/02_metodos/02_filtro_no_transables.R` — Saca de la tabla todos los servicios
   3.`03_script/02_metodos/03_metodo_regresion.R` — Analsis de regresión
3. Exploratorio
   1.`03_script/03_exploratorio/analisis_descriptivo.R` — Crea tabla con cálculo de RCA
4.Graficos
   1.`03_script/04_graficos/grafico_mapa.R` — Crea grafico mapa
   2.`03_script/04_graficos/grafico_scatter_plot.R` — Crea grafico scatter (indiferente el orden cuando se corre)

## Conclusiones principales


``` 
























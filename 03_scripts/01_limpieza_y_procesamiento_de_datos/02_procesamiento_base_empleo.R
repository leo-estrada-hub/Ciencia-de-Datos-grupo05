
# procesamiento de base de empleo 

library(tidyverse)   
library(readxl) 
library(janitor)   

options(scipen = 999)

#1 usamos canal raw y descargamos base de datos cruda
instub <- '01_raw'

ruta_excel <- file.path(instub,'provinciales_serie_empleo_trimestral_2dig_5.xlsx')

hojas <- excel_sheets(ruta_excel)

#1 sacamos hojas innecesarias del excel
hojas <- hojas[
  !(hojas %in%
      c("Carátula",
        "Indice",
        "Glosario",
        "Notas",
        "Descriptores de actividad"))
]

#2 creamos funcion para procesar base
procesar_provincia <- function(sheet){

  datos <- read_xlsx(
    ruta_excel,
    sheet = sheet,
    skip = 3
  )
  
  datos <-
    datos %>%
    clean_names()
  
  ## primera columna = código
  ## segunda columna = rama
  
  names(datos)[1] <- "codigo"
  names(datos)[2] <- "sector_original"
  
  datos <- datos %>%
    mutate(
      across(
        -c(codigo, sector_original),
        ~ readr::parse_number(as.character(.))
      )
    )
  
  ## eliminar filas completamente vacías
  
  datos <-
    datos %>%
    filter(!is.na(sector_original) ,
  sector_original != "Serie anterior"
  )
  
  ## pasar a formato largo
  
  datos <-
    datos %>%
    pivot_longer(
      cols = -c(codigo,sector_original),
      names_to = "periodo",
      values_to = "empleo"
    )
  
  ## extraer trimestre y año
  
  datos <-
    datos %>%
    mutate(
      anio =
        stringr::str_extract(periodo,"[0-9]{4}") %>%
        as.integer(),
      trimestre =
        stringr::str_extract(periodo,"^[1-4]")
    )
  
  ## promedio anual
  
  datos <-
    datos %>%
    group_by(
      sector_original,
      anio
    ) %>%
    summarise(
      empleo_registrado =
        mean(
          as.numeric(empleo),
          na.rm = TRUE
        ),
      .groups="drop"
    )
  datos$provincia <- sheet
  datos
}

#3 Procesar todas las provincias

empleo_total <-
  map_dfr(
    hojas,
    procesar_provincia
  )

#4 Unificación de Buenos Aires

empleo_total <-
  empleo_total %>%
  mutate(
    provincia =
      case_when(
        provincia=="Capital Federal" ~
          "CABA",
        provincia=="Partidos de GBA" ~
          "Buenos Aires",
        provincia=="Resto de Buenos Aires" ~
          "Buenos Aires",
        TRUE ~ provincia
      )
  )

# Suma GBA + Resto Buenos Aires

empleo_total <-
  empleo_total %>%
  group_by(
    provincia,
    sector_original,
    anio
  ) %>%
  summarise(
    empleo_registrado =
      sum(
        empleo_registrado,
        na.rm=TRUE
      ),
    .groups="drop"
  )

#5 renombramos provincias para unificar con base VAB
empleo_total <-
  empleo_total %>%
  mutate(
    provincia =
      dplyr::recode(
        provincia,
        "Cordoba"="Córdoba",
        "Entre Rios"="Entre Ríos",
        "Neuquen"="Neuquén",
        "Rio Negro"="Río Negro",
        "Santa Fe"="Santa Fe",
        "Santiago del Estero"="Santiago del Estero",
        "Tierra del Fuego"="Tierra del Fuego",
        "Tucuman"="Tucumán"
      )
    )

#6 cuantas variables hay antes de agruparlas?
nrow(empleo_total)     #51180  filas
ncol(empleo_total)     #4  columnas


#7 unificamos con base vab
empleo_total <-
  empleo_total %>%
  filter(
    !sector_original %in% c(
      "Extraccion de petroleo crudo y gas natural",
      "Extraccion de minerales metaliferos",
      "Explotacion de otras minas y canteras",
      "CONSTRUCCION",
      "AGRICULTURA, GANADERIA, CAZA Y SILVICULTURA",
      "PESCA Y SERVICIOS CONEXOS",
      "INDUSTRIA MANUFACTURERA",
      "Electricidad, gas y agua",
      "Captación, depuración y distribución de agua",
      "COMERCIO AL POR MAYOR Y AL POR MENOR",
      "HOTELERIA Y RESTAURANTES ",
      "SERVICIOS DE TRANSPORTE, DE ALMACENAMIENTO Y DE COMUNICACIONES",
      "INTERMEDIACION FINANCIERA Y OTROS SERVICIOS FINANCIEROS ",
      "SERVICIOS SOCIALES Y DE SALUD",
      "ENSEÑANZA",
      "Servicios de organizaciones empresariales",
      "Agencias de empleo eventual",
      "Servicios jurídicos, contables y otros servicios a empresas",
      "Investigación y desarrollo",
      "Actividades de informática",
      "Alquiler de equipo de transporte y de maquinaria",
      "SERVICIOS COMUNITARIOS, SOCIALES Y PERSONALES N.C.P.",
      "SERVICIOS INMOBILIARIOS, EMPRESARIALES Y DE ALQUILER",
      "TOTAL",
      "serie anterior"
    )
  )

empleo_total <- empleo_total %>%
  mutate(
    sector_agregado = case_when(
      sector_original == "Agricultura y ganaderia" ~ "Agricultura, ganaderia, caza y servicios conexos",
      sector_original == "Silvicultura, extracción de madera" ~ "Silvicultura, extracción de madera y servicios conexos",
      sector_original ==  "Pesca y actividades relacionadas con la pesca" ~ "Pesca" ,
 sector_original ==  "Alimentos" ~ "Elaboración de productos alimenticios y bebidas" ,
 sector_original == "Tabaco"  ~ "Elaboración de productos de tabaco",
 sector_original ==  "Productos textiles" ~ "Fabricación de productos textiles",
 sector_original == "Confecciones" ~ "Fabricación de prendas de vestir; terminación y teñido de pieles",
 sector_original %in% c("Calzado y cuero","Calzado y productos de cuero","Cuero","Cuero y calzado","Calzado") ~ "Curtido y terminación de cueros; fabricación de artículos de marroquinería, talabartería y calzado y de sus partes",
 sector_original == "Madera" ~ "Producción de madera y fabricación de productos de madera y corcho, excepto muebles; fabricación de artículos de paja y de materiales trenzables",
 sector_original == "Papel" ~ "Fabricación de papel y de  productos de papel",
 sector_original %in% c("Edición", "Edición e impresión") ~ "Edición e impresión; reproducción de grabaciones",
 sector_original == "Productos de petróleo" ~ "Fabricación de coque, productos de la refinación del petróleo y combustible nuclear",
 sector_original == "Productos químicos" ~ "Fabricación de sustancias y productos químicos",
 sector_original == "Productos de caucho y plástico" ~ "Fabricación de productos de caucho y plástico",
 sector_original == "Otros minerales no metálicos" ~ "Fabricación de productos minerales no metálicos",
 sector_original == "Metales comunes" ~ "Fabricación de metales comunes",
 sector_original == "Otros productos de metal" ~ "Fabricación de productos elaborados de metal, excepto maquinaria y equipo",
 sector_original == "Maquinaria y equipo" ~ "Fabricación de maquinaria y equipo n.c.p.",
 sector_original == "Maquinaria de oficina" ~ "Fabricación de maquinaria de oficina, contabilidad e informática",
 sector_original == "Aparatos eléctricos" ~ "Fabricación de maquinaria y aparatos eléctricos  n.c.p.",
 sector_original == "Radio y televisión" ~ "Fabricación de equipos y aparatos de radio, televisión y comunicaciones",
 sector_original == "Instrumentos médicos" ~ "Fabricación de instrumentos médicos, ópticos y de precisión; fabricación de relojes",
 sector_original == "Automotores" ~ "Fabricación de vehículos automotores, remolques y semirremolques",
 sector_original == "Otros equipo de transporte" ~ "Fabricación de equipo de transporte n.c.p.",
 sector_original == "Muebles" ~ "Fabricación de muebles y colchones; industrias manufactureras n.c.p.",
 sector_original == "Reciclamiento de desperdicios y desechos" ~ "Reciclamiento",
 sector_original == "ELECTRICIDAD, GAS Y AGUA" ~ "Electricidad, gas y agua",
 sector_original == "Construccion" ~ "Construcción",
 sector_original %in% c("Vta y reparación de vehículos. vta por menor de combustible","Comercio al por mayor","Comercio al por menor") ~ "Comercio mayorista, minorista y reparaciones",
 sector_original == "Servicios de hoteleria y restaurantes" ~ "Servicios de hoteleria y restaurantes",
 sector_original %in% c("Transporte ferroviario y automotor y por tuberias","Transporte marítimo y fluvial","Transporte aéreo de cargas y de pasajeros","Manipulación de carga, almacenamiento y depósito") 
 ~ "Transporte",
 sector_original == "Telecomunicaciones y correos" ~ "Comunicaciones",
 sector_original %in% c( "Intermediacion financiera y otros servicios financieros","INTERMEDIACION FINANCIERA Y OTROS SERVICIOS FINANCIEROS") ~ "Intermediación financiera y otros servicios financieros",
 sector_original == "Seguros" ~ "Servicios de seguros",
 sector_original == "Servicios auxiliares a la actividad financiera" ~ "Servicios auxiliares a la actividad financiera",
 sector_original == "Servicios inmobiliarios" ~ "Servicios inmobiliarios",
 sector_original == "Eliminación de desperdicios" ~ "Eliminación de desperdicios y aguas residuales, saneamiento y servicios similares",
 sector_original == "Servicios culturales, deportivos y de esparcimiento" ~ "Servicios culturales y deportivos. Otras actividades",
 sector_original == "Servicios n.c.p." ~ "Servicios n.c.p.",
 sector_original == "Enseñanza" ~ "Enseñanza",
 sector_original == "Servicios sociales y de salud" ~ "Servicios sociales y de salud",
 sector_original =="HOTELERIA Y RESTAURANTES" ~ "Servicios de hoteleria y restaurantes",
 TRUE ~ sector_original 
    ))

#8 acomodamos bases para posterior orden
empleo_total <-
  empleo_total %>%
  group_by(
    provincia,
    sector_agregado,
    anio
  ) %>%
  summarise(
    empleo_registrado =
      sum(
        empleo_registrado,
        na.rm=TRUE
      ),
    .groups="drop"
  )

empleo_total <-
  empleo_total %>%
  filter(anio >= 2004, anio <= 2024) %>%
  arrange(
    provincia,
    sector_agregado,
    anio
  )

#9 cuantas variables hay despues de agruparlas?
nrow(empleo_total)           #21168  filas
ncol(empleo_total)           #4  columnas
sum(is.na(empleo_total))     #sin NAs
sapply(empleo_total, class)  #provincia y sector_agregado character/ anio integer y empleo_registrado numeric
glimpse(empleo_total)        #que tiene aprox la tabla

saveRDS(empleo_total, "02_input/empleo_sector.rds")



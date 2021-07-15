# Librerías ---------------------------------------------------------------

library(shiny)
library(sf)
library(tidyverse)
library(lubridate)
library(leaflet)
library(leaflet.extras)
library(shinyWidgets)
library(shinythemes)

# Carga de Datos ----------------------------------------------------------

options(OutDec= ",", big.mark = T)

if (!exists("MAPA")) {
  # Cargar datos
  load("data/_data.RData")

  ref <- read.csv("data/ref.csv")
#
#   names(EPH) <- c("I", "H_A", "H_Q")
#   EPH$I <- EPH$I %>%
#     mutate(across(.cols = starts_with("T_"),
#                   .fns = ~.x/100 ))
#
  SE <- data.frame(INI = seq(as.Date("2020-01-05"),
                             as.Date(today()+7), by = 7)) %>%
    mutate(FIN = INI + 6,
           LAB = paste0(format(INI, "%d %b %Y"), " - ", format(FIN, "%d %b %Y")),
           w = 1:n(),
           y = year(INI)) %>%
    group_by(y) %>%
    mutate(se = 1:n(),
           se_l = str_length(se),
           se_c = case_when(se_l == 1 ~ paste0("0", se),
                            se_l == 2 ~ paste0(se)),
           se_y = paste0(y, "_", se_c),
           se_l = NULL,
           se_c = NULL) %>%
    ungroup()

  if(file.exists("_draft/_COVIDdb.RData")){
    load("_draft/_COVIDdb.RData")
  }

  DEPTO <- MAPA$DEPTO
  MAPA$DEPTO <- NULL

  INFRAEST <- INFRA
  rm(INFRA)
}

# Armar variables para selectores -------------------------------------


AGLO <- c("Seleccionar (Todos)", unique(MAPA$AGLOMERADO$aglo))

poligono <- tibble::tribble(
  ~ base,      ~ color,      ~ fill,     ~opacity,
  "MUNI",      "black",      F,          1,
  "RENABAP",   "blue",       T,          0,
  "COUNTRY",   "green",      T,          0)

var_polig <-  poligono$base
names(var_polig) <- c('Municipio', 'Barrio Popular', 'Country')

# Escala para Pirámide
l <- c(paste(seq(0, 90, by = 5),
             seq(5, 95, by = 5), sep = "-"),
       "+95")

# Selectores SER
infraestructura <- c('Salud General' = "General",
                     'Salud Especialidades' = "Especialidad",
                     'Escuelas Estatales/Gestión Social' = "E_PUB",
                     'Escuelas Privadas' = "E_PRI",
                     'Universidades' = "universidades",
                     'Venta de Alimentos' = "mercado",
                     'Sevicios Financieros' = "financiero",
                     'Seccional Policial' = "seguridad")

# Selectores POB
indic_POB <- c('Edad Promedio' = "EDAD",
               'Tasa de Juventud' = "t_JUV",
               'Tasa de Envejecimiento' = "t_ENV",
               '% de Personas Sin Estudios Secundarios Completos' = "SEC_p",
               '% de Personas Con Estudios Universitarios Completos' = "SUP_p")

# Selectores de SAL
indic_SAL <- c('% de Personas Con Cobertura de Salud sólo sistema estatal' = "SALUDno")


# Selectores MIG
paises <- DEMOG %>%
  select('América Otros':'Asia y Oceanía') %>% names()
paises <- list(Sudamericanos = paises[c(2:4, 6:7, 9, 1)],
               Otros = paises[c(5, 10, 8)])

# Selectores HAB-DEF
indic_HAB <- c('Déficit de Vivienda por hogares' = "DEFICIT",
               'Promedio de Hacinamiento (Personas por cuarto)' = "H_HAC",
               '% de Hogares con NBI' = "H_NBI",
               '% de Hogares sin agua potable' = "H_AGUA",
               '% de Hogares sin desague cloacal/pozo' = "H_CLOACA",
               '% de Hogares en Vivienda sin recolección de basura' = "BASURA",
               '% de Hogares en Vivienda sin acceso a transporte público (300m)' = "TRANSPORTE",
               '% de Hogares en Vivienda sin pavimento' = "PAVIMENTO",
               '% de Hogares en Vivienda sin alumbrado público' = "ALUMBRADO")

deficit <- DEMOG %>%
  select('Sin deficit':'Vivienda con Núcleo Allegado Interno + Mejora') %>% names()

# VARS <-  c("Personas por vivienda" = "VIV")

# Selectores EPH

indic_EPH <- c('Ingreso de los Hogares' = "ING",
               '% de Personas sin cobertura de salud privada' = "SAL",
               'Indicadores de Ocupación' = "OCU")


# Bases auxiliares --------------------------------------------------------

# Base auxiliar con límites de radios para selección

R <- MAPA$RADIO %>%
  select(ID) %>%
  cbind(sapply(.$geometry, st_bbox) %>% t()) %>%
  st_drop_geometry()

# Carto con centroides de radios
CEN <- MAPA$RADIO %>%
  st_point_on_surface() %>%
  left_join(DEMOG %>% select(c("ID", all_of(unname(unlist(paises))))), by = "ID" )

# Datos demográficos resumen
resumen <- DEMOG %>%
  group_by(Nivel = aglo) %>%
  summarise(Población = sum(PERSONAS, na.rm = T),
            Hogares = sum(HOGARES, na.rm = T),
            Viviendas = sum(VIVIENDAS, na.rm = T),
            Radios = n())

resumen <- rbind(resumen,
                c("Total Aglomerados", apply(resumen[-1], 2, sum ))) %>%
  as.data.frame()
rownames(resumen) <- resumen$Nivel

# Íconos ------------------------------------------------------------------

size <- 25

# Mapa
icon_lista <- iconList(
  Hospital = makeIcon("icon/SALUD_hospital.png",
                      iconWidth = size, iconHeight = size),
  'Centro de Salud' = makeIcon("icon/SALUD_salita.png",
                               iconWidth = size-10, iconHeight = size-10),
  'UPA/UDP' = makeIcon("icon/SALUD_salita.png",
                       iconWidth = size-5, iconHeight = size-5),
  E_PUB = makeIcon("icon/EDU_esPUB.png",
                   iconWidth = size, iconHeight = size),
  E_GS = makeIcon("icon/EDU_esGS.png",
                   iconWidth = size, iconHeight = size),
  E_PRI = makeIcon("icon/EDU_esPRI.png",
                   iconWidth = size, iconHeight = size),
  Estatal = makeIcon("icon/EDU_uniPUB.png",
                     iconWidth = size, iconHeight = size),
  Privado = makeIcon("icon/EDU_uniPRI.png",
                     iconWidth = size, iconHeight = size),
  Mercado = makeIcon("icon/GRAL_market.png",
                     iconWidth = size, iconHeight = size),
  Banco = makeIcon("icon/GRAL_bank.png",
                   iconWidth = size, iconHeight = size),
  Cajero = makeIcon("icon/GRAL_ATM.png",
                    iconWidth = size-10, iconHeight = size-10),
  Policia = makeIcon("icon/GRAL_policia.png",
                     iconWidth = size-5, iconHeight = size-5)
  )

# Iconos para Seleccion
# icon_sel  <- data.frame(
#   img = c(sprintf("<img src='icon/SALUD_hospital.png' width=30px><div class='jhr'>%s</div></img>", "General"),
#           sprintf("<img src='icon/SALUD_hospital.png' width=30px><div class='jhr'>%s</div></img>", "Especialidad"),
#           sprintf("<img src='icon/EDU_esPUB.png' width=30px><div class='jhr'>%s</div></img>", "E_PUB"),
#           sprintf("<img src='icon/EDU_esPRI.png' width=30px><div class='jhr'>%s</div></img>", "E_PRI"),
#           sprintf("<img src='icon/EDU_uniPUB.png' width=30px><div class='jhr'>%s</div></img>", "universidades"),
#           sprintf("<img src='icon/GRAL_market.png' width=30px><div class='jhr'>%s</div></img>", "mercado"),
#           sprintf("<img src='icon/GRAL_bank.png' width=30px><div class='jhr'>%s</div></img>", "financiero"),
#           sprintf("<img src='icon/GRAL_policia.png' width=30px><div class='jhr'>%s</div></img>", "seguridad")
#   )
# )

# Funciones ---------------------------------------------------------------
# Pasar a R/
# map(paste0("R/", list.files("R/")), source)

map_polig <- function(base = c("MUNI", "COUNTRY", "RENABAP"),
                     grupo = base,
                     color = "black", opacity = 0, fill = T, weight = 1,
                     highlightOptions = NULL,
                     seleccion = input$zonas, POLIG_BASE){

  if(base %in% seleccion){
    leafletProxy("map", data = POLIG_BASE[[base]]) %>%
      clearGroup(grupo) %>%
      addPolygons(group = grupo,
                  popup = ~name,
                  color = color,
                  fill = fill,
                  highlightOptions = highlightOptions,
                  weight = weight,
                  smoothFactor = 0.5,
                  noClip = T,
                  opacity = opacity) %>%
      showGroup(grupo)
  }else{
    leafletProxy("map",data = POLIG_BASE[[base]]) %>%
      hideGroup(grupo)
  }
}

map_serv <- function(lista, base, seleccion,
                     servicio = base, grupo = base,
                     # show.calor = T,
                     cluster = F
                     ){

  if(servicio %in% seleccion){
    leafletProxy("map",
                 data = lista[[base]]) %>%
      clearGroup(grupo) %>%
        addMarkers(group = grupo,
                   popup = ~cuadro,
                   icon = ~icon_lista[icono],
                   clusterOptions = cluster
        ) %>%
      showGroup(group = grupo)

  }else{
    leafletProxy("map", data = lista[[base]]) %>%
      hideGroup(grupo)
  }
}

map_heat <- function(lista, base,
                     servicio = base,
                     grupo = "heat",
                     seleccion = seleccion,
                     calor = F,
                     radio = 8, blur = 16){

  if(calor){
    leafletProxy("map",
                 data = lista[[base]]) %>%
      clearGroup(grupo) %>%
      addHeatmap(group = grupo,
                 radius = radio, blur = blur) %>%
      showGroup(group = grupo)

  }else{
    leafletProxy("map",data = lista[[base]]) %>%
      hideGroup(grupo)
  }

}

addRadio <- function(map, data, grupo,
                     indicador,
                     # col_DATA = T, #Agregar elegir color en base a datos o en base a variable absoluta
                     PAL = "YlOrRd" ){

  data$indic <- data[[indicador]]

  # Exigir que la variable de análisis tenga al menos dos valores diferentes (para sacar cuantiles)
  if(length(unique(data$indic) ) > 1){
    l <- length( unique( quantile(data$indic, na.rm = T, probs = seq(0, 1, length.out = 9) )  ) )
    if(l >= 9){
      data$l <- colorQuantile(PAL, data$indic, n = 9)(data$indic)
    }else{
      data$l <- colorNumeric(PAL, range(data$indic))(data$indic)
    }

    map %>%
      addPolygons(data = data, group = grupo, smoothFactor = 0.5,
                  popup = ~cuadro,
                  opacity = 1.0, color = "#BDBDBD", weight = 0.3,
                  # fillOpacity = ~VAR_FILL,
                  fillColor = ~l,
                  highlightOptions = highlightOptions(color = "black", weight = 1,
                                                      bringToFront = TRUE)) %>%
      showGroup(group = grupo)

  }else{
    # Genera warning porque no quiere generar grupos con menos de dos objetos espaciales
    map %>%
      addPolygons(data = rbind(data, data), group = grupo, smoothFactor = 0.5,
                  popup = ~cuadro,
                  opacity = 1.0, color = "#BDBDBD", weight = 0.3,
                  # fillOpacity = ~VAR_FILL,
                  fillColor = "#A9A9A9",
                  highlightOptions = highlightOptions(color = "black", weight = 1,
                                                      bringToFront = TRUE)) #%>%
      # hideGroup(grupo)
  }
}

# Librerías ---------------------------------------------------------------

library(shiny)
library(sf)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(shinyWidgets)

# Carga de Datos ----------------------------------------------------------

if (!exists("MAPA")) {
  options(OutDec= ",", big.mark = T)
  # Cargar datos
  load("data/_data.RData")
  ref <- read.csv("data/ref.csv")
  
  INFRAEST <- INFRA
  rm(INFRA)
}

# Armar variables para selectores -------------------------------------

MUNI <- MAPA$MUNI %>%
  st_drop_geometry() %>%
  select(muni = name, aglo)

VARS <-  c("Personas por vivienda" = "VIV")

AGLO <- c("Seleccionar (Todos)", MAPA$AGLO$aglo)

l <- c(paste(seq(0, 90, by = 5),
             seq(5, 95, by = 5), sep = "-"),
       "+95")

paises <- DEMOG %>%
  select('América Otros':'Asia y Oceanía') %>% names()
paises <- list(Sudamericanos = paises[c(2:4, 6:7, 9, 1)],
               Otros = paises[c(5, 10, 8)])

deficit <- DEMOG %>%
  select('Sin deficit':'Vivienda con Núcleo Allegado Interno + Mejora') %>% names()

indic_POB <- c('Edad Promedio' = "EDAD",
               'Tasa de Juventud' = "t_JUV",
               'Tasa de Envejecimiento' = "t_ENV",
               '% de personas Sin Estudios Secundarios Completos' = "SEC_p",
               '% de personas Con Estudios Universitarios Completos' = "SUP_p"
               )

indic_HAB <- c('Déficit de Vivienda por hogares' = "DEFICIT",
               'Promedio de Hacinamiento (Personas por cuarto)' = "H_HAC",
               '% de Hogares con NBI' = "H_NBI",
               '% de Hogares sin agua potable' = "H_AGUA",
               '% de Hogares sin desague cloacal/pozo' = "H_CLOACA",
               '% de Hogares en Vivienda sin recolección de basura' = "BASURA",
               '% de Hogares en Vivienda sin acceso a transporte público (300m)' = "TRANSPORTE",
               '% de Hogares en Vivienda sin pavimento' = "PAVIMENTO",
               '% de Hogares en Vivienda sin alumbrado público' = "ALUMBRADO" )

poligono <-  c('Municipio' = "MUNI",
               'Barrio Popular' = "RENABAP",
               'Country' = "COUNTRY")

infraestructura <- c('Salud General' = "General",
                     'Salud Especialidades' = "Especialidad",
                     'Escuelas Estatales/Gestión Social' = "E_PUB",
                     'Escuelas Privadas' = "E_PRI",
                     'Universidades' = "universidades",
                     'Venta de Alimentos' = "mercado",
                     'Sevicios Financieros' = "financiero",
                     'Seccional Policial' = "seguridad")

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

# Seleccion
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


map_base <- function(base = c("MUNI", "COUNTRY", "RENABAP"),
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

sel_serv <- function(lista, base, seleccion,
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
    leafletProxy("map",data = lista[[base]]) %>%
      hideGroup(grupo)
  }
}

heat_map <- function(lista, base,
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
                     indicador, PAL = "YlOrRd"
                     ){

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
    map %>%
      addPolygons(data = data, group = grupo, smoothFactor = 0.5,
                  popup = ~cuadro,
                  opacity = 1.0, color = "#BDBDBD", weight = 0.3,
                  # fillOpacity = ~VAR_FILL,
                  fillColor = "#A9A9A9",
                  highlightOptions = highlightOptions(color = "black", weight = 1,
                                                      bringToFront = TRUE)) %>%
      hideGroup(grupo)
  }

}

# Librerías ---------------------------------------------------------------

library(shiny)
library(sf)
library(tidyverse)
library(lubridate)
library(leaflet)
library(leaflet.extras)
library(shinyWidgets)
library(plotly)

# Carga de Datos ----------------------------------------------------------

# options(OutDec= ",", big.mark = T)
plotly::config(plot_ly(), displayModeBar = FALSE) 


if (!exists("MAPA")) {
  # Cargar datos
  load("data/_data.RData")
  load("data/COVID.RData")
  ref <- read.csv("data/ref.csv")

  DEPTO <- MAPA$DEPTO
  MAPA$DEPTO <- NULL

  INFRAEST <- INFRA
  rm(INFRA)

  COVID <- COVID %>%
    mutate(cuadro = paste0("<strong>Totales cada 100mil habitantes</strong>",
                           "<br>Período: ", format(SE$INI_p[1], format = "%d/%m/%Y"), 
                           " - ", format(SE$FIN_p[nrow(SE)],  format = "%d/%m/%Y"),
                           "",
                           "<br><br>- Positivos ", round(INI, 1),
                           "<br>- Internados ", round(INT, 1),
                           "<br>- En ciudados intensivos ", round(CUI, 1),
                           "<br>- Fallecidos ", round(FAL, 1)))
  
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
indic_SAL <- c('% de Personas Con Cobertura de Salud sólo sistema estatal' = "SALUDno",
               'Casos Positivos por COVID (c/100.000 hab.)' = "INI",
               'Internados por COVID (c/100.000 hab.)' = "INT",
               'Casos en cuidados intensivos por COVID (c/100.000 hab.)' = "CUI",
               'Fallecidos por COVID (c/100.000 hab.)' = "FAL"
               )


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

plot_SE <- SE %>%
  pivot_longer(cols = INI:FAL, names_to = "VAR", values_to = "n") %>%
  filter(!is.na(n) ) %>%
  left_join(data.frame(VAR = c("INI", "INT", "CUI", "FAL"), 
                       LAB_VAR = c("Casos Positivos", "Internados", 
                                   "Cuidados intensivos", "Fallecidos")), by = "VAR") %>%
  mutate(label = paste(se_SEL, "<br>", LAB_VAR, n ) ) %>%
  ggplot(aes(x = INI_p, y = n, group = VAR, color = VAR, text = sprintf(label))) +
  geom_point(alpha = 0.3) +
  geom_line(alpha = 0.7) +
  scale_x_date(date_breaks = "2 month", date_labels = "%b/%y") +
  facet_wrap("LAB_VAR", scales = "free_y", ) +
  labs(x = "", y = "") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90),
        legend.position = "none")

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
                     map = "map",
                     color = "black", opacity = 0, fill = T, weight = 1,
                     highlightOptions = NULL,
                     seleccion = input$zonas, POLIG_BASE){

  if(base %in% seleccion){
    leafletProxy(map = map, data = POLIG_BASE[[base]]) %>%
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
    leafletProxy(map,data = POLIG_BASE[[base]]) %>%
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

paleta <- function(data = data, indic = "indic",
                   rango = as.null(), PAL = "YlOrRd"){
  
  if(length(unique(data[[indic]]) ) > 1 | !is.null(rango)){
    if(is.null(rango) ){
      l <- length( unique( quantile(data[[indic]], na.rm = T, 
                                    probs = seq(0, 1, length.out = 9) )  ) )
      if(l >= 9){
        # 9 percentiles
        PAL_f <- colorQuantile(PAL, data[[indic]], n = 9)
      }else{
        # Menos de 9 percentiles -> Escala continua con rango
        PAL_f <- colorNumeric(PAL, range(data[[indic]]))
      }
    }else{
      # Rango
      PAL_f <- colorNumeric(PAL, rango)
    }
  }else{
    # Sólo un valor diferente -> Valor Constante (gris)
    PAL_f <- colorNumeric(palette = "#A9A9A9", domain = data[[indic]])
  }
  return(PAL_f)
}

addRadioPolyg <- function(map, data = getMapData(map), 
                          grupo, PAL_f){
  
  map %>%
    addPolygons(data = data, 
                group = grupo, smoothFactor = 0.5,
                popup = ~cuadro,
                opacity = 1.0, color = "#BDBDBD", weight = 0.3,
                fillColor = ~PAL_f(indic),
                highlightOptions = highlightOptions(color = "black", weight = 1,
                                                    bringToFront = TRUE))  %>%
    showGroup(group = grupo)
}

addRadioLeyenda <- function(map, 
                            data = getMapData(map), 
                            grupo, 
                            rango = as.null(),
                            transf = function(x) {x},
                            PAL_f){
  
  if(is.null(rango)){
    if(attributes(PAL_f)$colorType == "quantile"){
      qpal_colors <- unique(PAL_f(sort(data[["indic"]]))) # hex codes
      qpal_labs <- quantile(data[["indic"]],  na.rm = T,
                            seq(0, 1, length.out = length(qpal_colors) + 1)) # depends on n from pal
      qpal_labs <- round(qpal_labs)
      qpal_labs <- paste(lag(qpal_labs), qpal_labs, sep = " - ")[-1] # first lag is NA
      
      map %>%
        addLegend(title = "", position = "topleft",
                  colors = qpal_colors, 
                  labels = qpal_labs,
                  labFormat = labelFormat(transf = transf),
                  group = grupo)
    }else{
      map %>%
        addLegend(title = "", position = "topleft",
                  pal = PAL_f,
                  values = ~indic,
                  labFormat = labelFormat(transf = transf),
                  group = grupo)
    }
    
  }else{
    map %>%
      addLegend(title = "", position = "topleft",
                pal = PAL_f,
                values = seq(from = rango[1], to = rango[2], length.out = 10),
                labFormat = labelFormat(transf = transf),
                group = grupo)
  }
}

addRadio <- function(map = "map", data, indicador, 
                      grupo,
                      rango = as.null(), transf = function(x) {x},
                      leyenda = T,
                      PAL = "YlOrRd" ){

  # if(is.null(db[["cuadro"]]))
  
  data$indic <- data[[indicador]]
  PAL_f <- paleta(data = data, indic = "indic",
                  rango = rango, PAL = PAL)
  
  mapa <- leafletProxy(map, data = data) %>%
      clearGroup(grupo) %>%
      clearControls() %>%
      addRadioPolyg(grupo = grupo, PAL_f = PAL_f)


  if(leyenda){
    mapa <- mapa %>%
      addRadioLeyenda(grupo = grupo, PAL_f = PAL_f,
                      transf = transf, rango = rango)
  }

  return(mapa)
  
}

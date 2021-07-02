#' @export

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

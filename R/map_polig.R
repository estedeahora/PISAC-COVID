#' @export

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
#' @export

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
#' @export

sel_serv <- function(lista, base, seleccion,
                     servicio = base, grupo = base,
                     # show.calor = T,
                     cluster = F){
  
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


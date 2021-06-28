server <- function(input, output, session) {

# Reactivos --------------------------------------------------------------
  
### Paneles UI ----------------------------------------------------------
  
  # Panel de selección jerárquico para municipios
  # input$aglo
  
  panel_muni <- reactive({
    if(input$aglo != "Seleccionar (Todos)"){
      MUNI %>%
        filter(aglo == input$aglo)
    }
  })
  
  observeEvent(panel_muni(), {
    choices <- unique(panel_muni()$muni)
    updateSelectInput(inputId = "muni", selected = "Seleccionar (Todos)", 
                      choices = c("Seleccionar (Todos)", choices) )
  })
  
  # Servicios (SER) -----------------------------------------------------
  
  # Cluster Sí/no
  cluster <- reactive({
    if(input$clu){
      markerClusterOptions(maxClusterRadius = 35)
    }else{
      NULL
    }
    
  })
  
  # Panel de selección de heatmap

  observeEvent(input$act_heat, {
    updateTabsetPanel(inputId = "p_HEAT",
                      selected = if(input$act_heat) {"p_HEATSI"} else {"p_HEATNO"})
  })
  
  # Demográfico (DEM) ----------------------------------------------------
  
  # Reactivo para pirámide
  
  PIRAMIDE_r <- reactive({
    DEMOG_r() %>%
      select(ID, eph_aglome, aglo, PERSONAS,
             'V_0-5':'M_+95') %>%
      summarise(across(.cols = 'V_0-5':'M_+95', .fns = ~sum(.x)) ) %>%
      pivot_longer(cols = everything() ) %>%
      mutate(sexo = str_sub(name,  end = 1),
             EDADQUI = str_sub(name,  start = 3),
             EDADQUI = factor(EDADQUI, levels = l)) %>%
      arrange(EDADQUI) %>%
      pivot_wider(id_cols = EDADQUI, names_from = sexo,
                  values_from = value, values_fill = 0, names_sort = F) 
    
  })
  
  # Migración (MIG) ------------------------------------------------
  
  MIGRA_r <- reactive({
    DEMOG_r() %>%
      select(ID, eph_aglome, aglo, PERSONAS, MIGRANTE,
             'América Otros':'Asia y Oceanía') %>%
      summarise(across(.cols = 'América Otros':'Asia y Oceanía', 
                       .fns = ~sum(.x, na.rm = T)) ) %>%
      pivot_longer(cols = everything() ) %>%
      arrange(value)  %>%
      filter(name %in% input$MIG_sel)
  })
  
  # Hábitat (HAB) -------------------------------------------------
  # Panel de selección de Hábitat (Déficit vs resto)
  
  observeEvent(input$HAB_sel, {
    updateTabsetPanel(inputId = "p_HAB", 
                      selected = if(input$HAB_sel == "DEFICIT") {"p_DEFICIT"} else {"p_HABITAT"})
  })

  
  
### Selección de aglomerados --------------------------------------------------
  # input$aglo
  
  ## MAPA 
  POLIGONO <- reactive({
    if(input$aglo == "Seleccionar (Todos)"){
      MAPA
    }else{
      map(MAPA, filter, aglo %in% input$aglo)
    }
  })

  ## INFRAEST
  SERVICIO <- reactive({
    req(input$tab1 %in% c("SER"))
    
    if(input$aglo == "Seleccionar (Todos)"){
      INFRAEST
    }else{
      map(INFRAEST, filter, aglo %in% input$aglo)
    }
  })
  
### Selección de radios visibles ----------------------------------------------------------
  
  # Límites del mapa visible
  # input$map_bounds
  
  limite <- reactive({
    req(input$tab1 %in% c("DEM", "POB", "MIG", "HAB"))
    req(input$map_bounds)
    
    b <- input$map_bounds

    ID <- R %>%
      filter(ymax <= b$north & ymin >= b$south &
               xmax <= b$east & xmin >= b$west) %>%
      select(ID)
    
    list(ID = ID$ID, marca = c(time = Sys.time(), simplify(b)) )
    
  })
  
  
  # limite <- debounce(limite_change, 1000)
  
  # Selección de radios visibles
  # limite()

  RADIO_r <- reactive({
    
    req(length(limite()$ID ) > 0)

    MAPA$RADIO %>%
      filter(ID %in% limite()$ID) 
    
  })
  
  # Envolvente de radios visibles
  # RADIO_r()
  
  RADIO_env <- reactive({
    
    RADIO_r() %>%
      summarise()
    
  })
  
  # Selección de base DEMOG (radios visibles)
  # limite()
  
  DEMOG_r <- reactive({
    
    req(length(limite()$ID ) > 0)
    
    DEMOG %>%
      filter(ID %in% limite()$ID) 
  })
  
  # Selección de centroides de radios visibles
  # limite() 
  
  CEN_r <- reactive({
    
    req(length(limite()$ID ) > 0)
    
    CEN %>%
      filter(ID %in% limite()$ID)
    
  })
  
  # Selección de centroide p/Migrantes 
  # (Panel MIG)
  # input$MIG_sel
  
  CEN_MIGRA_r <- reactive({
    
    req(input$tab1 %in% c("MIG") & length(input$MIG_sel) > 0 )
    
    CEN_r() %>%
      mutate(T_MIGRA = apply(st_drop_geometry(.[input$MIG_sel]), 1, sum),
             cuadro = paste0("<strong>", P_MIGRA, " Migrantes</strong><br>(",
                             round(P_MIGRA/Personas2010 * 100, 1),
                             "% de la población del radio)"),
             cuadro = ifelse(T_MIGRA == P_MIGRA, 
                             cuadro,
                             paste0(cuadro, 
                                    "<br><strong>Selección:</strong> ",
                                    "Migrantes de ", 
                                    paste(input$MIG_sel, collapse = ", " ),
                                    "<br>Total: ", T_MIGRA, 
                                    "<br>Proporción sobre Población Total: ", round(T_MIGRA/Personas2010*100, 1),"%",
                                    "<br>Proporción sobre Población Migrantes: ", round(T_MIGRA/P_MIGRA*100, 1),"%")),
             cuadro = as.character(cuadro),
             T_MIGRA = as.integer(T_MIGRA)
      )
  })
  
# Mapa base ---------------------------------------------------------------
  # POLIGONO()$AGLOMERADO

  output$map <- renderLeaflet({
    mapa <- POLIGONO()$AGLOMERADO %>%
      leaflet() %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addResetMapButton() %>%
      addMeasure(
        position = "bottomleft",
        primaryLengthUnit = "meters",
        primaryAreaUnit = "sqmeters",
        activeColor = "#3D535D",
        completedColor = "#7D4479") %>%
      addMiniMap(width = 100, height = 130,
                  zoomLevelOffset = -4, toggleDisplay = T) %>%
      addPolygons(fill = FALSE, stroke = TRUE, 
                  color = "tomato",
                  opacity = 0.2)
    
    if(is.null(input$muni) || input$muni %in% c("Seleccionar (Todos)", "")){
      mapa
    }else{
      bb <- POLIGONO()$MUNI %>% 
        filter(name == input$muni) %>% 
        st_bbox() %>% 
        as.numeric()

      mapa %>%
        clearBounds() %>%
        fitBounds(lng1 = bb[1], lng2 = bb[3],
                  lat1 = bb[2], lat2 = bb[4])

    }
  })

### Mapas suplementarios de territorios y envolventes ---------------------------
  
### Mapa de zonas/territorios
  # POLIGONO()

  # Municipios (POLIGONO()$MUNI)
  observe({
    map_base(base = "MUNI", POLIG_BASE = POLIGONO(),
             seleccion = input$zonas, fill = F,
             color = "black", opacity = 1)

  })

  # Barrios cerrados (POLIGONO()$COUNTRY)
  observe({
    map_base(base = "COUNTRY", POLIG_BASE = POLIGONO(),
             seleccion = input$zonas,
             color = "green")
  })

  # Barrios Populares (POLIGONO()$RENABAP)
  observe({
    map_base(base = "RENABAP", POLIG_BASE = POLIGONO(),
             seleccion = input$zonas, color = "blue")
  })

### Mapa con envolvente de radios visibles 
  # input$tab1  %in% c("DEM", "MIG") +  input$map_zoom >= 9 +  RADIO_env()
  
  observe({
    
    req(nrow(RADIO_env()))
    
    if(input$tab1 %in% c("DEM", "MIG") & 
       input$map_zoom >= 9 & 
       nrow(RADIO_env()) > 0){
      
      leafletProxy("map", data = RADIO_env()) %>%
        clearGroup("ENV") %>%
        addPolygons(group = "ENV",
                    color = "azure1",
                    fill = "azure1",
                    noClip = T,
                    opacity = 0) %>%
        showGroup("ENV")
    }else{
      leafletProxy("map",data = RADIO_env()) %>%
        clearGroup("ENV") 
    }
  })
  
### Descripción de indicadores -------------------------------------------------
  
  # DESARROLLAR
  
### Cálculo de distancias ------------------------------------------------------
  
  # DESARROLLAR
    
# 1. Infraestructura urbana (SERVICIO())  -------------------------

### Salud ---------------------------------------------------------
  # input$sel_infra 
  
  # Servicios generales (SERVICIO()$General)
  observe({
    sel_serv(base = "General",
             seleccion = input$sel_infra, 
             lista = SERVICIO(), 
             cluster = cluster())
  })

  # Servicios Especiales (SERVICIO()$Especialidad)
  observe({
    sel_serv(base = "Especialidad", 
             seleccion = input$sel_infra, 
             lista = SERVICIO(), 
             cluster = cluster())
  })

### Educación -----------------------------------------------------
  # input$sel_infra
  
  # Escuelas Estatales/Sociales (SERVICIO()$E_PUB)
  observe({
    sel_serv(base = "E_PUB", 
             seleccion = input$sel_infra, 
             lista = SERVICIO(),
             cluster = cluster())
  })

  # Escuelas Privadas (SERVICIO()$E_PRI)
  observe({
    sel_serv(base = "E_PRI", 
             seleccion = input$sel_infra, 
             lista = SERVICIO(),
             cluster = cluster())
  })

  # Universidades (SERVICIO()$universidades)
  observe({
    sel_serv(base = "universidades", 
             seleccion = input$sel_infra, 
             lista = SERVICIO(),
             cluster = cluster())
  })

### Servicios urbanos ---------------------------------------------
  # input$sel_infra

  # Mercados de alimentos (SERVICIO()$mercado)
  observe({
    sel_serv(base = "mercado", 
             seleccion = input$sel_infra, 
             lista = SERVICIO(),
             cluster = cluster())
  })
  # Financiero (SERVICIO()$financiero)
  observe({
    sel_serv(base = "financiero", 
             seleccion = input$sel_infra, 
             lista = SERVICIO(),
             cluster = cluster())
  })
  # Policia (SERVICIO()$seguridad)
  observe({
    sel_serv(base = "seguridad", 
            seleccion = input$sel_infra, 
            lista = SERVICIO(),
            cluster = cluster())
  })

### Heat map -------------------------------------------------------
  
  observe({
    
    req(input$sel_HEAT)

    heat_map(base = input$sel_HEAT, 
             lista = SERVICIO(),
             calor = input$act_heat,
             radio = input$radio,
             blur = input$blur)
    
  })
# 2. Demográficos (DEMOG_r + PIRAMIDE_r) ------------------------------------------------------------
# Armar gráfico para seleccionar por radio dentro de zoom.
# Transformar radios a puntos surface y calcular población

### Resumen población --------------------------------------------------------
  # DEMOG_r()
  
  output$resumen_dem <- renderTable({
    DEMOG_r() %>% 
      summarise(Población = sum(PERSONAS, na.rm = T),
                Hogares = sum(HOGARES, na.rm = T),
                Viviendas = sum(VIVIENDAS, na.rm = T),
                Radios = n())
  }, digits = 0)
  
### Pirámide población ---------------------------------------------------
  # PIRAMIDE_r()
  
  output$piramide <- renderPlot({
    PIRAMIDE_r()  %>%
      ggplot(mapping = aes(x = EDADQUI)) +
      geom_bar(aes(y = V), stat = "identity", fill = "blue", alpha = 0.4) +
      geom_bar(aes(y = M), stat = "identity", fill = "red", alpha = 0.4) +
      coord_flip() +
      labs(x = NULL, y = NULL) +
      scale_y_continuous(labels =  function(br) paste0(abs(br)/1000, "k")) +
      theme_minimal()
  })

# 3. Población -----------------------------------------------------------------

### Radios visibles con Indicadores-------------------------------------------------------------------------
  
  observe({
    
    req(input$map_zoom)
    req(input$POB_sel)
    
    if(input$tab1 %in% "POB" & 
       input$map_zoom >= 9 & 
       nrow(RADIO_r()) > 0){
      
      if(exists("nom") && nom != input$POB_sel){
        leafletProxy("map") %>%
          clearGroup("POB")
      }
      
      nom <- names(indic_POB)[indic_POB == input$POB_sel]
      r_aux <- RADIO_r() 
      r_aux$VARIABLE <- r_aux[[input$POB_sel]]
      r_aux <- r_aux %>%
          mutate(cuadro = paste0("<strong>", nom, ":</strong> ", 
                                 round(VARIABLE, 1), 
                                 cuadro_DEM))
      leafletProxy("map") %>%
          clearGroup("POB") %>%
          addRadio(data = r_aux, grupo = "POB", 
                   indicador = "VARIABLE", PAL = "YlOrRd") # %>%
        # showGroup("RAD")
    }else{
      leafletProxy("map",data = RADIO_env()) %>%
        clearGroup("POB") 
    }
  })
  

### Gráfico de densidad de variables  -------------------------------------------

  output$POB_histograma <- renderPlot({
    nom <- names(indic_POB)[indic_POB == input$POB_sel]
    
    data.frame(v = RADIO_r()[[input$POB_sel]]) %>%
        ggplot(aes(x = v)) + 
        geom_density(color = "tomato3") + 
        labs(x  = paste0(nom), y = "Densidad") +
        theme_minimal() 
      
  })
  
  
# 4. Migración (MIGRA_r + DEMOG_r + CEN_MIGRA_r)--------------------------
  
### Resumen Migrantes  ---------------------------------------------------
  # DEMOG_r()
  
  output$resumen_mig <- renderTable({
    
    DEMOG_r() %>% 
      summarise(Migrantes = sum(MIGRANTE, na.rm = T),
                Población = sum(PERSONAS, na.rm = T),
                Hogares = sum(HOGARES, na.rm = T),
                Viviendas = sum(VIVIENDAS, na.rm = T),
                Radios = n())
  }, digits = 0)
  
### Gráfico de Torta ------------------------------------------------------
  # MIGRA_r()
  
  output$migrantes <- renderPlot({
    
    req(input$tab1 %in% c("MIG") & length(input$MIG_sel) > 0 )
    
    MIGRA_r() %>%
      ggplot(aes(x = "", y = value, fill = name)) +
      geom_bar(stat = "identity", width = 1, color = "white") +
      coord_polar("y", start = 0) +
      theme_void()
  })
  
### Centroide en mapa ---------------------------------------------
  # CEN_MIGRA_r()
  
  observe({
    
    req(input$map_zoom)
    req(length(input$MIG_sel) > 0 )
    
    if(input$tab1 %in% c("MIG") && input$map_zoom >= 10 && nrow(CEN_MIGRA_r()) > 0){
      b <-  8 - (input$map_zoom- 9)/2
      b <- ifelse(b <= 1.5, 1.5, b)
      
      leafletProxy("map", data = CEN_MIGRA_r()) %>%
        clearGroup("CEN_M") %>%
        addCircleMarkers(group = "CEN_M", 
                         radius = ~logb(T_MIGRA + 1, base = b)**2,
                         fillColor = "red", 
                         opacity = 0,
                         options = pathOptions(pane = "overlayPane", zIndex = 1000),
                         popup = ~cuadro) %>%
        showGroup("CEN_M")
    }else{
      leafletProxy("map") %>%
        clearGroup("CEN_M") 
    }
  })
  
# 5. Hábitat ------------------------------------------------------------

### Déficit ------------------------------------------------------------- 
  
### Resumen Déficit -----------------------------------------------------
  # 
  # output$resumen_hab <- renderTable({
  #   renderTable({
  #     browser()
  #     
  #     t_aux <- RADIO_r()
  #     
  #     if(input$HAB_sel != "DEFICIT"){
  #       
  #       t_aux$indic <- t_aux[[input$HAB_sel]]
  #       
  #       if(input$HAB_sel != "H_HAC"){
  #         t_aux <- t_aux() %>% 
  #           summarise(Población = sum(Vivienda2010, na.rm = T),
  #                     Hogares = sum(Hogares2010, na.rm = T),
  #                     Viviendas = sum(Personas2010, na.rm = T),
  #                     Radios = n(),
  #                     INDIC = sum(indic, na.rm = T)
  #           )
  #         names(t_aux)[5] <- names(indic_HAB)[input$HAB_sel == indic_HAB]
  #         t_aux
  #       }else{
  #         
  #       }
  #         
  #     }
  #     
  #     
  #   }, digits = 0)
  # })
  
### Radios visibles con Indicadores -------------------------------------
  # RADIO_r() + input$HAB_sel + 
  # input$tab1 %in% "HAB" + input$map_zoom 
  
  observe({
    
    req(input$map_zoom)
    req(input$HAB_sel)
    
    if(input$tab1 %in% "HAB" & 
       input$map_zoom >= 9 & 
       nrow(RADIO_r()) > 0){
      
      if(exists("nom") && nom != input$HAB_sel){
        leafletProxy("map") %>%
          clearGroup("RAD")
      }
      
      nom <- names(indic_HAB)[indic_HAB == input$HAB_sel]
      r_aux <- RADIO_r() 
      
      if(input$HAB_sel != "DEFICIT"){
        
        r_aux$VARIABLE <- r_aux[[input$HAB_sel]]
        
        if(input$HAB_sel != "H_HAC"){
          r_aux <- r_aux %>%
            mutate(VARIABLE_p = as.numeric(VARIABLE/Hogares2010 * 100),
                   cuadro = paste0("<strong>", nom, ":</strong> ", 
                                   round(VARIABLE_p, 1), 
                                   "% (Total: ", round(VARIABLE, 1), ")",
                                   cuadro_DEM),
                   indicador = VARIABLE_p)
        }else{
          r_aux <- r_aux %>%
            mutate(cuadro = paste0("<strong>", nom, ":</strong> ", 
                                   round(VARIABLE, 1),
                                   cuadro_DEM),
                   indicador = VARIABLE)
        }

        leafletProxy("map") %>%
          clearGroup("RAD") %>%
          clearGroup("DEF") %>%
          addRadio(data = r_aux, grupo = "RAD", 
                   indicador = "indicador", PAL = "YlOrRd") # %>%
          # showGroup("RAD")
        
      }else{
        # DEFICIT
        r_aux <- r_aux %>%
          mutate(cuadro = paste0("<strong>Déficit de vivienda</strong>", "<br>",
                                 "<br>Viviendas Nuevas Requeridas cada 100 hogares: ", round(x100H_CUANT, 1),
                                 "<br>Viviendas Con Necesidad de Modificaciones cada 100 hogares: ", round(x100H_CUALI, 1),
                                 "<br>", cuadro_DEM))
        
        m <- leafletProxy("map", data = r_aux) %>%
          clearGroup("DEF") %>%
          clearGroup("RAD")
        
        # CUANTI
        if(input$DEF_ch == "CUANTI" || input$DEF_ch == "MIXTO"){
          
          m <- addRadio(map = m, data = r_aux, grupo = "DEF", 
                        indicador = "x100H_CUANT", PAL = "Reds")
        }
        # CUALI
        if(input$DEF_ch == "CUALI" || input$DEF_ch == "MIXTO"){
          m <- addRadio(map = m, data = r_aux, grupo = "DEF", 
                        indicador = "x100H_CUALI", PAL = "PuBu")
        }
        
      }
    }else{
      leafletProxy("map",data = RADIO_env()) %>%
        clearGroup("RAD") %>%
        clearGroup("DEF") 
    }
  })
 
### Gráfico de densidad de variables -------------------------------------

  output$HAB_histograma1 <- renderPlot({
      nom <- names(indic_HAB)[indic_HAB == input$HAB_sel]
      
      if(input$HAB_sel != "H_HAC"){
        data.frame(v = RADIO_r()[[input$HAB_sel]] / RADIO_r()$Hogares2010 * 100) %>%
          ggplot(aes(x = v)) + 
          geom_density(color = "tomato3") + 
          labs(x  = paste0("% ", nom), y = "Densidad") +
          theme_minimal() 
        
      }else{
        data.frame(v = RADIO_r()[[input$HAB_sel]]) %>%
          ggplot(aes(x = v)) + 
          geom_density(color = "tomato3") + 
          labs(x  = paste0("% ", nom), y = "Densidad") +
          theme_minimal() 
      }
  })
  
  output$HAB_histograma2 <- renderPlot({
    RADIO_r() %>% 
      select(Cuantitativo = x100H_CUANT, Cualitativo =  x100H_CUALI) %>%
      pivot_longer(cols = c(Cuantitativo, Cualitativo)) %>%
      ggplot(aes(x = value, color = name)) + 
      geom_density() + 
      labs(color = "Déficit e Vivienda", x  =  "Indicador de déficit", y = "Densidad") +
      theme_minimal() 
      
  })
  
### Resumen Déficit -----------------------------------------------------------
  
  output$DEF_t <- renderTable({
    DEMOG_r() %>%
      summarise(across(.cols = all_of(deficit), .fns = ~sum(.x, na.rm = T))) %>%
      pivot_longer(cols = everything( )) %>%
      mutate(Tot = paste(value),
             Por = paste(round(value / sum(value) * 100, 1), "%"),
             value = NULL)
  }, digits = 0, colnames= 0)
}

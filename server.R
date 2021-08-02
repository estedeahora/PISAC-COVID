server <- function(input, output, session) {

# Reactivos --------------------------------------------------------------
  
### Paneles UI ----------------------------------------------------------
  
  # Servicios (SER) -----------------------------*
  
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
  
  # Población (POB) -----------------------------*
  # Panel de selección de Población (Pirámide + Indicadores + Migración)
  
  observeEvent(input$DEM_ch, {
    updateTabsetPanel(inputId = "p_DEM", 
                      selected = input$DEM_ch)
  })

  # SALUD (SAL) -----------------------------*
  # Panel de selección de SALUD (COVID / COBERTURA)
  
  observeEvent(input$SAL_sel, {
    updateTabsetPanel(inputId = "p_SAL", 
                      selected = if(input$SAL_sel == "SALUDno") {"p_COBER"} else {"p_COVID"} )
  })
  
  observeEvent(input$COVID_ch, {
    updateTabsetPanel(inputId = "COVID", 
                      selected = input$COVID_ch)
  })
  
  
  # Hábitat (HAB) -----------------------------*
  # Panel de selección de Hábitat (Déficit vs resto)
  
  observeEvent(input$HAB_sel, {
    updateTabsetPanel(inputId = "p_HAB", 
                      selected = if(input$HAB_sel == "DEFICIT") {"p_DEFICIT"} else {"p_HABITAT"})
  })
  
### Reactivo para pirámide poblacional (DEM) -----------------------------------------------
  
  PIRAMIDE_r <- reactive({
    cat(as.character(Sys.time()), "PIRAM_r_pre", "\n")
    req(input$p_DEM %in% c("PIR"))
    cat(as.character(Sys.time()), "PIRAM_r", "\n")
    
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
  
### Reactivo para torta en Migración (MIG) -------------------------------------
  
  MIGRA_r <- reactive({
    cat(as.character(Sys.time()), "MIGRA_r", "\n")
    
    req(input$p_DEM %in% c("MIG"))
    
    DEMOG_r() %>%
      select(ID, eph_aglome, aglo, PERSONAS, MIGRANTE,
             'América Otros':'Asia y Oceanía') %>%
      summarise(across(.cols = 'América Otros':'Asia y Oceanía', 
                       .fns = ~sum(.x, na.rm = T)) ) %>%
      pivot_longer(cols = everything() ) %>%
      arrange(value)  %>%
      filter(name %in% input$MIG_sel)
  })
  
### Selección de aglomerados --------------------------------------------------
  # input$aglo
  
  ## MAPA 
  POLIGONO <- reactive({
    cat(as.character(Sys.time()), "POLIGONO", "\n")
    
    req(input$aglo)
    
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
    
    req(input$tab1 %in% c("DEM", "HAB", "MOV"))
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
    cat(as.character(Sys.time()), "RADIO_r_pre", "\n")
    req(length(limite()$ID ) > 0)
    cat(as.character(Sys.time()), "RADIO_r", "\n")

    MAPA$RADIO %>%
      filter(ID %in% limite()$ID) 
  })
  
  DEMOG_r <- reactive({
    cat(as.character(Sys.time()), "DEMOG_r_pre", "\n")
    
    req(length(limite()$ID ) > 0)
    cat(as.character(Sys.time()), "DEMOG_r", "\n")

    DEMOG %>%
      filter(ID %in% limite()$ID) 
  })
  
  # Selección de centroides de radios visibles
  # limite() 
  
  CEN_r <- reactive({
    cat(as.character(Sys.time()), "CEN_r", "\n")
    
    req(length(limite()$ID ) > 0)
    
    CEN %>%
      filter(ID %in% limite()$ID)
  })
  
  # Selección de centroide p/Migrantes 
  # (Panel MIG)
  # input$MIG_sel
  
  CEN_MIGRA_r <- reactive({
    cat(as.character(Sys.time()), "CEN_MIGRA_r_pre", "\n")
    
    req(input$p_DEM %in% c("MIG") & length(input$MIG_sel) > 0 )
    
    cat(as.character(Sys.time()), "CEN_MIGRA_r", "\n")
    
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
  
# Panel: Cartografía -----------------------------------------------------------
  
# 0. Mapa base ---------------------------------------------------------------
  # POLIGONO()$AGLOMERADO

  output$map <- renderLeaflet({
    cat(as.character(Sys.time()), "MAPA_PPAL", "\n")
    
    POLIGONO()$AGLOMERADO %>%
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
  })

### Mapas suplementarios de territorios ---------------------------
  
### Mapa de zonas/territorios
  # POLIGONO()

  observe({
    pmap(poligono, map_polig, 
         POLIG_BASE = POLIGONO(), seleccion = input$zonas)
  })
  
### Descripción de indicadores -------------------------------------------------
  
  output$desc <-  renderUI({

    ref_t <-  ref %>% filter(panel %in% input$tab1)
    final <- paste0("<strong>Descripción de los indicadores</strong>",
                    "<br>", ref_t$desc_panel [1],
                    # "<strong>Indicador: </strong>",
                    # ref_t$desc_indic[ref_t$desc_indic == ],
                    "<br><br><strong>Fuente: </strong>", ref_t$source [1])
    HTML(final)
  })
  
### Cálculo de distancias ------------------------------------------------------
  
  # DESARROLLAR
    
### Resumen de población -----------------------------------------------------
  # input$aglo
  # DEMOG_r()

  output$resumen_dem <- renderTable({

    req(input$aglo)

    t_res <- DEMOG_r() %>%
      summarise(Nivel = "Selección",
                Población = sum(PERSONAS, na.rm = T),
                Hogares = sum(HOGARES, na.rm = T),
                Viviendas = sum(VIVIENDAS, na.rm = T),
                Radios = n())

    if(input$aglo != "Seleccionar (Todos)"){
      sel <- c("Total Aglomerados", input$aglo)
    }else{
      sel <- c("Total Aglomerados")
    }

    rbind(resumen[sel, ],
          t_res)

  })
  
# 1. Infraestructura urbana (SERVICIO())  -------------------------

  observe({
    map(names(INFRAEST), map_serv,
        seleccion = input$sel_infra, 
        lista = SERVICIO(), cluster = cluster())
    
  })

### Heat map -------------------------------------------------------
  
  observe({
    
    req(input$sel_HEAT)

    map_heat(base = input$sel_HEAT, 
             lista = SERVICIO(),
             calor = input$act_heat,
             radio = input$radio,
             blur = input$blur)
  })
# 2. Demográfico ---------------------------------------------------------------
  
  # Mapa con envolvente de radios visibles (MIG + POB) -----------------------------------
  # input$p_DEM  %in% c("DEM", "MIG") +  input$map_zoom >= 9 +  RADIO_env()
  
  observe({
    
    # req(nrow(RADIO_env()))
    cat(as.character(Sys.time()), "MAPA_ENV", "\n")
    
    req(input$p_DEM)
    req(input$map_zoom)
    
    if(input$p_DEM %in% c("PIR", "MIG") && 
       input$tab1 == c("DEM") && 
       input$map_zoom >= 9){
      # if(nrow(RADIO_env()) > 0){
      req(RADIO_r())
      
      RADIO_r() %>%
        st_combine() %>%
        leafletProxy("map", data = .) %>%
        clearGroup("ENV") %>%
        addPolygons(group = "ENV",
                    color = "azure1",
                    fill = "azure1",
                    noClip = T,
                    opacity = 0) %>%
        showGroup("ENV")
      # }
    }else{
      leafletProxy("map") %>%
        clearGroup("ENV") 
    }
  })
### 2a Pirámide población ---------------------------------------------------
  # PIRAMIDE_r()
  
  output$piramide <- renderPlotly({
    
    req(input$p_DEM == "PIR")
    
    cat(as.character(Sys.time()), "PIRAMIDE", "\n")
    p <- PIRAMIDE_r()  %>%
      ggplot(mapping = aes(x = EDADQUI)) +
      geom_bar(aes(y = V, text = paste("Edad:", EDADQUI, "<br>Varones:", abs(V) ) ), 
               stat = "identity", fill = "blue", alpha = 0.4) +
      geom_bar(aes(y = M, text = paste("Edad:", EDADQUI, "<br>Mujeres:", abs(M) ) ), 
               stat = "identity", fill = "red", alpha = 0.4) +
      coord_flip() +
      labs(x = NULL, y = NULL) +
      scale_y_continuous(labels =  function(br) paste0(abs(br)/1000, "k")) +
      theme_minimal()
    
    ggplotly(p, tooltip="text") %>% plotly::config(plot_ly(), displayModeBar = FALSE)
  
  })

### 2b Población -----------------------------------------------------------------

  # Radios visibles con Indicadores --------------------------------------------
  
  observe({
    
    req(input$map_zoom)
    req(input$POB_sel)
    
    cat(as.character(Sys.time()), "MAPA_POB_pre", "\n")
    # Revisar. WARNING1. En SER, cuando cambia zoom pasa por acá. Quizás es para sacar mapa
    
    if(input$p_DEM == "POB" && 
       input$tab1 == "DEM" && 
       input$map_zoom >= 9){
      
      req(RADIO_r())
      
      cat(as.character(Sys.time()), "MAPA_POB", "\n")
        
        nom <- names(indic_POB)[indic_POB == input$POB_sel]
        
        r_aux <- RADIO_r() 
        r_aux$VARIABLE <- r_aux[[input$POB_sel]]
        r_aux <- r_aux %>%
          mutate(cuadro = paste0("<strong>", nom, ":</strong> ", 
                                 round(VARIABLE, 1), 
                                 cuadro_DEM))
        
        addRadio("map", data = r_aux, grupo = "POB", 
                 indicador = "VARIABLE", PAL = "YlOrRd")
      
    }else{
      leafletProxy("map") %>%
        clearGroup("POB") #%>%
        # clearControls()
    }
  })
  
  # Gráfico de densidad de variables -------------------------------------------
  # input$POB_sel + RADIO_r()
  
  output$POB_histograma <- renderPlot({
    
    req(RADIO_r())
    
    nom <- names(indic_POB)[indic_POB == input$POB_sel]
    data.frame(v = RADIO_r()[[input$POB_sel]]) %>%
        ggplot(aes(x = v)) + 
        geom_density(color = "tomato3") + 
        labs(x  = paste0(nom, "en los radios visualizados"), y = "Densidad") +
        theme_minimal() 
      
  })
  
### 2c. Migración (MIGRA_r + DEMOG_r + CEN_MIGRA_r)--------------------------
  
  # Gráfico de Torta ------------------------------------------------------
  # MIGRA_r()
  
  output$migrantes <- renderPlot({
    
    req(input$p_DEM %in% c("MIG") & length(input$MIG_sel) > 0 )
    cat(as.character(Sys.time()), "TORTA_MIG", "\n")
    
    MIGRA_r() %>%
      ggplot(aes(x = "", y = value, fill = name)) +
      geom_bar(stat = "identity", width = 2, color = "white") +
      coord_polar("y", start = 0) +
      labs (fill = "País / Región") +
      theme_void()
  })
  
  # Centroide en mapa ---------------------------------------------
  # CEN_MIGRA_r()
  
  observe({
    
    req(input$map_zoom)
    req(length(input$MIG_sel) > 0 )
    
    cat(as.character(Sys.time()), "MIGRA_CENT_pre", "\n")
    # Revisar. WARNING1. En SER, cuando cambia zoom pasa por acá. Quizás es para sacar mapa
    # También en Hábitat. Revisar en Gral
    
    if(input$p_DEM %in% c("MIG") && 
       input$tab1 %in% c("DEM") && 
       input$map_zoom >= 10 && 
       nrow(CEN_MIGRA_r()) > 0){
      b <-  8 - (input$map_zoom- 9)/2
      b <- ifelse(b <= 1.5, 1.5, b)
      cat(as.character(Sys.time()), "MIGRA_CENT", "\n")
      
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
  
# 3. Salud ---------------------------------------------------------------------
  
### Departamentos visibles con Indicadores --------------------------------------------
  
  observe({ 

    req(input$SAL_sel)

    cat(as.character(Sys.time()), "MAPA_SAL_pre", "\n")

    if(input$tab1 %in% "SAL" ){

      cat(as.character(Sys.time()), "MAPA_SAL", "\n")

      nom <- names(indic_SAL)[indic_SAL == input$SAL_sel]
      
      if(input$SAL_sel == "SALUDno"){
        # Cobertura de salud
        
        r_aux <- DEPTO
        r_aux$VARIABLE <- r_aux[[input$SAL_sel]]
        r_aux <- r_aux %>%
          mutate(cuadro = paste0("<strong>", nom, ":</strong> ",
                                 round(VARIABLE, 1)))
        r_sel <- NULL
        l_sel <-  F
        
      }else{
        # Covid 
        r_aux <- COVID %>% 
          select(starts_with(input$SAL_sel), cuadro)
        
        
        
        if(input$COVID_ch == "COVID_T"){
          # COVID: TOTAL
          r_aux$VARIABLE <- r_aux[[input$SAL_sel]]
          l_sel <-  F
          r_sel <- range(r_aux$VARIABLE)
        }else{
          # COVID: Por SE
          sel <- paste0(input$SAL_sel, "_", SE$se_y[SE$se_SEL  == input$SE])
          VAR <- r_aux[[sel]]
          
          if(is.null(VAR)){
            VAR <- rep(0, nrow(r_aux))
          }
          r_sel <- rango_COVID[rango_COVID$SEL == input$SAL_sel,
                               c("MIN", "MAX")] %>% 
            simplify() 
          
          if(input$SAL_sel %in% c("INI", "INT", "CUI")){
            r_aux$VARIABLE <- log(VAR+1)**2
            r_aux <- r_aux %>%
              mutate(cuadro = paste0("<strong>", input$SE, "</strong>",  
                                     "<br>", SE$LAB[SE$se_SEL  == input$SE],
                                     "<br><br><strong>", nom, ":</strong> ", 
                                     round(VAR, 1)))
            
            r_sel <- log(r_sel+1)**2
            l_sel <-  T
          }else{
            r_aux$VARIABLE <- VAR
            r_aux <- r_aux %>%
              mutate(cuadro = paste0("<strong>", input$SE, "</strong>",
                                     "<br>", SE$LAB[SE$se_SEL  == input$SE],
                                     "<br><br><strong>", nom, ":</strong> ",
                                     round(VARIABLE, 1)))
            l_sel <-  F
            
          }
        }
        
        
      }
      if(l_sel){
        transf <- function(x) {round(exp(x**(1/2)) - 1)}
      }else{
        transf <- function(x) {x}
      }
      
      addRadio("map", data = r_aux, grupo = "SAL", 
               rango = r_sel, transf = transf,
               indicador = "VARIABLE", PAL = "YlOrRd") 
      
    }else{
      leafletProxy("map") %>%
        clearGroup("SAL") 
      }
  })
  

### Gráfico de casos COVID -----------------------------------------------------
  
  output$COVID_CRONO <- renderPlot({
    
    plot_SE + 
      geom_vline(xintercept = SE$INI_p[SE$se_SEL == input$SE], 
                 alpha = 0.4,  size = 2, color = "tomato3") 
    
    # ggplotly(p, tooltip="text") %>% plotly::config(plot_ly(), displayModeBar = FALSE)
  })
  
# SE Texto ----------------------------------------------------------------

  output$SEL_SE <- renderText({
    SE$LAB[SE$se_SEL  == input$SE]
  })
  
# 4. Hábitat ------------------------------------------------------------

### Déficit ------------------------------------------------------------- 
  
### Radios visibles con Indicadores -------------------------------------
  # RADIO_r() + input$HAB_sel + 
  # input$tab1 %in% "HAB" + input$map_zoom 
  
  observe({
    
    req(input$tab1 %in% "SER")
    
    leafletProxy("map") %>%
      clearGroup("RAD") %>%
      clearGroup("DEF") %>% 
      clearGroup("CEN_M") %>% 
      clearGroup("POB") %>%
      clearGroup("ENV") %>%
      clearControls()
    
      })

  observe({
    
    req(input$map_zoom)
    req(input$HAB_sel)
    
    if(input$tab1 %in% "HAB" && 
       input$map_zoom >= 9 && 
       nrow(RADIO_r()) > 0){
      
      cat(as.character(Sys.time()), "MAP_HAB", "\n")
      
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
                   VARIABLE = VARIABLE_p)
        }else{
          r_aux <- r_aux %>%
            mutate(cuadro = paste0("<strong>", nom, ":</strong> ", 
                                   round(VARIABLE, 1),
                                   cuadro_DEM),
                   VARIABLE = VARIABLE)
        }

        addRadio("map", data = r_aux, grupo = "RAD", 
                 indicador = "VARIABLE", PAL = "YlOrRd") %>%
            clearGroup("DEF") 

      }else{
        # DEFICIT
        r_aux <- r_aux %>%
          mutate(cuadro = paste0("<strong>Déficit de vivienda</strong>", "<br>",
                                 "<br>Viviendas Nuevas Requeridas cada 100 hogares: ", round(x100H_CUANT, 1),
                                 "<br>Viviendas Con Necesidad de Modificaciones cada 100 hogares: ", round(x100H_CUALI, 1),
                                 "<br>", cuadro_DEM))
        
        m <- leafletProxy("map", data = r_aux) %>%
          clearGroup("DEF") %>%
          clearGroup("RAD") %>%
          clearControls()
        
        # CUANTI
        if(input$DEF_ch == "CUANTI" || input$DEF_ch == "MIXTO"){
          
          r_aux_CT <- r_aux %>% select(indic = x100H_CUANT, cuadro)
          PAL_f_CT <- paleta(PAL = "Reds", data = r_aux_CT, indic = "indic")
          
          m <- m %>% 
            addRadioPolyg(grupo = "DEF", data = r_aux_CT,
                          PAL_f = PAL_f_CT) %>%
            addRadioLeyenda(grupo = "DEF", PAL_f = PAL_f_CT, data = r_aux_CT)
        }
        # CUALI
        if(input$DEF_ch == "CUALI" || input$DEF_ch == "MIXTO"){
          
          r_aux_CL <- r_aux %>% select(indic = x100H_CUALI, cuadro)
          PAL_f_CL <- paleta(PAL = "PuBu", data = r_aux_CL, indic = "indic")
          
          m <- m %>% 
            addRadioPolyg(grupo = "DEF", data = r_aux_CL,
                          PAL_f = PAL_f_CL) %>%
            addRadioLeyenda(grupo = "DEF", PAL_f = PAL_f_CL, data = r_aux_CL)
        }
        m
        
      }
    }else{
      leafletProxy("map") %>%
        clearGroup("RAD") %>%
        clearGroup("DEF") 
    }
  })
 
### Gráfico de densidad de variables -------------------------------------

  output$HAB_histograma1 <- renderPlot({
    
    cat(as.character(Sys.time()), "HIST_HAB1", "\n")
    
      nom <- names(indic_HAB)[indic_HAB == input$HAB_sel]
      
      
      if(input$HAB_sel != "H_HAC"){
        data.frame(v = RADIO_r()[[input$HAB_sel]] / RADIO_r()$Hogares2010 * 100) %>%
          ggplot(aes(x = v)) + 
          geom_density(color = "tomato3") + 
          labs(x  = paste0(nom, "en los radios visualizados"), y = "Densidad") +
          theme_minimal() 
        
      }else{
        data.frame(v = RADIO_r()[[input$HAB_sel]]) %>%
          ggplot(aes(x = v)) + 
          geom_density(color = "tomato3") + 
          labs(x  = paste0(nom, "en los radios visualizados"), y = "Densidad") +
          theme_minimal() 
      }
  })
  
  output$HAB_histograma2 <- renderPlot({
    
    cat(as.character(Sys.time()), "HIST_HAB2", "\n")
    
    RADIO_r() %>% 
      st_drop_geometry() %>%
      select(Cuantitativo = x100H_CUANT, Cualitativo =  x100H_CUALI) %>%
      pivot_longer(cols = c("Cuantitativo", "Cualitativo")) %>%
      filter(!is.na(value)) %>%
      ggplot(aes(x = value, color = name)) + 
      geom_density() + 
      labs(color = "Déficit de Vivienda", 
           x  =  "Indicador de déficit en los radios visualizados", 
           y = "Densidad") +
      theme_minimal() 
      
  })
  
### Resumen Déficit -----------------------------------------------------------
  
  output$DEF_t <- renderTable({
    
    cat(as.character(Sys.time()), "RESUMEN_DEF", "\n")
    
    DEMOG_r() %>%
      summarise(across(.cols = all_of(deficit), .fns = ~sum(.x, na.rm = T))) %>%
      pivot_longer(cols = everything( )) %>%
      mutate(Tot = paste(value),
             Por = paste(round(value / sum(value) * 100, 1), "%"),
             value = NULL)
  }, digits = 0, colnames= 0)
# Panel: Evolución -------------------------------------------------------------

# 1. Gráfico -------------------------------------------------------------------

  output$EPH_plot <- renderPlot({

    req(input$EPH_aglo, input$tab_gral == "Evolución", input$EPH_ind)
    
    EPH_A <- map(EPH, filter, LAB %in% input$EPH_aglo)
    
    if (input$EPH_ind == "ING"){
      p <- EPH_A$H_Q %>%
        # Datos por Quintil
        ggplot() +
        geom_point(aes(x = ONDA, y = IPC_Mediana, color = ADECCFR2), alpha = 0.5) +
        geom_line(aes(x = ONDA, y = IPC_Mediana,
                      group = ADECCFR2, color = ADECCFR2),
                  alpha = 0.7, linetype = 2) +
        #Datos de Mediana
        geom_point(data = EPH_A$H_A, aes(x = ONDA, y = IPC_Mediana),
                   alpha = 0.7) +
        geom_line(data = EPH_A$H_A, aes(x = ONDA, y = IPC_Mediana, group = LAB),
                  alpha = 0.7) +
        # General
        scale_color_manual("Quintil IPC", breaks = 1:5,
                           values = c("#BF3F3F", "#BF7F3F",
                                      "#BFBF3F",
                                      "#3FBF7F", "#3F7FBF"),
                           labels = paste0(1:5, " Quintil")) +
        scale_y_continuous("Ingreso Per Capita del Hogar") 
    }
    if (input$EPH_ind == "OCU"){
      
      p_base <- EPH_A$I %>%
        ggplot() 
      # Desocupación
      p_base <- p_base + 
        geom_point(aes(x = ONDA, y = T_DES, color = "Tasa de Desocupación"), 
                  alpha = 0.7) +
        geom_line(aes(x = ONDA, y = T_DES, group = LAB, color = "Tasa de Desocupación"), 
                  alpha = 0.7, linetype = 2)
      
      # Actividad
      p_base <- p_base + 
        geom_point(aes(x = ONDA, y = T_ACT-0.15, color = "Tasa de Actividad"), 
                   alpha = 0.7) +
        geom_line(aes(x = ONDA, y = T_ACT-0.15, group = LAB, color = "Tasa de Actividad"), 
                  alpha = 0.7, linetype = 2)
      
      # Actividad
      p_base <- p_base + 
        geom_point(aes(x = ONDA, y = T_EMP-0.15, color = "Tasa de Actividad"), 
                   color = "darkgreen", alpha = 0.7) +
        geom_line(aes(x = ONDA, y = T_EMP-0.15, group = LAB, color = "Tasa de Empleo"), 
                  alpha = 0.7, linetype = 2)
      
      # Estético
      p <- p_base +
        scale_color_discrete("Indicador") +
        scale_y_continuous("Tasa de Desocupación (%)",
                           labels = scales::percent_format(accuracy = 1), 
                           sec.axis =  sec_axis(name = "Tasa de Empleo y Actividad (%)",
                                                trans = ~ . + 0.15, 
                                                labels = scales::percent_format(accuracy = 1))
                             
                           ) 
    }
    if (input$EPH_ind == "SAL"){
      
      p <- EPH_A$I %>%
        ggplot() + 
        geom_point(aes(x = ONDA, y = T_SAL), 
                   color = "darkgreen", alpha = 0.7) +
        geom_line(aes(x = ONDA, y = T_SAL, group = LAB), 
                  color = "darkgreen", alpha = 0.7, linetype = 2) +
        scale_y_continuous("% de Personas sin cobertura de salud privada",
                           labels = scales::percent_format(accuracy = 1))
    }
    p +
      labs(x = "Relevamiento EPH") +
      # Inicio ASPO
      geom_vline(xintercept = "2T-2020", linetype = 2,
                 color = "tomato3", alpha = 0.3) +
      # Facet
      facet_wrap(~LAB, scales = "fixed") +
      # Theme
      theme_minimal() +
      theme(axis.text.x = element_text(size = 6, angle = 45),
            axis.text.y = element_text(size = 6),
            strip.text.x = element_text(size = 7), 
            legend.position = "bottom") 
  })
}

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

  # Salud (SAL) -----------------------------*
  
  # Panel de selección de SALUD (COVID / COBERTURA)
  observeEvent(input$SAL_sel, {
    updateTabsetPanel(inputId = "p_SAL", 
                      selected = if(input$SAL_sel == "SALUDno") {"p_COBER"} else {"p_COVID"} )
  })
  
  # Panel de selección de Total / Cronológico
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
  

  # Movilidad (MOV) --------------------------*
  # Panel de selección de Total / Por hora
  
  observeEvent(input$SUBE_ch, {
    updateTabsetPanel(inputId = "SUBE", 
                      selected = if(input$SUBE_ch) "SUBE_C" else "SUBE_T" )
  })
  
  output$SUBE_DIA <- renderUI({
    value <- isolate(input$SUBE_DIA_din)
    
    if(input$SUBE_rel){
      ini <- SUBE_d$lab[2]
    }else{
      ini <- SUBE_d$lab[1]
    }
    sliderTextInput(
      inputId = "SUBE_DIA_din",
      label = "Seleccionar día",
      choices = SUBE_d$lab, 
      selected = value,
      from_min = ini,
      animate = animationOptions(interval = 3000),
      grid = T,
      hide_min_max = T
    )
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
  
  ## MOVILIDAD
  
  SUBE <- reactive({
    
    req(input$tab1 %in% c("MOV"))
    cat(as.character(Sys.time()), "REAC_SUBE", "\n")
    
    # req(!s, cancelOutput = TRUE)
    if(input$aglo == "Seleccionar (Todos)"){
      AGLO_h3_c
    }else{
      AGLO_h3_c %>% filter(aglo %in% input$aglo)
    }
  })
  
  s <- reactive({
    input$aglo %in% c("Seleccionar (Todos)", "Bariloche", "AMGBA", 
                      "Gran Santa Fe-Paraná", "Gran Mendoza")
  })
  
  SUBE_rango <- reactive({
    cat(as.character(Sys.time()), "REAC_SUBE_rango", "\n")
    
    if(!input$SUBE_rel){
      db <- SUBE() %>% 
        st_drop_geometry()
      
      nom <- names(db)

      if(input$SUBE_ch){  
        sel <- str_starts(nom, "SUBE_") 
      }else{
        sel <- str_starts(nom, "TOTAL_") 
      }
      db[sel] |> simplify() |> quantile(na.rm = T, probs = seq (0, 1, 0.05)) |> round() |>  unique()
      
    } 
    
  })
    
  observe({
    shinyFeedback::feedbackWarning(inputId = "aglo", !s() & input$tab1 %in% c("MOV"), 
                                   "Este aglomerado no tiene datos de movilidad")
  })
  
  
### Selección de radios visibles ----------------------------------------------------------
  
  # Límites del mapa visible
  # input$map_bounds
  
  limite <- reactive({
    cat(as.character(Sys.time()), "LIMITE", "\n")
    req(input$tab1 %in% c("DEM", "HAB"))
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
  
### Resumen de población -----------------------------------------------------
  # input$aglo
  # DEMOG_r()
  
  output$resumen_dem <- renderTable({
    
    req(input$aglo)
    
    if(input$aglo == "Seleccionar (Todos)"){
      sel <- c("Total Aglomerados")
    }else{
      sel <- c("Total Aglomerados", input$aglo)
    }
    
    if(input$tab1 %in% c("DEM", "HAB") ){
      t_res <- DEMOG_r() %>%
        summarise(Nivel = "Selección",
                  Poblacion = sum(PERSONAS, na.rm = T),
                  Hogares = sum(HOGARES, na.rm = T),
                  Viviendas = sum(VIVIENDAS, na.rm = T),
                  Radios = n())
      tf <- rbind(resumen[sel, ],
            t_res)
    }else{
      tf <- resumen[sel, ] %>% select(-Radios)
    }
    
    tf %>% 
      mutate(across(.cols = -Nivel,
                    .fns = ~format(as.numeric(.x), big.mark = " ", trim = F) ))
  }, align = "r")
  
### Descripción de indicadores -------------------------------------------------
  
  output$desc <-  renderUI({

    if(input$tab1 %in% c("SER", "MOV")){
        ref_f <- ref %>% 
                  filter(panel %in% input$tab1)
      
    }else if(input$tab1 == "DEM"){
        ref_f <- ref %>% 
                   filter(panel == input$tab1 &
                          subpanel == input$DEM_ch)
        
        if(input$DEM_ch == "POB"){
          ref_f <- ref_f %>% 
            filter(indic == input$POB_sel)
        }
    }else if(input$tab1 == "SAL"){
        ref_f <- ref %>% 
          filter(panel == input$tab1 &
                 indic == input$SAL_sel)
        
    }else if(input$tab1 == "HAB"){
        ref_f <- ref %>% 
          filter(panel == input$tab1 &
                 indic == input$HAB_sel)
    }
    
    
    if(nrow(ref_f) != 1) stop("Problema con descipción de " )
    
    final <- paste0("<strong>", ref_f$desc_panel,": </strong>",
                    ref_f$desc_indic, 
                    "<br><br><strong>Fuente: </strong>", ref_f$source,
                    "<br><em>Notas: </em>", ref_f$notes
                    )
    HTML(final)
  })
  
### Cálculo de distancias ------------------------------------------------------
  
  # DESARROLLAR
    
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

# 2. Salud ---------------------------------------------------------------------

### Departamentos con Indicadores --------------------------------------------

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
      clearGroup("SAL") %>%
      clearControls()
  }
})

### Gráfico de casos COVID -----------------------------------------------------

output$COVID_CRONO <- renderPlot({
  if(input$COVID_ch == "COVID_T"){
    plot_SE
  }else{
    plot_SE + 
      geom_vline(xintercept = SE$INI_p[SE$se_SEL == input$SE], 
                 alpha = 0.4,  size = 2, color = "tomato3") 
  }
  
  
  # ggplotly(p, tooltip="text") %>% plotly::config(plot_ly(), displayModeBar = FALSE)
})

### SE Texto ----------------------------------------------------------------

output$SEL_SE <- renderText({
  SE$LAB[SE$se_SEL  == input$SE]
})


# 3. Demográfico ---------------------------------------------------------------
  
  # Mapa con envolvente de radios visibles (MIG + POB) -----------------------------------
  # input$p_DEM  %in% c("DEM", "MIG") +  input$map_zoom >= 9 +  RADIO_env()
  
  observe({
    
    # req(nrow(RADIO_env()))
    cat(as.character(Sys.time()), "MAPA_ENV_PRE", "\n")
    
    req(input$p_DEM)
    req(input$map_zoom)
    
    if(input$p_DEM %in% c("PIR", "MIG") && 
       input$tab1 == c("DEM") && 
       input$map_zoom >= 9){
      # if(nrow(RADIO_env()) > 0){
      cat(as.character(Sys.time()), "MAPA_ENV", "\n")
      req(RADIO_r())
      
      RADIO_r() %>%
        st_combine() %>%
        leafletProxy("map", data = .) %>%
        clearGroup("ENV") %>%
        clearControls() %>%
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
      mutate(Tot = format(value, big.mark = " "),
             Por = paste0(format(round(value / sum(value) * 100, 1), 
                                 decimal.mark = ","), "%"),
             value = NULL) 
  }, digits = 0, colnames= 0, align = "lrr" )
# 5. Movilidad -----------------------------------------------------------------
  
### Hexágonos visibles con Indicadores
  
  observe({ 
    
    req(input$tab1 %in% "MOV")
    
    cat(as.character(Sys.time()), "MAPA_MOV_pre", "\n")
    if(input$tab1 %in% "MOV"){
      cat(as.character(Sys.time()), "MAPA_MOV", "\n")
      
      # nom <- names(indic_SAL)[indic_SAL == input$SAL_sel]
      # SUBE()
      
    }else{
      leafletProxy("map") %>%
        clearGroup("MOV") 
    }
  })
  
  
### Hexágonos con Indicadores --------------------------------------------------
  
  
  observe({ 
    
    req(input$SUBE_DIA_din)
    if(!s()) req(F)
      
    cat(as.character(Sys.time()), "MAPA_MOV_pre", "\n")
    
    if(input$tab1 %in% "MOV"){

      cat(as.character(Sys.time()), "MAPA_MOV", "\n")
      
      d <- SUBE_d$val[SUBE_d$lab == input$SUBE_DIA_din]
      h <- SUBE_h$val[SUBE_h$lab == input$Hs]
      
      if(input$SUBE_ch){
        nom <- nom_t <- paste("SUBE", d, h, sep = "_")
        cuadro <- paste0("<strong>Día: ", input$SUBE_DIA_din, 
                         ". Rango horario: ",  input$Hs, ".</strong>")
      }else{
        nom <- nom_t <- paste("TOTAL", d, sep = "_")
        cuadro <- paste0("<strong>Día: ", input$SUBE_DIA_din, ".</strong>")
      }
      
      if(input$SUBE_rel){
        nom <- paste("p", nom, sep = "_")
        
        cortes <- c(seq(0, 1, by = 0.20), 1.5, 2, 100)
        PAL_l <- c(paste0(lag(cortes*100), "-",
                         cortes*100,  "%")[-length(cortes)][-1], 
                            "+200%")
        PAL <- "BrBG"
        inv <- T
      }else{
        cortes <- SUBE_rango()
        PAL <- "viridis"
        inv <- F
      }
      
      req(nom != "p_TOTAL_2020.03.11")
      
      r_aux <- SUBE() %>%
        mutate(VARIABLE = .data[[nom]],
               cuadro =  paste0(cuadro, "<br>Transacciones totales: ", .data[[nom_t]]),
               cuadro = case_when(input$SUBE_rel & !is.na(VARIABLE) ~ paste0(cuadro, 
                                                                             "<br>% de variación (referencia 11 marzo 2020): ",
                                                                             round(VARIABLE*100, 1), "%"),
                                  T ~ paste0(cuadro) )
        )
      
      PAL_f <- colorBin(palette = PAL, reverse = inv,
                        domain = r_aux$VARIABLE, 
                        bins = cortes, na.color = "#A9A9A9")
      
      mapa <- leafletProxy("map", data = r_aux) %>%
        clearGroup("MOV") %>%
        clearControls() %>%
        addPolygons(group = "MOV", smoothFactor = 0.5,
                    popup = ~cuadro,
                    opacity = 1.0, color = "#BDBDBD", weight = 0.3,
                    fillColor = ~PAL_f(VARIABLE), fillOpacity = 0.4,
                    highlightOptions = highlightOptions(color = "black", weight = 1,
                                                        bringToFront = TRUE) ) 
      
      if(input$SUBE_rel){
        PAL_c <- PAL_f(cortes)[-length(cortes)] # hex codes
        
        mapa %>%
          addLegend(colors = PAL_c, labels = PAL_l,
                    na.label = "Sin viajes", title = "Viajes Respecto<br>a Marzo 2020")
                  # na.label = "NA"
      }else{
        mapa %>%
          addLegend(pal = PAL_f , values = ~VARIABLE, 
                    na.label = "Sin viajes", title = "Viajes Totales")
                    
      }
    }else{
      leafletProxy("map") %>%
        clearGroup("MOV") %>%
        clearControls()
    }
  })
  
### Gráfico transacciones por hora ---------------------------------------------
  
    output$SUBE_CRONO <- renderPlot({
      
      if(!s()) validate("Este aglomerado no tiene datos de movilidad")
      
      req(nrow(SUBE()) > 0, input$SUBE_DIA_din)
      
      if(input$aglo == "Seleccionar (Todos)"){
        db_aux <- hr %>%
          count(dia, hora, wt = n)
      }else{
        db_aux <- hr %>%
          filter(aglo == input$aglo)
      }
      p <- db_aux %>%
        mutate(l = ifelse(dia == input$SUBE_DIA_din, "1", "2") ) %>%
        ggplot() + 
        geom_line(aes(x = hora, y = n, color = dia,  linetype = l),
                  alpha = 0.8) +
        scale_x_continuous("Hora del día", breaks = seq(0, 23, by = 3)) +
        scale_y_continuous("Total dex transacciones SUBE", labels = \(x) paste0(x/1000, "k"), 
                           limits = c(0, max(db_aux$n) *1.05) ) +
        scale_color_discrete("") +
        guides(linetype = "none") +
        theme_minimal() +
        theme(legend.position="bottom")
      
      if(input$SUBE_ch){
        lim  <-  data.frame(xmin = SUBE_h$ini[SUBE_h$lab == input$Hs],
                         xmax = SUBE_h$fin[SUBE_h$lab == input$Hs],
                         ymin = 0,
                         ymax = max(db_aux$n) *1.05 ) 
        
        p + 
          geom_rect(data = lim, 
                    aes(xmin=xmin, xmax = xmax,
                        ymin = ymin, ymax = ymax), 
                     alpha = 0.4,  fill = "tomato2")
      }else{
        p
      }
      
    })
# Panel: Aglomerados -----------------------------------------------------------
  
### Gráfico de indicadores -----------------------------------------------------

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
  
# Panel: Descarga --------------------------------------------------------------
  
  data <- reactive({
    db <- list()
    if("R_download" %in% input$download_db){
      db$Radio <- MAPA$RADIO %>%
        select(-c(cuadro_DEM, x100H_CUANT, x100H_CUALI)) %>%
        left_join(DEMOG %>% select(-c(eph_aglome:PERSONAS)),
                  by = "ID")
    }
    if("D_download" %in% input$download_db){
      db$Depto <- DEPTO %>%
        select(ID_DTO:provincia, OS:SinCobertura) %>%
        left_join(st_drop_geometry(COVID ), by = "ID_DTO")
    }
    if("H_download" %in% input$download_db){
      db$Hexag <- AGLO_h3_b
    }
    if("L_download" %in% input$download_db){
      db$Lugar <- bind_rows(INFRAEST, .id = "tipo")
    }
    
    if(!input$download_geo & length(db) > 0 ){
      db <- map(db, st_drop_geometry)
    }
    # shinyFeedback::feedbackWarning(inputId = "download_db", !(length(db) > 0), 
    #                                "Seleccione al menos una base")
    db
  })

  # modal_confirm <- modalDialog(
  #   "Debe seleccionar algún archivo para descargar",
  #   title = "Downloading files",
  #   footer = tagList(
  #     actionButton("cancel", "Cancel"),
  #     actionButton("ok", "Delete", class = "btn btn-danger")
  #   )
  # )
  # 
  # observeEvent(input$downloadData, {
  #   showModal(modal_confirm)
  # })
  # 
  # observeEvent(input$ok, {
  #   showNotification("Files deleted")
  #   removeModal()
  # })
  # observeEvent(input$cancel, {
  #   removeModal()
  # })

  output$downloadData <- downloadHandler(
    filename = "TRIP_COVID.zip",
    
    content = function(fname) {
      
      if(length(data()) > 0){
        waiter <- waiter::Waiter$new(id = "download_db")
        waiter$show()
        
        if(input$download_geo){
          path <-  paste0(tempdir(),"/db_", names(data()), ".geojson")
          walk2(data(), path, st_write)
        }else{
          path <-  paste0(tempdir(),"/db_", names(data()), ".csv")
          walk2(data(), path, write.csv)
        }
        zip(zipfile = fname, files = path, flags = "-j")
        on.exit(waiter$hide())
      }else{
        path <- paste0(tempdir(), "/empty")
        file.create(path )
        zip(fname, 'empty')
      }
    }, contentType = "application/zip"
  )
  
}

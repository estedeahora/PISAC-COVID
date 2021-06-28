ui <- fluidPage(
  tabsetPanel(
    tabPanel("Cartografía", icon = icon("map"),
             # Encabezado para seleccionar aglomerados y polígonos principales
             fluidRow(
               column(width = 4,
                      fluidRow(
                        column(width = 6,
                               selectInput("aglo", "Elija el Aglomerado",
                                           choices = AGLO)
                        ),
                        column(width = 2,
                               tabsetPanel(
                                 #   id = "switcher",
                                 # type = "hidden",
                                 # tabPanelBody("panel1", ""),
                                 # tabPanelBody("panel2",
                                 # selectInput("muni", "Elija el Municipio",
                                 #             choices = NULL))
                               )
                        ),
                      ),
                      fluidRow(
                        column(width = 12,
                          checkboxGroupInput("zonas", "Zonas urbanas",
                                             poligono, inline = T)
                        )
                      ),
               ),
               
               # column(width = 3, 
                      # Selección de Polígonos generales
                      # checkboxGroupInput("zonas", "Zonas urbanas",
                      #                    poligono, inline = T)
               # ),
               column(width = 5,
                      tableOutput("resumen_dem")
                      )
               
             ), 
             # Body de la API
             fluidRow(
               sidebarLayout(
                 # Panel de selección
                 sidebarPanel(
                   tabsetPanel(id = "tab1",
                               # Panel de servicios urbanos (SER)
                               tabPanel("Infraestructura", value = "SER",
                                        icon = icon("hospital-alt"),
                                        pickerInput(
                                          inputId = "sel_infra",
                                          label = "Seleccione servicios para mostrar",
                                          choices = list('Salud' = infraestructura[1:2],
                                                         'Educación' = infraestructura[3:5],
                                                         'Servicios urbanos' = infraestructura[6:8]),
                                          # choicesOpt = list(content = icon_sel$img),
                                          options = list(
                                            `actions-box` = TRUE,
                                            `selected-text-format` = "count > 3"
                                          ),
                                          multiple = TRUE
                                        ),
                                        
                                        switchInput(inputId = "clu", label = "Clusters", value = TRUE, labelWidth = "50",
                                                    onLabel = "Sí",  offLabel = "No", size = "mini"),
                                        HTML("<br>"),
                                        strong("Mapa de calor"),
                                        switchInput(inputId = "act_heat", label = "Heatmap", value = FALSE,
                                                    onLabel = "Sí",  offLabel = "No", size = "mini", inline = F),
                                        tabsetPanel(
                                          id = "p_HEAT",
                                          type = "hidden",
                                          tabPanelBody("p_HEATNO"),
                                          tabPanelBody("p_HEATSI", 
                                                       selectInput("sel_HEAT", "Elija Infraestructura urbana",
                                                                   choices = infraestructura, 
                                                                   selected = infraestructura[1]),
                                                       column(width = 6, 
                                                              sliderInput("radio", "Radio", value = 8, 
                                                                          min = 1, max = 50)
                                                       ),
                                                       column(width = 6, 
                                                              sliderInput("blur", "Esfumado", value = 6, 
                                                                          min = 10, max = 80)
                                                       )
                                                       
                                                       # switchInput(inputId = "heat", label = "Heatmap", value = FALSE,
                                                       #             onLabel = "Sí",  offLabel = "No", size = "mini", inline = F)
                                                       
                                          )
                                        )
                                        
                                        
                               ),
                               # Panel de datos demográfico
                               tabPanel("Demográfico", value = "DEM",
                                        icon = icon("align-center", lib = "glyphicon"),
                                        # tableOutput("resumen_dem"),
                                        # switchInput(inputId = "pir", value = TRUE, size = "small",
                                        #             onLabel = "N", offLabel = "%"),
                                        plotOutput("piramide")
                               ),
                               # Panel de Variables para polígonos (VER NOMBRE)
                               tabPanel("Población", value = "POB",
                                        icon = icon("group"),
                                        selectInput("POB_sel", "Elija indicador", choices = indic_POB),
                                        # switchInput(inputId = "edu", value = TRUE,
                                        #             size = "small",
                                        #             onLabel = "Primario", offLabel = "Secundario"),
                                        # switchInput(inputId = "pob", value = TRUE,
                                        #             size = "small",
                                        #             onLabel = "N", offLabel = "%"),
                                        # sliderInput("lim", "", value = c(0, 100), min = 0, max = 100),
                                        
                                        plotOutput("POB_histograma")
                                        
                               ),
                               # Panel de Migración
                               tabPanel("Migración", value = "MIG",
                                        icon = icon("flag"),
                                        pickerInput(
                                          inputId = "MIG_sel",
                                          label = "Seleccion e grupos migrantes",
                                          choices = paises,
                                          selected = unname(unlist(paises)),
                                          options = list(
                                            `actions-box` = TRUE,
                                            # size = 10,
                                            `selected-text-format` = "count > 3"
                                          ),
                                          multiple = TRUE
                                        ),
                                        # tableOutput("resumen_mig"),
                                        plotOutput("migrantes")
                                        
                               ),
                               # Panel de Hábitat
                               tabPanel("Hábitat", value = "HAB",
                                        icon = icon("home"),
                                        selectInput("HAB_sel", "Elija un indicador", choices = indic_HAB),
                                        tableOutput("resumen_hab"),
                                        tabsetPanel(
                                          id = "p_HAB",
                                          type = "hidden",
                                          tabPanelBody("p_DEFICIT", 
                                                   radioGroupButtons(
                                                     inputId = "DEF_ch",
                                                     label = "", selected = "MIXTO",
                                                     choices = c("Cuantitativo" = "CUANTI",
                                                                 "Mixto" = "MIXTO",
                                                                 "Cuanlitativo" = "CUALI"),
                                                     justified = TRUE,
                                                     status = "primary",
                                                     checkIcon = list(
                                                       yes = icon("ok",  style = "color: steelblue",
                                                                  lib = "glyphicon"))),
                                                   tabsetPanel(
                                                     tabPanel("Tabla",
                                                              tableOutput("DEF_t")
                                                     ),
                                                     tabPanel("Densidad",
                                                              plotOutput("HAB_histograma2")
                                                     )
                                                   )
                                          ),
                                          tabPanelBody("p_HABITAT",
                                                   plotOutput("HAB_histograma1")
                                          ) 
                                        ),
                                        
                               )
                   ),
                 ),
                 # Mapa principal
                 mainPanel(width = 7,
                   leafletOutput("map",  height = 600)
                 )
               )
             ),
             fluidRow(
               column(width = 4, 
                      htmlOutput("desc")
                      )
             #   column(width = 8, 
             #          # DESARROLLAR CALCULO DE DISTANCIAS
             #          # fluidRow(
             #          #   actionButton("boton_dis", "Calcular distancia")
             #          #   ),
             #          # fluidRow(
             #          #   "Inicio: ",
             #          #   textOutput("x_point"),
             #          #   htmlOutput( "<br>"), "Final: ", 
             #          #   textOutput("y_point")
             #          # )
             #   )           
             )
    ),
    tabPanel("Evolución", icon = icon("line-chart")),
    tabPanel("Proyecto",
             h1("Acerca del proyecto"),
             "Esta aplicación fue desarrollada en el marco del proyecto PISAC-COVID, coordinado por María Mercedes Di Virgilio...",
             h1("Nota metodológica"),
             "Los datos presentados fueron extraídos del CENSO 2010 y la EPH. A su vez, se utilizó cartografía proporcionada por IGN y el Ministerio de Educación para la localización de los establecimientos estatales de salud y las escuelas, respectivamente"
    )
    
    
  )
)

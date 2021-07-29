ui <- fluidPage(#theme = shinytheme("united"),
  tabsetPanel(id = "tab_gral",
    tabPanel("Cartografía", icon = icon("map"),
             # Encabezado para seleccionar aglomerados y polígonos principales
             fluidRow(
               column(width = 3,
                      fluidRow(
                        column(width = 12,
                               selectInput("aglo", "Elija el Aglomerado",
                                           choices = AGLO)
                        )
                      ),
                      fluidRow(
                        column(width = 12,
                               checkboxGroupInput("zonas", "Zonas urbanas",
                                                  var_polig, inline = T)
                        )
                      ),
               ),
               column(width = 5,
                      tableOutput("resumen_dem")
               )
             ), 
             # Body de la API
             fluidRow(
               sidebarLayout(
                 # Panel de selección
                 sidebarPanel(width = 3,
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
                                        
                                        switchInput(inputId = "clu", label = "Clusters", 
                                                    value = TRUE, labelWidth = "50",
                                                    onLabel = "Sí",  offLabel = "No", 
                                                    onStatus = "danger", size = "mini"),
                                        HTML("<br>"),
                                        strong("Mapa de calor"),
                                        switchInput(inputId = "act_heat", 
                                                    label = "Heatmap", value = FALSE,
                                                    onLabel = "Sí",  offLabel = "No", 
                                                    size = "mini", onStatus = "danger"),
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
                                                              sliderInput("blur", "Esfumado", value = 20, 
                                                                          min = 10, max = 80)
                                                       )
                                          )
                                        )
                               ),
                               # Panel de datos demográficos
                               tabPanel("Demográfico", value = "DEM",
                                        icon = icon("group"),
                                        radioGroupButtons(
                                          inputId = "DEM_ch",
                                          label = "", selected = "PIR",
                                          choices = c("<i class='fa fa-align-center'> Pirámide </i>" = "PIR",
                                                      "<i class='fa fa-user'> Población </i>" = "POB",
                                                      "<i class='fa fa-flag'> Migración </i>" = "MIG"),
                                          justified = TRUE,
                                          checkIcon = list(
                                            yes = icon("ok",  style = "color: Coral",
                                                       lib = "glyphicon"))),
                                        tabsetPanel(
                                          id = "p_DEM",
                                          type = "hidden",
                                          # Sub Panel de Pirámide de población
                                          tabPanel("Pirámide", value = "PIR",
                                                   # icon = icon("align-center", lib = "glyphicon"),
                                                   plotlyOutput("piramide")
                                          ),
                                          # Sub Panel de Variables para población
                                          tabPanel("Población", value = "POB",
                                                   # icon = icon("user"),
                                                   selectInput("POB_sel", "Elija indicador", choices = indic_POB),
                                                   # sliderInput("lim", "", value = c(0, 100), min = 0, max = 100),
                                                   plotOutput("POB_histograma")
                                          ),
                                          # Sub Panel de Migración
                                          tabPanel("Migración", value = "MIG",
                                                   # icon = icon("flag"),
                                                   pickerInput(
                                                     inputId = "MIG_sel",
                                                     label = "Seleccion e grupos migrantes",
                                                     choices = paises,
                                                     selected = unname(unlist(paises)),
                                                     options = list(
                                                       `actions-box` = TRUE,
                                                       `selected-text-format` = "count > 3"
                                                     ),
                                                     multiple = TRUE
                                                   ),
                                                   plotOutput("migrantes")
                                                   
                                          )
                                        )
                               ),
                               # Panel de Salud
                               tabPanel("Salud", value = "SAL",
                                        icon = icon("stethoscope"),
                                        selectInput("SAL_sel", "Elija indicador", choices = indic_SAL),
                                        tabsetPanel(
                                          id = "p_SAL",
                                          type = "hidden",
                                          tabPanelBody("p_COBER",
                                                       plotOutput("SAL_histograma")
                                          ),
                                          tabPanelBody("p_COVID",
                                                       radioGroupButtons(
                                                         inputId = "COVID_ch",
                                                         choices = c("Total" = "COVID_T",
                                                                     "Cronológico" = "COVID_C"),
                                                         justified = TRUE,
                                                         checkIcon = list(
                                                           yes = icon("ok",  style = "color: Coral",
                                                                      lib = "glyphicon"))),
                                                       tabsetPanel(
                                                         id = "COVID",
                                                         type = "hidden",
                                                         tabPanelBody("COVID_T",
                                                                      "TOTAL DE CASOS - WIP"
                                                                      ),
                                                         tabPanelBody("COVID_C",
                                                                      sliderTextInput("SE", "Semana epidemeológica", 
                                                                                      animate = TRUE,
                                                                                      choices = SE$se_SEL,
                                                                                      force_edges = TRUE,
                                                                                      width = '100%'
                                                                      ),
                                                                      textOutput("SEL_SE"),
                                                                      plotOutput("COVID_CRONO")
                                                                      
                                                         )
                                                       )
                                          ) 
                                        )
                                        
                               ),
                               # Panel de Hábitat
                               tabPanel("Hábitat", value = "HAB",
                                        icon = icon("city"),
                                        selectInput("HAB_sel", "Elija un indicador", choices = indic_HAB),
                                        # tableOutput("resumen_hab"),
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
                                                         # status = "primary",
                                                         checkIcon = list(
                                                           yes = icon("ok",  style = "color: Coral",
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
                                        )
                               ),
                               # Panel de Movilidad
                               tabPanel("Movilidad", value = "MOV",
                                        icon = icon("bus")
                                        )
                   ),
                 ),
                 # Mapa principal
                 mainPanel(width = 8,
                           leafletOutput("map", height = 600)
                 )
               )
             ),
             fluidRow(
               column(width = 3,
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
      tabPanel("Evolución", icon = icon("line-chart"),
             sidebarLayout(
               sidebarPanel(
                 pickerInput(
                   inputId = "EPH_aglo",
                   label = "Seleccione Aglomerados para mostrar",
                   choices = unique(EPH$I$LAB),
                   options = list(
                     `actions-box` = TRUE,
                     `selected-text-format` = "count > 3"
                   ),
                   multiple = TRUE
                 ),
                 pickerInput(
                   inputId = "EPH_ind",
                   # label = "",
                   choices = indic_EPH,
                   options = list(
                     title = "Elija indicador")
                 )
               ),
               
               mainPanel(
                 plotOutput("EPH_plot", height = 600)
                 
               )
             )
    ),
    tabPanel("Proyecto", icon = icon("question"),
             column(width = 6,
               h1("Acerca del proyecto"),
               HTML("Esta aplicación fue desarrollada en el marco del Proyecto <em>La implementación de políticas públicas para dar respuesta a la crisis desatada por la pandemia COVID-19: Una mirada desde las relaciones intergubernamentales y las redes de políticas</em>. El mismo es parte del grupo de proyectos asociativos de investigación en Ciencias Sociales y Humanas para la generación de conocimientos a partir del estudio de la sociedad argentina (PISAC) en la pandemia y la postpandemia del COVID-19."),
               HTML("<br><br>Los PISACS son impulsados por la Agencia Nacional de Promoción de la Investigación, el Desarrollo Tecnológico y la Innovación (Agencia I+D+i ) y financiados por el Fondo para la Investigación Científica y Tecnológica (FONCYT)."),
               HTML("<br><br>Este proyecto está dirigido por la Dra. María Mercedes Di Virgilio (UBA, IIGG/ CONICET) y se encuentra conformado por 11 grupos de investigación (nodos) de distintas regiones e instituciones de Argentina."),
               HTML("<br><a href = 'http://tripcovidiigg.sociales.uba.ar/'> Saber más</a>"),
               h2("Cómo citar..."),
               HTML("Serrati, P. (2021). <em> Aplicación TRIP-COVID </em> (1.0) [Aplicación Shiny]. Proyecto PISAC TRIP-COVID. <a href= 'https://estedeahora.shinyapps.io/PISAC-COVID/'> https://estedeahora.shinyapps.io/PISAC-COVID/ </a>"),
               HTML("<br> <br>
                    <p>Repositorio GitHub: <a href= 'https://github.com/estedeahora/TRIP-COVID'> https://github.com/estedeahora/TRIP-COVID </a></p>")
             ),
             column(width = 1),
             column(width = 2, 
                    h2("Descargar datos"),
                    HTML("<br>INACTIVO - WIP<br>"),
                    downloadButton("down", label = "Descarga")
             )
             
    )
  )
)

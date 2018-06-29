#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyAce)
library(shinydashboard)
library(shinycssloaders)
library(shinyWidgets)
library(colourpicker)
library(shinyjs)
library(knitr)
library(DT)
library(promises)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(reshape)
library(corrplot)
library(dendextend)
library(scatterplot3d)
library(stringr)

# Define UI for application that draws a histogram
shinyUI(dashboardPage( 
  dashboardHeader(title = tags$a(href="http://promidat.com", 
                                 img(src="Logo2.png", height=55, width="100%", style="padding-top:2px; padding-bottom:6px;"))),
  dashboardSidebar(
    sidebarMenu(id = "principal", 
                tags$div(style="padding-top:10px;"),
                menuItem("Datos", tabName = "cargar", icon = icon("dashboard")),
                menuItem("Estadísticas Básicas", tabName = "parte1", icon = icon("th"),
                         menuSubItem("Resumen Numérico", tabName = "resumen", icon = icon("th")),
                         menuSubItem("Dispersión", tabName = "dispersion", icon = icon("th")),
                         menuSubItem("Distribuciones", tabName = "distribucion", icon = icon("th")),
                         menuSubItem("Correlación", tabName = "correlacion", icon = icon("th"))),
                menuItem("ACP", tabName = "acp", icon = icon("th")),
                menuItem("Clusterización", tabName = "agrupacion", icon = icon("th")),
                menuItem("K-Medias", tabName = "kmedias", icon = icon("th")),
                menuItem("Generar Reporte", tabName = "reporte", icon = icon("th")),
                
                hr(),
                
                radioGroupButtons(
                  inputId = "sel.datos",
                  label = "Seleccionar Datos",
                  choices = c("Original" = "datos.originales", "Modificada" = "datos.modificados"),
                  status = "primary",
                  checkIcon = list(
                    yes = icon("ok", lib = "glyphicon"),
                    no = icon("remove", lib = "glyphicon"))
                ),
                conditionalPanel(
                  condition = "input.principal == 'dispersion'",
                  selectizeInput("select.var", "Seleccionar variables", multiple = T, choices = c(""), options = list(maxItems = 3))
                ),
                conditionalPanel(
                  condition = "input.principal == 'distribucion'",
                  conditionalPanel(
                    condition = "input.tabDyA == 'numericas'",
                    selectInput(inputId = "sel.distribucion.num", label = "Seleccionar Variable", choices =  "", selectize = T)
                  ),
                  conditionalPanel(
                    condition = "input.tabDyA == 'categoricas'",
                    selectInput(inputId = "sel.distribucion.cat", label = "Seleccionar Variable", choices =  "", selectize = T)
                  )
                ),
                conditionalPanel(
                  condition = "input.principal == 'agrupacion'",
                  selectInput(inputId = "cant.cluster", label = "Cantidad de Clusters:", choices =  c(2:10)),
                  conditionalPanel(
                    condition = "input.tabjerar == 'Horizontal'",
                    selectInput(inputId = "sel.cluster", label = "Seleccionar Cluster:", choices =  "")),
                  conditionalPanel(
                    condition = "input.tabjerar == 'Vertical'",
                    selectInput(inputId = "sel.verticales", label = "Seleccionar Variable:", choices =  "")),
                  conditionalPanel(
                    condition = "input.tabjerar == 'Barras'",
                    selectInput(inputId = "sel.cat.var", label = "Seleccionar Variable:", choices =  "")
                  )
                ),
                conditionalPanel(
                  condition = "input.principal == 'kmedias'",
                  selectInput(inputId = "cant.kmeans.cluster", label = "Cantidad de Clusters:", choices =  c(2:10)),
                  conditionalPanel(
                    condition = "input.tabkmedias== 'Horizontal'",
                    selectInput(inputId = "sel.kmeans.cluster", label = "Seleccionar Cluster:", choices =  "")),
                  conditionalPanel(
                    condition = "input.tabkmedias == 'Vertical'",
                    selectInput(inputId = "sel.kmeans.verticales", label = "Seleccionar Variable:", choices =  "")),
                  conditionalPanel(
                    condition = "input.tabkmedias == 'Barras'",
                    selectInput(inputId = "sel.kcat.var", label = "Seleccionar Variable:", choices =  "")
                  )
                )
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style_promidat.css")
    ),
    
    tabItems(
      
      #Carga de Datos
      tabItem(tabName = "cargar",
              column(width = 4,
                     tabBox(title = NULL, width = 12,
                       tabPanel(title = "Cargar", width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                       checkboxInput('header', 'Encabezado (Header)', TRUE),
                       checkboxInput('columname', 'Incluir nombre de filas', TRUE),
                       radioButtons('sep', 'Seperador', c(Coma=',', 'Punto y Coma'=';', Tab='\t'), selected = 'Coma'),
                       radioButtons('dec', 'Separador Decimal', c('Punto'='.', 'Coma'=","), selected = 'Punto'),
                       fileInput('file1', 'Cargar Archivo',
                                 accept = c('text/csv', 'text/comma-separated-values, text/plain', '.csv'), buttonLabel = "Subir",
                                 placeholder = ""),
                       hr(),
                       aceEditor("fieldCodeData", mode = "r", theme = "monokai", value = "", height = "15vh", readOnly = T)
                     ),
                     tabPanel(title = "Transformar", width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                       selectizeInput("trans.var", "Seleccionar variables", multiple = T, choices = c(""), width = "100%"),
                       selectInput(inputId = "tipo.var", label = "Nuevo Tipo:", choices =  c("Numérico", "Categorico"), width = "100%"),
                       actionButton("transButton", "Transformar", width = "100%"),
                       hr(),
                       aceEditor("fieldCodeTrans", mode = "r", theme = "monokai", value = "", height = "20vh",  readOnly = T)
                     )
              )),
              column(width = 8,
                     box(title = "Datos", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                       withSpinner(DT::DTOutput('contents'), type = 7, color = "#CBB051")
              ))
      ),
      
      #Resumen Numérico
      tabItem(tabName = "resumen",
              column(width = 7,
                     box(title = "Resumen Numérico", status = "primary",
                         width = 12, solidHeader = TRUE, collapsible = TRUE, shiny::dataTableOutput("resumen.completo"),
                         aceEditor("fieldCodeResum", mode = "r", theme = "monokai", value = "", height = "8vh", autoComplete = "enabled")
                     )
              ),
              column(width = 5,
                     box(title = "Resumen Numérico por Variable", status = "primary", 
                         width = 12, solidHeader = TRUE, collapsible = TRUE,
                         selectInput(inputId = "sel.resumen", label = h4("Seleccionar Variable:"), choices =  ""),
                         fluidRow(uiOutput("resumen"))
                     )
              )
      ),
      
      #Dispersión
      tabItem(tabName = "dispersion",
              column(width = 4, 
                     dropdownButton(h4("Opciones"),
                                    circle = F, status = "danger", icon = icon("gear"), width = "100%",
                                    tooltip = tooltipOptions(title = "Clic para ver opciones"))),
              plotOutput('plot.disp', height = "82vh"),
              aceEditor("fieldCodeDisp", mode = "r", theme = "monokai", value = "", height = "8vh", autoComplete = "enabled")
      ),
      
      #Correlaciones
      tabItem(tabName = "correlacion",
              fluidRow(
                column(width = 6, 
                       dropdownButton(
                         h4("Opciones"),
                         selectInput(inputId = "cor.metodo", label = "Seleccionar Método", 
                                     choices =  c("circle", "square", "ellipse", "number", "shade", "color", "pie")),
                         circle = F, status = "danger", icon = icon("gear"), width = "300px",
                         tooltip = tooltipOptions(title = "Clic para ver opciones")
              ))),
              #withSpinner(plotOutput('plot.cor', height = "84vh"), type = 7, color = "#CBB051"), 
              plotOutput('plot.cor', height = "80vh"),
              fluidRow(
                column(width = 4, 
                       aceEditor("fieldModelCor", mode = "r", theme = "monokai", value = "", height = "6vh", autoComplete = "enabled")),
                column(width = 8, 
                       aceEditor("fieldCodeCor", mode = "r", theme = "monokai", value = "", height = "6vh", autoComplete = "enabled")))
      ),
      
      #PCA
      tabItem(tabName = "acp",
              column(width = 12,
                     tabBox(id = "tabPCA", title = dropdownButton(h4("Opciones"),
                                                                  conditionalPanel(
                                                                    condition = "input.tabPCA == 'individuos' || input.tabPCA == 'sobreposicion'",
                                                                    colourpicker::colourInput("col.pca.ind", "Seleccionar Color (Individuos):", 
                                                                                              value = "#696969", allowTransparent = T)),
                                                                  conditionalPanel(
                                                                    condition = "input.tabPCA == 'variables' || input.tabPCA == 'sobreposicion'",
                                                                    colourpicker::colourInput("col.pca.var", "Seleccionar Color (Variables):", 
                                                                                              value = "steelblue", allowTransparent = T)),
                                                                  conditionalPanel(
                                                                    condition = "input.tabPCA == 'individuos' || input.tabPCA == 'sobreposicion'",
                                                                    sliderInput("ind.cos", "Coseno de los Individuos: ", min = 0, max = 100, value = 0)
                                                                  ),
                                                                  conditionalPanel(
                                                                    condition = "input.tabPCA == 'variables' || input.tabPCA == 'sobreposicion'",
                                                                    sliderInput("var.cos", "Coseno de las Variables: ", min = 0, max = 100, value = 0)
                                                                  ),
                                                                  circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                                                                  tooltip = tooltipOptions(title = "Clic para ver opciones")), width = NULL,
                            tabPanel(title = 'Individuos', value = "individuos", plotOutput('plot.ind', height = "76vh")),
                            tabPanel(title = 'Variables', value = "variables", plotOutput('plot.var', height = "76vh")),
                            tabPanel(title = 'Sobreposición', value = "sobreposicion", plotOutput('plot.biplot', height = "76vh"))
                     ),
                     column(width = 5, 
                            aceEditor("fieldCodePCAModelo", mode = "r", theme = "monokai", value = "", height = "5vh", readOnly = T, autoComplete = "enabled")),
                     column(width = 7, 
                            conditionalPanel(
                              condition = "input.tabPCA == 'individuos'",
                              aceEditor("fieldCodeInd", mode = "r", theme = "monokai", value = "", height = "5vh", autoComplete = "enabled")
                            ),
                            conditionalPanel(
                              condition = "input.tabPCA == 'variables'",
                              aceEditor("fieldCodeVar", mode = "r", theme = "monokai", value = "", height = "5vh", autoComplete = "enabled")
                            ),
                            conditionalPanel(
                              condition = "input.tabPCA == 'sobreposicion'",
                              aceEditor("fieldCodeBi", mode = "r", theme = "monokai", value = "", height = "5vh", autoComplete = "enabled")
                            )
                     )
              )
      ),
      
      #Distribuciones
      tabItem(tabName = "distribucion",
              column(width = 12,
                     tabBox(id = "tabDyA", 
                            title = fluidRow(
                              column(width = 6, 
                                     dropdownButton(h4("Codigo"),
                                                    h5("Grafico de la Distribución (Numéricas)"),
                                                    aceEditor("fieldFuncNum", mode = "r", theme = "monokai", value = "", height = "20vh", autoComplete = "enabled"),
                                                    h5("Grafico de la Distribución (Categoricas)"),
                                                    aceEditor("fieldFuncCat", mode = "r", theme = "monokai", value = "", height = "20vh", autoComplete = "enabled"),
                                                    circle = F, status = "danger", icon = icon("code"), width = "400px", right = T,
                                                    tooltip = tooltipOptions(title = "Clic para ver el codigo"))),
                              column(width = 5, 
                                     dropdownButton(h4("Opciones"),
                                                    colourpicker::colourInput("col.dist", "Seleccionar Color:", value = "#0D00FFAA", allowTransparent = T),
                                                    circle = F, status = "danger", icon = icon("gear"), width = "100%", right = T,
                                                   tooltip = tooltipOptions(title = "Clic para ver opciones")))), width = 12,
                            tabPanel(title = 'Numéricas', value = "numericas", plotOutput('plot.num', height = "65vh"),
                                     fluidRow(column(width = 6,
                                              aceEditor("fieldCodeNum", mode = "r", theme = "monokai", value = "", height = "15vh", autoComplete = "enabled")),
                                       column(width = 6,
                                              DT::dataTableOutput("mostrar.atipicos")))
                            ),
                            tabPanel(title = 'Categoricas', value = "categoricas", plotOutput('plot.cat', height = "76vh"),
                                     aceEditor("fieldCodeCat", mode = "r", theme = "monokai", value = "", height = "6vh", autoComplete = "enabled")
                            )
                     )
              )
      ),
      
      #Agrupaciones
      tabItem(tabName = "agrupacion",
              column(width = 12,
                     tabBox(id = "tabjerar", title = 
                              fluidRow(
                                column(width = 6, 
                                       dropdownButton(h4("Codigo"),
                                            h5("Calculo de los centros"),
                                            aceEditor("fieldCodeCentr", mode = "r", theme = "monokai", value = "", height = "25vh", autoComplete = "enabled"),
                                            conditionalPanel(
                                              condition = "input.tabjerar == 'Horizontal'",
                                              h5("Grafica todas las variables en Horizontal"),
                                              aceEditor("fieldFuncHoriz", mode = "r", theme = "monokai", value = "", height = "20vh", autoComplete = "enabled")),
                                            conditionalPanel(
                                              condition = "input.tabjerar == 'Vertical'",
                                              h5("Grafica todas las variables en Vertical"),
                                              aceEditor("fieldFuncVert", mode = "r", theme = "monokai", value = "", height = "18vh", autoComplete = "enabled")),
                                            conditionalPanel(
                                              condition = "input.tabjerar == 'Radar'",
                                              h5("Grafica todas las variables en Radar"),
                                              aceEditor("fieldFuncRadar", mode = "r", theme = "monokai", value = "", height = "30vh", autoComplete = "enabled")),
                                            circle = F, status = "danger", icon = icon("code"), width = "400px", right = T,
                                            tooltip = tooltipOptions(title = "Clic para ver el codigo"))),
                                
                                column(width = 5, dropdownButton(h4("Opciones"),
                                             circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                                             tooltip = tooltipOptions(title = "Clic para ver opciones")))), width = 12,
                            tabPanel(title = 'Diagrama', plotOutput('plot.diag', height = "71vh")),
                            tabPanel(title = 'Mapa', plotOutput('plot.mapa', height = "71vh")),
                            tabPanel(title = 'Horizontal', plotOutput('plot.horiz', height = "71vh")),
                            tabPanel(title = 'Vertical', plotOutput('plot.vert', height = "71vh")),
                            tabPanel(title = 'Radar', plotOutput('plot.radar', height = "71vh")),
                            tabPanel(title = 'Barras', plotOutput('plot.bar.cat', height = "71vh"))
                     ), 
                     fluidRow(
                       column(width = 6, 
                              aceEditor("fieldCodeModelo", mode = "r", theme = "monokai", value = "", height = "11vh", readOnly = T, autoComplete = "enabled")
                       ),
                       column(width = 6, 
                              conditionalPanel(
                                condition = "input.tabjerar == 'Diagrama'",
                                aceEditor("fieldCodeDiag", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled")),
                              conditionalPanel(
                                condition = "input.tabjerar == 'Mapa'",
                                aceEditor("fieldCodeMapa", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled")),
                              conditionalPanel(
                                condition = "input.tabjerar == 'Horizontal'",
                                aceEditor("fieldCodeHoriz", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled")),
                              conditionalPanel(
                                condition = "input.tabjerar == 'Vertical'",
                                aceEditor("fieldCodeVert", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled")),
                              conditionalPanel(
                                condition = "input.tabjerar == 'Radar'",
                                aceEditor("fieldCodeRadar", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled")),
                              conditionalPanel(
                                condition = "input.tabjerar == 'Barras'",
                                aceEditor("fieldCodeBarras", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled"))
                       ))
              )
      ),
      
      #K-means
      tabItem(tabName = "kmedias",
              column(width = 12,
                     tabBox(id = "tabkmedias", title = 
                              fluidRow(
                                column(width = 6, 
                                       dropdownButton(h4("Codigo"),
                                                      conditionalPanel(
                                                        condition = "input.tabkmedias == 'codoJambu'",
                                                        h5("Calculo del Codo de Jambu"),
                                                        aceEditor("fieldFuncJambu", mode = "r", theme = "monokai", value = "", height = "40vh", autoComplete = "enabled")),
                                                      conditionalPanel(
                                                        condition = "input.tabkmedias == 'Horizontal'",
                                                        h5("Grafica todas las variables en Horizontal"),
                                                        aceEditor("fieldFuncKhoriz", mode = "r", theme = "monokai", value = "", height = "25vh", autoComplete = "enabled")),
                                                      conditionalPanel(
                                                        condition = "input.tabkmedias == 'Vertical'",
                                                        h5("Grafica todas las variables en Vertical"),
                                                        aceEditor("fieldFuncKvert", mode = "r", theme = "monokai", value = "", height = "25vh", autoComplete = "enabled")),
                                                      conditionalPanel(
                                                        condition = "input.tabkmedias == 'Radar'",
                                                        h5("Grafica todas las variables en Radar"),
                                                        aceEditor("fieldFuncKradar", mode = "r", theme = "monokai", value = "", height = "40vh", autoComplete = "enabled")),
                                                      circle = F, status = "danger", icon = icon("code"), width = "400px", right = T,
                                                      tooltip = tooltipOptions(title = "Clic para ver el codigo"))),
                                
                                column(width = 5, dropdownButton(h4("Opciones"),
                                                      circle = F, status = "danger", icon = icon("gear"), width = "100%", right = T,
                                                      tooltip = tooltipOptions(title = "Clic para ver opciones")))), width = 12,
                            
                            tabPanel(title = 'Inercia', fluidRow(uiOutput('resumen.kmedias'))),
                            tabPanel(title = 'Codo Jambu', value = "codoJambu", plotOutput('plot.jambu', height = "71vh")),
                            tabPanel(title = 'Mapa', plotOutput('plot.kmapa', height = "71vh")),
                            tabPanel(title = 'Horizontal', plotOutput('plot.khoriz', height = "71vh")),
                            tabPanel(title = 'Vertical', plotOutput('plot.kvert', height = "71vh")),
                            tabPanel(title = 'Radar', plotOutput('plot.kradar', height = "71vh")),
                            tabPanel(title = 'Barras', plotOutput('plot.kcat', height = "71vh"))
                     ), 
                     fluidRow(
                       column(width = 6, 
                              aceEditor("fieldCodeKModelo", mode = "r", theme = "monokai", value = "", height = "11vh", readOnly = T, autoComplete = "enabled")),
                       column(width = 6, 
                              conditionalPanel(
                                condition = "input.tabkmedias == 'codoJambu'",
                                aceEditor("fieldCodeJambu", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled")),
                              conditionalPanel(
                                condition = "input.tabkmedias == 'Mapa'",
                                aceEditor("fieldCodeKmapa", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled")),
                              conditionalPanel(
                                condition = "input.tabkmedias == 'Horizontal'",
                                aceEditor("fieldCodeKhoriz", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled")),
                              conditionalPanel(
                                condition = "input.tabkmedias == 'Vertical'",
                                aceEditor("fieldCodeKvert", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled")),
                              conditionalPanel(
                                condition = "input.tabkmedias == 'Radar'",
                                aceEditor("fieldCodeKradar", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled")),
                              conditionalPanel(
                                condition = "input.tabkmedias == 'Barras'",
                                aceEditor("fieldCodeKbarras", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled"))
                     ))
              )
      ),
      
      #Generar Reporte
      tabItem(tabName = "reporte", 
              column(width = 5, 
                     tabBox(width = 12, id = "tabReporte", 
                            tabPanel(title = "Reporte", width = 12), 
                            tabPanel(title = "Codigo", width = 12, aceEditor("fieldCodeReport", mode="markdown", value=''))), 
                     downloadButton("descargar", "Descargar", style = "position: relative; left: 40%")),
              column(width = 7, 
                     box(title = "Vista Previa", width = 12, height = "90vh", status = "primary", solidHeader = TRUE,
                         collapsible = TRUE, div(style = 'overflow-x: scroll; overflow-y: scroll; height: 80vh;', htmlOutput("knitDoc")))))
    ) #tabItems
  ) #dashboardBody
)) #UI

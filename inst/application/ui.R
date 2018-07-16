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
library(future)
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
shinyUI(dashboardPage(title="PROMiDAT",
  dashboardHeader(title = tags$a(href="http://promidat.com",
                                 img(src="Logo2.png", height=55, width="100%", style="padding-top:2px; padding-bottom:6px;"))),
  dashboardSidebar(
    sidebarMenu(id = "principal",
                tags$div(style="padding-top:10px;"),
                menuItem("Datos", tabName = "cargar", icon = icon("dashboard")),
                menuItem("Estadísticas Básicas", tabName = "parte1", icon = icon("th"),
                         menuSubItem("Resumen Numérico", tabName = "resumen", icon = icon("th")),
                         menuSubItem("Test de Normalidad", tabName = "normalidad", icon = icon("th")),
                         menuSubItem("Dispersión", tabName = "dispersion", icon = icon("th")),
                         menuSubItem("Distribuciones", tabName = "distribucion", icon = icon("th")),
                         menuSubItem("Correlación", tabName = "correlacion", icon = icon("th"))),
                menuItem("ACP", tabName = "acp", icon = icon("th")),
                menuItem("Cluster Jerárquico", tabName = "agrupacion", icon = icon("th")),
                menuItem("K-Medias", tabName = "kmedias", icon = icon("th")),
                menuItem("Generar Reporte", tabName = "reporte", icon = icon("th"))
    )
  ),

  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style_promidat.css"),
      tags$link(rel = "icon", type = "image", href = "http://www.promidat.org/theme/image.php/formal_white/theme/1438713216/favicon"),
      useShinyjs()
    ),

    tabItems(

      #Carga de Datos
      tabItem(tabName = "cargar",
              column(width = 5,
                     tabBox(title = NULL, width = 12,
                       tabPanel(title = "Cargar", width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                       checkboxInput('header', 'Encabezado (Header)', TRUE),
                       checkboxInput('rowname', 'Incluir nombre de filas', TRUE),
                       radioButtons('sep', 'Seperador', c(Coma=',', 'Punto y Coma'=';', Tab='\t'), selected = 'Coma'),
                       radioButtons('dec', 'Separador Decimal', c('Punto'='.', 'Coma'=","), selected = 'Punto'),
                       fileInput('file1', label = 'Cargar Archivo', placeholder = "", buttonLabel = "Subir", width = "100%",
                                 accept = c('text/csv', 'text/comma-separated-values, text/plain', '.csv')),
                       actionButton("loadButton", "Cargar", width = "100%"),
                       hr(),
                       aceEditor("fieldCodeData", mode = "r", theme = "monokai", value = "", height = "15vh", readOnly = T)
                     ),
                     tabPanel(title = "Transformar", width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                       DT::dataTableOutput('transData'),
                       actionButton("transButton", "Aplicar", width = "100%"),
                       hr(),
                       aceEditor("fieldCodeTrans", mode = "r", theme = "monokai", value = "", height = "10vh",  readOnly = T)
                     )
              )),
              column(width = 7,
                     box(title = "Datos", status = "primary", width = 12, solidHeader = TRUE, collapsible = TRUE,
                       withSpinner(DT::DTOutput('contents'), type = 7, color = "#CBB051")
              ))
      ),

      #Resumen Numérico
      tabItem(tabName = "resumen",
              column(width = 7,
                     box(title = "Resumen Numérico", status = "primary",
                         width = 12, solidHeader = TRUE, collapsible = TRUE, DT::dataTableOutput("resumen.completo"), hr(),
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

      #test de Normalidad
      tabItem(tabName = "normalidad",
              column(width = 12,
                     tabBox(id = "BoxNormal", width = NULL, title =
                              fluidRow(
                                column(width = 9,
                                       selectInput(inputId = "sel.normal", label = NULL, choices =  "")),
                                column(width = 3, dropdownButton(h4("Opciones"),
                                                                 colourpicker::colourInput("col.normal", "Seleccionar Color:",
                                                                                           value = "#00FF22AA", allowTransparent = T),
                                                                 circle = F, status = "danger", icon = icon("gear"), width = "100%",
                                                                 tooltip = tooltipOptions(title = "Clic para ver opciones"), right = T))),
                            tabPanel(title = "Test de Normalidad", value = "tabNormal", plotOutput('plot.normal', height = "72vh")))),
              aceEditor("fieldCodeNormal", mode = "r", theme = "monokai", value = "", height = "8vh", autoComplete = "enabled")
      ),

      #Dispersión
      tabItem(tabName = "dispersion",
              column(width = 12,
                     tabBox(id = "BoxDisp", width = NULL, title =
                              fluidRow(
                                column(width = 9,
                                       tags$div(class="select-var-ind",
                                                selectizeInput("select.var", NULL, multiple = T, choices = c(""),
                                                               options = list(maxItems = 3, placeholder = "Seleccione la(s) variable(s)")))),
                                column(width = 3,dropdownButton(h4("Opciones"),
                                                                colourpicker::colourInput("col.disp", "Seleccionar Color:",
                                                                                          value = "#FF0000AA", allowTransparent = T),
                                                                circle = F, status = "danger", icon = icon("gear"), width = "100%",
                                                                tooltip = tooltipOptions(title = "Clic para ver opciones"), right = T))),
                     tabPanel(title = "Dispersión", value = "tabDisp", plotOutput('plot.disp', height = "72vh"))
              )),
              aceEditor("fieldCodeDisp", mode = "r", theme = "monokai", value = "", height = "8vh", autoComplete = "enabled")
      ),

      #Correlaciones
      tabItem(tabName = "correlacion",
              column(width = 12,
                     tabBox(id = "tabCor", width = NULL, title =
                            dropdownButton(h4("Opciones"),
                                           selectInput(inputId = "cor.metodo", label = "Seleccionar Método",
                                                       choices =  c("circle", "square", "ellipse", "number", "shade", "color", "pie")),
                                           selectInput(inputId = "cor.tipo", label = "Seleccionar Tipo", choices =  c("lower", "upper", "full")),
                                           circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                                           tooltip = tooltipOptions(title = "Clic para ver opciones")),
                            #withSpinner(plotOutput('plot.cor', height = "84vh"), type = 7, color = "#CBB051"),
                            tabPanel(title = 'Correlación', value = "correlacion", plotOutput('plot.cor', height = "76vh"),
                                     fluidRow(column(width = 4, aceEditor("fieldModelCor", mode = "r", theme = "monokai", value = "",
                                                                          height = "6vh", autoComplete = "enabled")),
                                              column(width = 8, aceEditor("fieldCodeCor", mode = "r", theme = "monokai", value = "",
                                                                          height = "6vh", autoComplete = "enabled")))),
                            tabPanel(title = 'Resultados Numéricos', value = "cor.salida", verbatimTextOutput("txtcor")))
              )
      ),

      #PCA
      tabItem(tabName = "acp",
              column(width = 12,
                     tabBox(id = "tabPCA", width = NULL, title =
                              dropdownButton(h4("Opciones"),
                                             switchInput(inputId = "switch.scale", onStatus = "success", offStatus = "danger", value = T,
                                                         label = "Centrar y Reducir", onLabel = "SI", offLabel = "NO", labelWidth = "100%"),
                                             sliderInput("slider.npc", "Número de Dimensiones: ", min = 2, max = 10, value = 5),
                                             conditionalPanel(condition = "input.tabPCA == 'individuos' || input.tabPCA == 'sobreposicion'",
                                               colourpicker::colourInput("col.pca.ind", "Seleccionar Color (Individuos):",
                                                                         value = "#696969", allowTransparent = T)
                                             ),
                                             conditionalPanel(condition = "input.tabPCA == 'variables' || input.tabPCA == 'sobreposicion'",
                                               colourpicker::colourInput("col.pca.var", "Seleccionar Color (Variables):",
                                                                         value = "steelblue", allowTransparent = T)
                                             ),
                                             conditionalPanel(condition = "input.tabPCA == 'individuos' || input.tabPCA == 'sobreposicion'",
                                               sliderInput("ind.cos", "Coseno de los Individuos: ", min = 0, max = 100, value = 0)
                                             ),
                                             conditionalPanel(condition = "input.tabPCA == 'variables' || input.tabPCA == 'sobreposicion'",
                                               sliderInput("var.cos", "Coseno de las Variables: ", min = 0, max = 100, value = 0)
                                             ),
                                             circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                                             tooltip = tooltipOptions(title = "Clic para ver opciones")),
                            tabPanel(title = 'Individuos', value = "individuos", plotOutput('plot.ind', height = "76vh")),
                            tabPanel(title = 'Variables', value = "variables", plotOutput('plot.var', height = "76vh")),
                            tabPanel(title = 'Sobreposición', value = "sobreposicion", plotOutput('plot.biplot', height = "76vh")),
                            navbarMenu("Ayuda Interpretación",
                                       tabPanel("Varianza Explicada por cada eje", value = "tabVarExplicada",
                                                plotOutput("plotVEE", height = "76vh")),
                                       tabPanel("Cosenos cuadrados de los individuos", value = "tabIndCos",
                                                plotOutput("plotCCI", height = "76vh")),
                                       tabPanel("Cosenos cuadrados de las variables", value = "tabVarCos",
                                                plotOutput("plotCCV", height = "76vh")),
                                       tabPanel("Correlación variables-componentes", value = "tabCorVarComp",
                                                plotOutput("plotCVC", height = "76vh")),
                                       tabPanel("Contribución de las Variables Dim-1", value = "tabVarDim1",
                                                plotOutput("plotCP1", height = "76vh")),
                                       tabPanel("Contribución de las Variables Dim-2", value = "tabVarDim2",
                                                plotOutput("plotCP2", height = "76vh"))),
                            tabPanel(title = "Resultados Numéricos", value = "pca.salida", verbatimTextOutput("txtpca"))
                     ),
                     column(width = 5,
                            aceEditor("fieldCodePCAModelo", mode = "r", theme = "monokai", value = "",
                                      height = "5vh", readOnly = T, autoComplete = "enabled")),
                     column(width = 7,
                            conditionalPanel(
                              condition = "input.tabPCA == 'individuos'",
                              aceEditor("fieldCodeInd", mode = "r", theme = "monokai", value = "", height = "5vh", autoComplete = "enabled")),
                            conditionalPanel(
                              condition = "input.tabPCA == 'variables'",
                              aceEditor("fieldCodeVar", mode = "r", theme = "monokai", value = "", height = "5vh", autoComplete = "enabled")),
                            conditionalPanel(
                              condition = "input.tabPCA == 'sobreposicion'",
                              aceEditor("fieldCodeBi", mode = "r", theme = "monokai", value = "", height = "5vh", autoComplete = "enabled")),
                            conditionalPanel(
                              condition = "input.tabPCA == 'tabVarExplicada' || input.tabPCA == 'tabIndCos' || input.tabPCA == 'tabVarCos' ||
                                           input.tabPCA == 'tabCorVarComp' || input.tabPCA == 'tabVarDim1' || input.tabPCA == 'tabVarDim2'",
                              aceEditor("fieldCodeAyuda", mode = "r", theme = "monokai", value = "", height = "5vh", readOnly = T))
                     )
              )
      ),

      #Distribuciones
      tabItem(tabName = "distribucion",
              column(width = 12,
                     tabBox(id = "tabDyA",
                            title = fluidRow(
                              column(width = 7,tags$div(class = "select-var-ind",
                                                        conditionalPanel(
                                                          condition = "input.tabDyA == 'numericas'",
                                                          selectInput(inputId = "sel.distribucion.num", label = NULL, choices =  "")
                                                        ),
                                                        conditionalPanel(
                                                          condition = "input.tabDyA == 'categoricas'",
                                                          selectInput(inputId = "sel.distribucion.cat", label = NULL, choices =  "")
                                                        ))),
                              column(width = 2,
                                     dropdownButton(h4("Código"),
                                                    h5("Grafico de la Distribución (Numéricas)"),
                                                    aceEditor("fieldFuncNum", mode = "r", theme = "monokai", value = "",
                                                              height = "20vh", autoComplete = "enabled"),
                                                    h5("Grafico de la Distribución (Categóricas)"),
                                                    aceEditor("fieldFuncCat", mode = "r", theme = "monokai", value = "",
                                                              height = "20vh", autoComplete = "enabled"),
                                                    circle = F, status = "danger", icon = icon("code"), width = "400px", right = T,
                                                    tooltip = tooltipOptions(title = "Clic para ver el código"))),
                              column(width = 2,
                                     dropdownButton(h4("Opciones"),
                                                    colourpicker::colourInput("col.dist", "Seleccionar Color:",
                                                                              value = "#0D00FFAA", allowTransparent = T),
                                                    circle = F, status = "danger", icon = icon("gear"), width = "100%", right = T,
                                                   tooltip = tooltipOptions(title = "Clic para ver opciones")))), width = 12,
                            tabPanel(title = 'Numéricas', value = "numericas", plotOutput('plot.num', height = "65vh"),
                                     fluidRow(column(width = 6,
                                              aceEditor("fieldCodeNum", mode = "r", theme = "monokai",
                                                        value = "", height = "15vh", autoComplete = "enabled")),
                                       column(width = 6,
                                              DT::dataTableOutput("mostrar.atipicos")))
                            ),
                            tabPanel(title = 'Categóricas', value = "categoricas", plotOutput('plot.cat', height = "76vh"),
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
                                conditionalPanel(
                                  condition = "input.tabjerar == 'Horizontal'",
                                  tags$div(class = "multiple-select-var", selectInput(inputId = "sel.cluster", label = NULL, choices =  ""))),
                                conditionalPanel(
                                  condition = "input.tabjerar == 'Vertical'",
                                  tags$div(class = "multiple-select-var", selectInput(inputId = "sel.verticales", label = NULL, choices =  ""))),
                                conditionalPanel(
                                  condition = "input.tabjerar == 'hcbarras'",
                                  tags$div(class = "multiple-select-var", selectInput(inputId = "sel.cat.var", label = NULL, choices =  ""))
                                ),
                                tags$div(class = "option-var-ind", dropdownButton(h4("Código"),
                                      h5("Calculo de los centros"),
                                      aceEditor("fieldCodeCentr", mode = "r", theme = "monokai",
                                                value = "", height = "25vh", autoComplete = "enabled"),
                                      conditionalPanel(
                                        condition = "input.tabjerar == 'Horizontal'",
                                        h5("Grafica todas las variables en Horizontal"),
                                        aceEditor("fieldFuncHoriz", mode = "r", theme = "monokai",
                                                  value = "", height = "20vh", autoComplete = "enabled")),
                                      conditionalPanel(
                                        condition = "input.tabjerar == 'Vertical'",
                                        h5("Grafica todas las variables en Vertical"),
                                        aceEditor("fieldFuncVert", mode = "r", theme = "monokai",
                                                  value = "", height = "18vh", autoComplete = "enabled")),
                                      conditionalPanel(
                                        condition = "input.tabjerar == 'Radar'",
                                        h5("Grafica todas las variables en Radar"),
                                        aceEditor("fieldFuncRadar", mode = "r", theme = "monokai",
                                                  value = "", height = "30vh", autoComplete = "enabled")),
                                      circle = F, status = "danger", icon = icon("code"), width = "400px", right = T,
                                      tooltip = tooltipOptions(title = "Clic para ver el código"))),

                                tags$div(class = "option-var-ind", dropdownButton(h4("Opciones"),
                                          sliderInput(inputId = "cant.cluster", label = "Cantidad de Clusters:", min = 2, max = 10, value = 2),
                                          selectInput(inputId = "sel.hc.method", label = "Método Jerarquico", selectize = T,
                                                      choices =  c("ward.D2", "single", "complete", "average")),
                                          selectInput(inputId = "sel.dist.method", label = "Método para la Distancia", selectize = T,
                                                      choices =  c("euclidean", "maximum", "manhattan", "canberra", "binary", "minkowski")),
                                          HTML("<label class='control-label'>Seleccionar Colores: </label>"),
                                          fluidRow(
                                            shiny::column(width = 2, colourpicker::colourInput("hcColor1",
                                                                                               NULL, value = "#F8766D", allowTransparent = T)),
                                            shiny::column(width = 2, colourpicker::colourInput("hcColor2",
                                                                                               NULL, value = "#00BFC4", allowTransparent = T)),
                                            shiny::column(width = 2, colourpicker::colourInput("hcColor3",
                                                                                               NULL, value = "#00BA38", allowTransparent = T)),
                                            shiny::column(width = 2, colourpicker::colourInput("hcColor4",
                                                                                               NULL, value = "#C77CFF", allowTransparent = T)),
                                            shiny::column(width = 2, colourpicker::colourInput("hcColor5",
                                                                                               NULL, value = "#00B0F6", allowTransparent = T)),
                                            shiny::column(width = 2, colourpicker::colourInput("hcColor6",
                                                                                               NULL, value = "#EEEE00", allowTransparent = T)),
                                            shiny::column(width = 2, colourpicker::colourInput("hcColor7",
                                                                                               NULL, value = "#CD661D", allowTransparent = T)),
                                            shiny::column(width = 2, colourpicker::colourInput("hcColor8",
                                                                                               NULL, value = "#006400", allowTransparent = T)),
                                            shiny::column(width = 2, colourpicker::colourInput("hcColor9",
                                                                                               NULL, value = "#EE82EE", allowTransparent = T)),
                                            shiny::column(width = 2, colourpicker::colourInput("hcColor10",
                                                                                               NULL, value = "#000080", allowTransparent = T))),
                                          circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                                          tooltip = tooltipOptions(title = "Clic para ver opciones")))), width = 12,
                            tabPanel(title = 'Diagrama', plotOutput('plot.diag', height = "71vh")),
                            tabPanel(title = 'Mapa', plotOutput('plot.mapa', height = "71vh")),
                            tabPanel(title = 'Horizontal', plotOutput('plot.horiz', height = "71vh")),
                            tabPanel(title = 'Vertical', plotOutput('plot.vert', height = "71vh")),
                            tabPanel(title = 'Radar', plotOutput('plot.radar', height = "71vh")),
                            tabPanel(title = 'Interpretación Categórico', value = "hcbarras", plotOutput('plot.bar.cat', height = "71vh")),
                            tabPanel(title = 'Resultados Numéricos', value = "salida.hc", verbatimTextOutput("txthc"),
                                     hr(), verbatimTextOutput("txtcentros"))
                     ),
                     fluidRow(
                       column(width = 6,
                              aceEditor("fieldCodeModelo", mode = "r", theme = "monokai",
                                        value = "", height = "11vh", readOnly = T, autoComplete = "enabled")),
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
                                condition = "input.tabjerar == 'hcbarras'",
                                aceEditor("fieldCodeBarras", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled"))
                       ))
              )
      ),

      #K-means
      tabItem(tabName = "kmedias",
              column(width = 12,
                     tabBox(id = "tabkmedias", width = 12, title =
                              fluidRow(
                                conditionalPanel(
                                  condition = "input.tabkmedias== 'Horizontal'",
                                  tags$div(class = "multiple-select-var", selectInput(inputId = "sel.kmeans.cluster", label = NULL, choices = ""))),
                                conditionalPanel(
                                  condition = "input.tabkmedias == 'Vertical'",
                                  tags$div(class = "multiple-select-var", selectInput(inputId = "sel.kmeans.verticales", label = NULL, choices = ""))),
                                conditionalPanel(
                                  condition = "input.tabkmedias == 'kbarras'",
                                  tags$div(class = "multiple-select-var", selectInput(inputId = "sel.kcat.var", label = NULL, choices = ""))),
                                tags$div(class = "option-var-ind",
                                         dropdownButton(h4("Código"),
                                           conditionalPanel(
                                             condition = "input.tabkmedias == 'codoJambu'",
                                             h5("Calculo del Codo de Jambu"),
                                             aceEditor("fieldFuncJambu", mode = "r", theme = "monokai",
                                                       value = "", height = "40vh", autoComplete = "enabled")),
                                           conditionalPanel(
                                             condition = "input.tabkmedias == 'Horizontal'",
                                             h5("Grafica todas las variables en Horizontal"),
                                             aceEditor("fieldFuncKhoriz", mode = "r", theme = "monokai",
                                                       value = "", height = "25vh", autoComplete = "enabled")),
                                           conditionalPanel(
                                             condition = "input.tabkmedias == 'Vertical'",
                                             h5("Grafica todas las variables en Vertical"),
                                             aceEditor("fieldFuncKvert", mode = "r", theme = "monokai",
                                                       value = "", height = "25vh", autoComplete = "enabled")),
                                           conditionalPanel(
                                             condition = "input.tabkmedias == 'Radar'",
                                             h5("Grafica todas las variables en Radar"),
                                             aceEditor("fieldFuncKradar", mode = "r", theme = "monokai",
                                                       value = "", height = "40vh", autoComplete = "enabled")),
                                           circle = F, status = "danger", icon = icon("code"), width = "400px", right = T,
                                           tooltip = tooltipOptions(title = "Clic para ver el código"))),
                                tags$div(class = "option-var-ind",
                                         dropdownButton(h4("Opciones"),
                                                        sliderInput(inputId = "cant.kmeans.cluster", min = 2, value = 2,
                                                                    label = "Cantidad de Clusters:", max = 10),
                                                        sliderInput("slider.nstart", "Cantidad de Centros al azar",
                                                                    value = 100, step = 10, min = 10, max = 100),
                                                        numericInput("num.iter", label = "Número de Iteraciones", step = 100, value = 100),
                                                        HTML("<label class='control-label'>Seleccionar Colores: </label>"),
                                                        fluidRow(
                                                          shiny::column(width = 2, colourpicker::colourInput("kColor1",
                                                                                            NULL, value = "#F8766D", allowTransparent = T)),
                                                          shiny::column(width = 2, colourpicker::colourInput("kColor2",
                                                                                            NULL, value = "#00BFC4", allowTransparent = T)),
                                                          shiny::column(width = 2, colourpicker::colourInput("kColor3",
                                                                                            NULL, value = "#00BA38", allowTransparent = T)),
                                                          shiny::column(width = 2, colourpicker::colourInput("kColor4",
                                                                                            NULL, value = "#C77CFF", allowTransparent = T)),
                                                          shiny::column(width = 2, colourpicker::colourInput("kColor5",
                                                                                            NULL, value = "#00B0F6", allowTransparent = T)),
                                                          shiny::column(width = 2, colourpicker::colourInput("kColor6",
                                                                                            NULL, value = "#EEEE00", allowTransparent = T)),
                                                          shiny::column(width = 2, colourpicker::colourInput("kColor7",
                                                                                            NULL, value = "#CD661D", allowTransparent = T)),
                                                          shiny::column(width = 2, colourpicker::colourInput("kColor8",
                                                                                            NULL, value = "#006400", allowTransparent = T)),
                                                          shiny::column(width = 2, colourpicker::colourInput("kColor9",
                                                                                            NULL, value = "#EE82EE", allowTransparent = T)),
                                                          shiny::column(width = 2, colourpicker::colourInput("kColor10",
                                                                                            NULL, value = "#000080", allowTransparent = T))),
                                                        circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                                                        tooltip = tooltipOptions(title = "Clic para ver opciones")))),
                            tabPanel(title = 'Inercia', fluidRow(uiOutput('resumen.kmedias'))),
                            tabPanel(title = 'Codo Jambu', value = "codoJambu", plotOutput('plot.jambu', height = "71vh")),
                            tabPanel(title = 'Mapa', plotOutput('plot.kmapa', height = "71vh")),
                            tabPanel(title = 'Horizontal', plotOutput('plot.khoriz', height = "71vh")),
                            tabPanel(title = 'Vertical', plotOutput('plot.kvert', height = "71vh")),
                            tabPanel(title = 'Radar', plotOutput('plot.kradar', height = "71vh")),
                            tabPanel(title = 'Interpretación Categórico', value = "kbarras", plotOutput('plot.kcat', height = "71vh")),
                            tabPanel(title = 'Resultados Numéricos', value = "salida.k", verbatimTextOutput("txtk"))
                     ),
                     fluidRow(
                       column(width = 6,
                              aceEditor("fieldCodeKModelo", mode = "r", theme = "monokai",
                                        value = "", height = "11vh", readOnly = T, autoComplete = "enabled")),
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
                                condition = "input.tabkmedias == 'kbarras'",
                                aceEditor("fieldCodeKbarras", mode = "r", theme = "monokai", value = "", height = "11vh", autoComplete = "enabled"))
                     ))
              )
      ),

      #Generar Reporte
      tabItem(tabName = "reporte",
              column(width = 5,
                     tabBox(width = 12, id = "tabReporte",
                            tabPanel(title = "Reporte", width = 12),
                            tabPanel(title = "Código", width = 12, aceEditor("fieldCodeReport", mode="markdown", value=''))),
                     column(width = 8, actionButton("btnReporte", "Actualizar Reporte")),
                     column(width = 4, downloadButton("descargar", "Descargar"))),
              column(width = 7,
                     box(title = "Vista Previa", width = 12, height = "90vh", status = "primary", solidHeader = TRUE,
                         collapsible = TRUE, div(style = 'overflow-x: scroll; overflow-y: scroll; height: 80vh;',
                                                 withSpinner(htmlOutput("knitDoc"), type = 7, color = "#CBB051")))))
    ) #tabItems
  ) #dashboardBody
)) #UI

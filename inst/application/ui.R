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
library(stringr)
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
library(modeest)
library(stringr)

cod.deshabilitar <-
  'shinyjs.init = function() {
$(".sidebar").on("click", ".disabled", function (e) {
e.preventDefault();
return false;
});
}'

# Define UI for application that draws a histogram
shinyUI(dashboardPage(title="PROMiDAT",
  dashboardHeader(title = tags$a(href="http://promidat.com",
                                 img(src="Logo2.png", height=55, width="100%", style="padding-top:2px; padding-bottom:6px;"))),
  dashboardSidebar(
    sidebarMenu(id = "principal",
                tags$div(style="padding-top:10px;"),
                menuItem("Datos", tabName = "cargar", icon = icon("dashboard")),
                menuItem("Estadísticas Básicas", tabName = "parte1", icon = icon("th-list"),
                         menuSubItem("Resumen Numérico", tabName = "resumen", icon = icon("sort-numeric-asc")),
                         menuSubItem("Test de Normalidad", tabName = "normalidad", icon = icon("bar-chart")),
                         menuSubItem("Dispersión", tabName = "dispersion", icon = icon("line-chart")),
                         menuSubItem("Distribuciones", tabName = "distribucion", icon = icon("area-chart")),
                         menuSubItem("Correlación", tabName = "correlacion", icon = icon("table"))),
                menuItem("ACP", tabName = "acp", icon = icon("pie-chart")),
                menuItem("Cluster Jerárquico", tabName = "agrupacion", icon = icon("sitemap")),
                menuItem("K-Medias", tabName = "kmedias", icon = icon("object-group")),
                menuItem("Generar Reporte", tabName = "reporte", icon = icon("save-file", lib = "glyphicon")),
                menuItem("Acerca De", tabName = "acercaDe", icon = icon("info"))
    )
  ),

  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style_promidat.css"),
      tags$link(rel = "icon", type = "image", href = "http://www.promidat.org/theme/image.php/formal_white/theme/1438713216/favicon"),
      useShinyjs(),
      extendShinyjs(text = cod.deshabilitar, functions = "init")
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
                       switchInput(inputId = "deleteNA", onStatus = "success", offStatus = "danger", value = T, width = "100%",
                                   label = "Eliminar NA", onLabel = "SI", offLabel = "NO", labelWidth = "100%"),
                       fileInput('file1', label = 'Cargar Archivo', placeholder = "", buttonLabel = "Subir", width = "100%",
                                 accept = c('text/csv', 'text/comma-separated-values, text/plain', '.csv')),
                       actionButton("loadButton", "Cargar", width = "100%"),
                       hr(),
                       aceEditor("fieldCodeData", mode = "r", theme = "monokai", value = "", height = "15vh", readOnly = T)
                     ),
                     tabPanel(title = "Transformar", width = 12, solidHeader = FALSE, collapsible = FALSE, collapsed = FALSE,
                       DT::dataTableOutput('transData'),
                       hr(),
                       actionButton("transButton", "Aplicar", width = "100%"),
                       hr(),
                       aceEditor("fieldCodeTrans", mode = "r", theme = "monokai", value = "", height = "10vh", readOnly = T)
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
                         aceEditor("fieldCodeResum", mode = "r", theme = "monokai", value = "", height = "8vh", readOnly = T)
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
                            tabPanel(title = "Gráfico Normalidad", value = "tabNormalPlot", plotOutput('plot.normal', height = "65vh")),
                            tabPanel(title = "Test de Normalidad", value = "tabNormalCalc", DT::dataTableOutput('calculo.normal')))),
              conditionalPanel(
                "input.BoxNormal == 'tabNormalPlot'",
                column(width = 12, campo.codigo("run.normal", "ref.normal", "fieldCodeNormal", height = "8vh"))
              ),
              conditionalPanel(
                "input.BoxNormal == 'tabNormalCalc'",
                column(width = 12, campo.codigo("run.calc.normal", "ref.calc.normal", "fieldCalcNormal", height = "8vh"))
              )
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
                     tabPanel(title = "Dispersión", value = "tabDisp", plotOutput('plot.disp', height = "65vh"))
              )),
              column(width = 12, campo.codigo("run.disp", "ref.disp", "fieldCodeDisp", height = "8vh"))
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
                            tabPanel(title = 'Correlación', value = "correlacion", plotOutput('plot.cor', height = "67vh"),
                                     fluidRow(column(width = 4, aceEditor("fieldModelCor", height = "6vh", mode = "r",
                                                                          theme = "monokai", value = "", readOnly = T)),
                                              column(width = 8, campo.codigo("run.code.cor", "ref.code.cor", "fieldCodeCor", height = "6vh")))),
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
                                             sliderTextInput("slider.ejes", "Seleccionar Ejes:", choices = c(1:10), selected = c(1,2), grid = T),
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
                                             conditionalPanel(
                                               condition = "input.tabPCA == 'tabCorVarComp'",
                                               selectInput(inputId = "cvc.metodo", label = "Seleccionar Método",
                                                           choices =  c("circle", "square", "ellipse", "number", "shade", "color", "pie"))
                                             ),
                                             circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                                             tooltip = tooltipOptions(title = "Clic para ver opciones")),
                            tabPanel(title = 'Individuos', value = "individuos", plotOutput('plot.ind', height = "70vh")),
                            tabPanel(title = 'Variables', value = "variables", plotOutput('plot.var', height = "70vh")),
                            tabPanel(title = 'Sobreposición', value = "sobreposicion", plotOutput('plot.biplot', height = "70vh")),
                            navbarMenu("Ayuda Interpretación",
                                       tabPanel("Varianza Explicada por cada eje", value = "tabVarExplicada",
                                                plotOutput("plotVEE", height = "70vh")),
                                       tabPanel("Cosenos cuadrados de los individuos", value = "tabIndCos",
                                                plotOutput("plotCCI", height = "70vh")),
                                       tabPanel("Cosenos cuadrados de las variables", value = "tabVarCos",
                                                plotOutput("plotCCV", height = "70vh")),
                                       tabPanel("Correlación variables-componentes", value = "tabCorVarComp",
                                                plotOutput("plotCVC", height = "70vh")),
                                       tabPanel("Contribución de las Variables Dim-1", value = "tabVarDim1",
                                                plotOutput("plotCP1", height = "70vh")),
                                       tabPanel("Contribución de las Variables Dim-2", value = "tabVarDim2",
                                                plotOutput("plotCP2", height = "70vh"))),
                            tabPanel(title = "Resultados Numéricos", value = "pca.salida", verbatimTextOutput("txtpca"))
                     ),
                     column(width = 5, aceEditor("fieldCodePCAModelo", height = "5vh", mode = "r",
                                                 theme = "monokai", value = "", readOnly = T)),
                     column(width = 7,
                            conditionalPanel(
                              condition = "input.tabPCA == 'individuos'",
                              campo.codigo("run.pca.ind", "ref.pca.ind", "fieldCodeInd", height = "5vh")),
                            conditionalPanel(
                              condition = "input.tabPCA == 'variables'",
                              campo.codigo("run.pca.var", "ref.pca.var", "fieldCodeVar", height = "5vh")),
                            conditionalPanel(
                              condition = "input.tabPCA == 'sobreposicion'",
                              campo.codigo("run.pca.bi", "ref.pca.bi", "fieldCodeBi", height = "5vh")),
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
                                                              height = "20vh", autoComplete = "enabled", readOnly = T),
                                                    h5("Grafico de la Distribución (Categóricas)"),
                                                    aceEditor("fieldFuncCat", mode = "r", theme = "monokai", value = "",
                                                              height = "20vh", autoComplete = "enabled", readOnly = T),
                                                    circle = F, status = "danger", icon = icon("code"), width = "400px", right = T,
                                                    tooltip = tooltipOptions(title = "Clic para ver el código"))),
                              column(width = 2,
                                     dropdownButton(h4("Opciones"),
                                                    colourpicker::colourInput("col.dist", "Seleccionar Color:",
                                                                              value = "#0D00FFAA", allowTransparent = T),
                                                    circle = F, status = "danger", icon = icon("gear"), width = "100%", right = T,
                                                   tooltip = tooltipOptions(title = "Clic para ver opciones")))), width = 12,
                            tabPanel(title = 'Numéricas', value = "numericas", plotOutput('plot.num', height = "65vh"),
                                     fluidRow(column(width = 6, campo.codigo("run.dya.num", "ref.dya.num", "fieldCodeNum", height = "8vh")),
                                       column(width = 6, DT::dataTableOutput("mostrar.atipicos")))
                            ),
                            tabPanel(title = 'Categóricas', value = "categoricas", plotOutput('plot.cat', height = "67vh"),
                                     campo.codigo("run.dya.cat", "ref.dya.cat", "fieldCodeCat", height = "6vh")
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
                                      aceEditor("fieldCodeCentr", mode = "r", theme = "monokai", value = "",
                                                height = "25vh", autoComplete = "enabled", readOnly = T),
                                      conditionalPanel(
                                        condition = "input.tabjerar == 'Horizontal'",
                                        h5("Grafica todas las variables en Horizontal"),
                                        aceEditor("fieldFuncHoriz", mode = "r", theme = "monokai", value = "",
                                                  height = "20vh", autoComplete = "enabled", readOnly = T)),
                                      conditionalPanel(
                                        condition = "input.tabjerar == 'Vertical'",
                                        h5("Grafica todas las variables en Vertical"),
                                        aceEditor("fieldFuncVert", mode = "r", theme = "monokai", value = "",
                                                  height = "18vh", autoComplete = "enabled", readOnly = T)),
                                      conditionalPanel(
                                        condition = "input.tabjerar == 'Radar'",
                                        h5("Grafica todas las variables en Radar"),
                                        aceEditor("fieldFuncRadar", mode = "r", theme = "monokai", value = "",
                                                  height = "30vh", autoComplete = "enabled", readOnly = T)),
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
                                          hr(), actionButton("HCbutton", label = "Agregar Cluster a tabla de datos", width = "100%"),
                                          circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                                          tooltip = tooltipOptions(title = "Clic para ver opciones")))), width = 12,
                            tabPanel(title = 'Inercia', fluidRow(uiOutput('inercia.cj'))),
                            tabPanel(title = 'Diagrama', plotOutput('plot.diag', height = "65vh")),
                            tabPanel(title = 'Mapa', plotOutput('plot.mapa', height = "65vh")),
                            tabPanel(title = 'Horizontal', plotOutput('plot.horiz', height = "65vh")),
                            tabPanel(title = 'Vertical', plotOutput('plot.vert', height = "65vh")),
                            tabPanel(title = 'Radar', plotOutput('plot.radar', height = "65vh")),
                            tabPanel(title = 'Interpretación Categórico', value = "hcbarras", plotOutput('plot.bar.cat', height = "65vh")),
                            tabPanel(title = 'Resultados Numéricos', value = "salida.hc", verbatimTextOutput("txthc"),
                                     hr(), verbatimTextOutput("txtcentros"))
                     ),
                     fluidRow(
                       column(width = 6, aceEditor("fieldCodeModelo", height = "8vh",mode = "r",
                                                   theme = "monokai", value = "", readOnly = T)),
                       column(width = 6,
                              conditionalPanel(
                                condition = "input.tabjerar == 'Diagrama'",
                                campo.codigo("run.hc.diag", "ref.hc.diag", "fieldCodeDiag", height = "8vh")),
                              conditionalPanel(
                                condition = "input.tabjerar == 'Mapa'",
                                campo.codigo("run.hc.mapa", "ref.hc.mapa", "fieldCodeMapa", height = "8vh")),
                              conditionalPanel(
                                condition = "input.tabjerar == 'Horizontal'",
                                campo.codigo("run.hc.horiz", "ref.hc.horiz", "fieldCodeHoriz", height = "8vh")),
                              conditionalPanel(
                                condition = "input.tabjerar == 'Vertical'",
                                campo.codigo("run.hc.vert", "ref.hc.vert", "fieldCodeVert", height = "8vh")),
                              conditionalPanel(
                                condition = "input.tabjerar == 'Radar'",
                                campo.codigo("run.hc.radar", "ref.hc.radar", "fieldCodeRadar", height = "8vh")),
                              conditionalPanel(
                                condition = "input.tabjerar == 'hcbarras'",
                                campo.codigo("run.hc.barras", "ref.hc.barras", "fieldCodeBarras", height = "8vh"))
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
                                                       value = "", height = "40vh", autoComplete = "enabled", readOnly = T)),
                                           conditionalPanel(
                                             condition = "input.tabkmedias == 'Horizontal'",
                                             h5("Grafica todas las variables en Horizontal"),
                                             aceEditor("fieldFuncKhoriz", mode = "r", theme = "monokai",
                                                       value = "", height = "25vh", autoComplete = "enabled", readOnly = T)),
                                           conditionalPanel(
                                             condition = "input.tabkmedias == 'Vertical'",
                                             h5("Grafica todas las variables en Vertical"),
                                             aceEditor("fieldFuncKvert", mode = "r", theme = "monokai",
                                                       value = "", height = "25vh", autoComplete = "enabled"), readOnly = T),
                                           conditionalPanel(
                                             condition = "input.tabkmedias == 'Radar'",
                                             h5("Grafica todas las variables en Radar"),
                                             aceEditor("fieldFuncKradar", mode = "r", theme = "monokai",
                                                       value = "", height = "40vh", autoComplete = "enabled"), readOnly = T),
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
                                                        hr(), actionButton("Kbutton", label = "Agregar Cluster a tabla de datos", width = "100%"),
                                                        circle = F, status = "danger", icon = icon("gear"), width = "300px", right = T,
                                                        tooltip = tooltipOptions(title = "Clic para ver opciones")))),
                            tabPanel(title = 'Inercia', fluidRow(uiOutput('resumen.kmedias'))),
                            tabPanel(title = 'Codo Jambu', value = "codoJambu", plotOutput('plot.jambu', height = "65vh")),
                            tabPanel(title = 'Mapa', plotOutput('plot.kmapa', height = "65vh")),
                            tabPanel(title = 'Horizontal', plotOutput('plot.khoriz', height = "65vh")),
                            tabPanel(title = 'Vertical', plotOutput('plot.kvert', height = "65vh")),
                            tabPanel(title = 'Radar', plotOutput('plot.kradar', height = "65vh")),
                            tabPanel(title = 'Interpretación Categórico', value = "kbarras", plotOutput('plot.kcat', height = "65vh")),
                            tabPanel(title = 'Resultados Numéricos', value = "salida.k", verbatimTextOutput("txtk"))
                     ),
                     fluidRow(
                       column(width = 6, aceEditor("fieldCodeKModelo", height = "8vh", mode = "r",
                                                   theme = "monokai", value = "", readOnly = T)),
                       column(width = 6,
                              conditionalPanel(
                                condition = "input.tabkmedias == 'codoJambu'",
                                campo.codigo("run.k.jambu", "ref.k.jambu", "fieldCodeJambu", height = "8vh")),
                              conditionalPanel(
                                condition = "input.tabkmedias == 'Mapa'",
                                campo.codigo("run.k.mapa", "ref.k.mapa", "fieldCodeKmapa", height = "8vh")),
                              conditionalPanel(
                                condition = "input.tabkmedias == 'Horizontal'",
                                campo.codigo("run.k.horiz", "ref.k.horiz", "fieldCodeKhoriz", height = "8vh")),
                              conditionalPanel(
                                condition = "input.tabkmedias == 'Vertical'",
                                campo.codigo("run.k.vert", "ref.k.vert", "fieldCodeKvert", height = "8vh")),
                              conditionalPanel(
                                condition = "input.tabkmedias == 'Radar'",
                                campo.codigo("run.k.radar", "ref.k.radar", "fieldCodeKradar", height = "8vh")),
                              conditionalPanel(
                                condition = "input.tabkmedias == 'kbarras'",
                                campo.codigo("run.k.barras", "ref.k.barras", "fieldCodeKbarras", height = "8vh"))
                     ))
              )
      ),

      #Generar Reporte
      tabItem(tabName = "reporte",
              column(width = 5,
                     tabBox(width = 12, id = "tabReporte",
                            tabPanel(title = "Reporte", width = 12,
                                     textInput("textTitulo", value = "Sin Titulo", width = "100%", label = "Digite el Titulo:"),
                                     textInput("textNombre", value = "PROMiDAT", width = "100%", label = "Digite su Nombre:")),
                            tabPanel(title = "Código", width = 12, aceEditor("fieldCodeReport", mode="markdown", value=''))),
                     column(width = 8, actionButton("btnReporte", "Actualizar Reporte")),
                     column(width = 4, downloadButton("descargar", "Descargar"))),
              column(width = 7,
                     box(title = "Vista Previa", width = 12, height = "90vh", status = "primary", solidHeader = TRUE,
                         collapsible = TRUE, div(style = 'overflow-x: scroll; overflow-y: scroll; height: 80vh;',
                                                 withSpinner(htmlOutput("knitDoc"), type = 7, color = "#CBB051"))))
      ),

      tabItem(tabName = "acercaDe",
              img(src="Logo.png", style="padding-bottom:20px;margin-left: auto;margin-right: auto;display: block;width: 50%;"),
              infoBoxPROMiDAT("Todos los derechos reservados a", "PROMiDAT S.A.", icono = icon("copyright")),
              infoBoxPROMiDAT("Versión del Sistema", "1.2.6", icono = icon("file-code-o"))
      )
    ) #tabItems
  ) #dashboardBody
)) #UI

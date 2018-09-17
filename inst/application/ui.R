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
library(ggdendro)
library(modeest)
library(stringr)
library(rgdal)
library(raster)

# Define UI for application that draws a histogram
shinyUI(dashboardPage(title="PROMiDAT - ExploreR",
  dashboardHeader(title = tags$a(href="http://promidat.com", target = "_blank",
                                 img(src="Logo2.png", height=55, width="100%",
                                     style="padding-top:2px; padding-bottom:6px;"))),
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
                menuItem("Acerca De", tabName = "acercaDe", icon = icon("info")),
                tags$div(style = "display:none;",
                  sliderInput(inputId = "aux", min = 2, value = 2,
                              label = "Cantidad de Clusters", max = 10),
                  colourpicker::colourInput(
                    "auxColor", NULL, value = "red", allowTransparent = T)
                )
    )
  ),

  dashboardBody(
    tags$head(
      tags$link(rel = "stylesheet", type = "text/css", href = "style_promidat.css"),
      tags$link(rel = "icon", type = "image", href =
                  "http://www.promidat.org/theme/image.php/formal_white/theme/1438713216/favicon"),
      useShinyjs(),
      tags$script(src = "myscript.js")
    ),
    conditionalPanel(
      condition="($('html').hasClass('shiny-busy'))",
      div(id = "loaderWrapper", div(id="loader"))
    ),

    tabItems(

      #Carga de Datos
      tabItem(tabName = "cargar",
              column(width = 5,
                     tabBox(title = NULL, width = 12,
                       tabPanel(title = "Cargar", width = 12, solidHeader = FALSE,
                                collapsible = FALSE, collapsed = FALSE,
                       checkboxInput('header', 'Encabezado (Header)', TRUE),
                       checkboxInput('rowname', 'Incluir nombre de filas', TRUE),
                       radioButtons('sep', 'Separador', c(
                         Coma=',', 'Punto y Coma'=';', Tab='\t'),
                         selected = 'Coma'),
                       radioButtons('dec', 'Separador Decimal',
                                    c('Punto'='.', 'Coma'=","),
                                    selected = 'Punto'),
                       switchInput(inputId = "deleteNA", onStatus = "success",
                                   offStatus = "danger", value = T, width = "100%",
                                   label = "Eliminar NA", onLabel = "SI",
                                   offLabel = "NO", labelWidth = "100%"),
                       fileInput('file1', label = 'Cargar Archivo', width = "100%",
                                 placeholder = "", buttonLabel = "Subir",
                                 accept = c('text/csv', 'text/comma-separated-values, text/plain', '.csv')),
                       actionButton("loadButton", "Cargar", width = "100%"),
                       hr(),
                       aceEditor("fieldCodeData", mode = "r", theme = "monokai",
                                 value = "", height = "15vh", readOnly = T)
                     ),
                     tabPanel(title = "Transformar", width = 12, solidHeader = FALSE,
                              collapsible = FALSE, collapsed = FALSE,
                              DT::dataTableOutput('transData'), hr(),
                              actionButton("transButton", "Aplicar", width = "100%"),
                              hr(),
                              aceEditor("fieldCodeTrans", mode = "r", theme = "monokai",
                                        value = "", height = "10vh", readOnly = T)
                     )
              )),
              column(width = 7,
                     box(title = "Datos", status = "primary", width = 12,
                         solidHeader = TRUE, collapsible = TRUE,
                         DT::DTOutput('contents'), hr(),
                         downloadButton("downloaDatos", "Descargar Datos",
                                        width = "100%")))
      ),

      #Resumen Numérico
      tabItem(tabName = "resumen",
              column(width = 7,
                     box(title = "Resumen Numérico", status = "primary",
                         width = 12, solidHeader = TRUE, collapsible = TRUE,
                         DT::dataTableOutput("resumen.completo"), hr(),
                         aceEditor("fieldCodeResum", mode = "r", theme = "monokai",
                                   value = "", height = "8vh", readOnly = T)
                     )
              ),
              column(width = 5,
                     box(title = "Resumen Numérico por Variable", status = "primary",
                         width = 12, solidHeader = TRUE, collapsible = TRUE,
                         selectInput(inputId = "sel.resumen",
                                     label = h4("Seleccionar Variable:"),
                                     choices =  ""),
                         fluidRow(uiOutput("resumen"))
                     )
              )
      ),

      #test de Normalidad
      tabItem(tabName = "normalidad",
              tabBox(id = "BoxNormal", width = NULL,
                     title = tags$div(class = "multiple-select-var",
                                selectInput(inputId = "sel.normal",
                                            label = NULL, choices =  "")),
                     tabPanel(title = "Gráfico Normalidad", value = "tabNormalPlot",
                              plotOutput('plot.normal', height = "70vh")),
                     tabPanel(title = "Test de Normalidad", value = "tabNormalCalc",
                              DT::dataTableOutput('calculo.normal')),
                     tabsOptions(heights = c(50, 50, 100), tabs.content = list(
                       list(h4("Opciones"), hr(),
                            colourpicker::colourInput(
                              "col.normal", "Seleccionar Color:",
                              value = "#00FF22AA", allowTransparent = T)),
                       list(
                         conditionalPanel(
                           "input.BoxNormal == 'tabNormalPlot'",
                           campo.codigo("run.normal", "ref.normal",
                                        "fieldCodeNormal", height = "25vh")),
                         conditionalPanel(
                           "input.BoxNormal == 'tabNormalCalc'",
                           campo.codigo("run.calc.normal", "ref.calc.normal",
                                        "fieldCalcNormal", height = "20vh"))
                       )
                     ))
              )
      ),

      #Dispersión
      tabItem(tabName = "dispersion",
              tabBox(id = "BoxDisp", width = NULL, title =
                       tags$div(class = "multiple-select-var",
                                selectizeInput("select.var", NULL, multiple = T, choices = c(""),
                                               options = list(
                                                 maxItems = 3,
                                                 placeholder = "Seleccione las variables"))),
                     tabPanel(title = "Dispersión", value = "tabDisp",
                              fluidRow(
                                column(width = 8,
                                       plotOutput('plot.disp', height = "70vh",
                                                  brush = brushOpts(
                                                    id = "zoom.disp",
                                                    resetOnNew = TRUE
                                ))),
                                column(width = 4,
                                       DT::dataTableOutput('mostrar.disp.zoom'),
                                       hr(), plotOutput('plot.disp.zoom', height = "41vh")
                     ))),
                     tabsOptions(heights = c(50, 40, 100), tabs.content = list(
                       list(h4("Opciones"), hr(),
                            colourpicker::colourInput(
                              "col.disp", "Seleccionar Color:",
                              value = "#FF0000AA", allowTransparent = T)),
                       list(
                         column(width = 12,
                                campo.codigo("run.disp", "ref.disp",
                                             "fieldCodeDisp", height = "15vh"))
                       )
                     ))
              )
      ),

      #Distribuciones
      tabItem(tabName = "distribucion",
              tabBox(id = "tabDyA", width = NULL,
                     title =  tags$div(class = "multiple-select-var",
                       conditionalPanel(
                         condition = "input.tabDyA == 'numericas'",
                         selectInput(inputId = "sel.distribucion.num",
                                     label = NULL, choices =  "")),
                       conditionalPanel(
                         condition = "input.tabDyA == 'categoricas'",
                         selectInput(inputId = "sel.distribucion.cat",
                                     label = NULL, choices =  ""))),
                     tabPanel(title = 'Numéricas', value = "numericas",
                              plotOutput('plot.num', height = "70vh")),
                     tabPanel(title = 'Categóricas', value = "categoricas",
                              plotOutput('plot.cat', height = "70vh")),
                     tabsOptions(
                       botones = list(icon("gear"), icon("terminal"),
                                      icon("info"), icon("code")),
                       widths = c(50, 100, 100, 100),
                       heights = c(50, 40, 50, 100),
                       tabs.content = list(
                         list(h4("Opciones"), hr(), colourpicker::colourInput(
                           "col.dist", "Seleccionar Color:", value = "#0D00FFAA",
                           allowTransparent = T)),
                         list(conditionalPanel(
                           condition = "input.tabDyA == 'numericas'",
                           campo.codigo("run.dya.num", "ref.dya.num",
                                        "fieldCodeNum", height = "15vh")),
                           conditionalPanel(
                             condition = "input.tabDyA == 'categoricas'",
                             campo.codigo("run.dya.cat", "ref.dya.cat",
                                          "fieldCodeCat", height = "15vh"))),
                         list(DT::dataTableOutput("mostrar.atipicos")),
                         list(h4("Código"), hr(),
                              h5("Grafico de la Distribución (Numéricas)"),
                              aceEditor("fieldFuncNum", mode = "r", theme = "monokai",
                                        value = "", height = "300px", readOnly = T),
                              h5("Grafico de la Distribución (Categóricas)"),
                              aceEditor("fieldFuncCat", mode = "r", theme = "monokai",
                                        value = "", height = "180px", readOnly = T)))
                     )
              )
      ),

      #Correlaciones
      tabItem(tabName = "correlacion",
              tabBox(id = "tabCor", width = NULL,
                     tabPanel(title = 'Correlación', value = "correlacion",
                              plotOutput('plot.cor', height = "70vh")),
                     tabPanel(title = 'Resultados Numéricos', value = "cor.salida",
                              verbatimTextOutput("txtcor")),
                     tabsOptions(heights = c(70, 50, 100), tabs.content = list(
                       list(h4("Opciones"), hr(),
                            selectInput(inputId = "cor.metodo", label = "Seleccionar Método",
                                        choices =  c("circle", "square", "ellipse", "number",
                                                     "shade", "color", "pie")),
                            selectInput(inputId = "cor.tipo", label = "Seleccionar Tipo",
                                        choices =  c("lower", "upper", "full"))),
                       list(
                         aceEditor("fieldModelCor", height = "6vh", mode = "r",
                                   theme = "monokai", value = "", readOnly = T),
                         campo.codigo("run.code.cor", "ref.code.cor",
                                      "fieldCodeCor", height = "15vh")
                       )
              )))
      ),

      #PCA
      tabItem(
        tabName = "acp",
        tabBox(
          id = "tabPCA", width = NULL,
          tabPanel(title = 'Individuos', value = "tabInd", fluidRow(
            column(
              width = 8,
              plotOutput('plot.ind', height = "70vh",
                         brush = brushOpts(id = "zoom.ind", resetOnNew = TRUE))
            ),
            column(width = 4, DT::dataTableOutput('mostrar.ind.zoom'),
                   hr(), plotOutput('plot.ind.zoom')))
          ),
          tabPanel(title = 'Variables', value = "tabVar",
                   plotOutput('plot.var', height = "70vh")
          ),
          tabPanel(
            title = 'Sobreposición', value = "tabBi", fluidRow(
              column(
                width = 8,
                plotOutput('plot.biplot', height = "70vh",
                           brush = brushOpts(id = "zoom.bi", resetOnNew = TRUE))
              ),
              column(width = 4, DT::dataTableOutput('mostrar.bi.zoom'),
                     hr(), plotOutput('plot.bi.zoom')))
          ),
          navbarMenu(
            "Ayuda Interpretación",
            tabPanel("Varianza Explicada por cada Eje", value = "tabVEE",
                     plotOutput("plotVEE", height = "70vh")),
            tabPanel("Cosenos Cuadrados de los Individuos", value = "tabCCI",
                     plotOutput("plotCCI", height = "70vh")),
            tabPanel("Cosenos Cuadrados de las Variables", value = "tabCCV",
                     plotOutput("plotCCV", height = "70vh")),
            tabPanel("Correlación Variables-Componentes", value = "tabCVC",
                     plotOutput("plotCVC", height = "70vh")),
            tabPanel("Contribución de las Variables Dim-1", value = "tabPC1",
                     plotOutput("plotPC1", height = "70vh")),
            tabPanel("Contribución de las Variables Dim-2", value = "tabPC2",
                     plotOutput("plotPC2", height = "70vh"))),
          tabPanel(title = "Resultados Numéricos", value = "pca.salida",
                   verbatimTextOutput("txtpca")),
          tabsOptions(
            tabs.content = list(
              list(h4("Opciones"), hr(),
                   switchInput(inputId = "switch.scale", value = T,
                               onStatus = "success", offStatus = "danger",
                               label = "Centrar y Reducir", onLabel = "SI",
                               offLabel = "NO", labelWidth = "100%"),
                   sliderInput("slider.npc", "Número de Dimensiones: ",
                               min = 2, max = 10, value = 5),
                   sliderTextInput("slider.ejes", "Seleccionar Ejes:", grid = T,
                                   choices = c(1:10), selected = c(1,2)),
                   conditionalPanel(
                     condition = paste0("input.tabPCA == 'tabInd' ||",
                                        " input.tabPCA == 'tabBi'"),
                     sliderInput("ind.cos", "Coseno de los Individuos: ",
                                 min = 0, max = 100, value = 0),
                     colourpicker::colourInput(
                       "col.pca.ind", "Seleccionar Color (Individuos):",
                       value = "#696969", allowTransparent = T)
                   ),
                   conditionalPanel(
                     condition = paste0("input.tabPCA == 'tabVar' || ",
                                        "input.tabPCA == 'tabBi'"),
                     sliderInput("var.cos", "Coseno de las Variables: ",
                                 min = 0, max = 100, value = 0),
                     colourpicker::colourInput(
                       "col.pca.var", "Seleccionar Color (Variables):",
                       value = "steelblue", allowTransparent = T)
                   ),
                   conditionalPanel(
                     condition = "input.tabPCA == 'tabCVC'",
                     selectInput(
                       inputId = "cvc.metodo", label = "Seleccionar Método",
                       choices =  c("circle", "square", "ellipse",  "number",
                                    "shade", "color", "pie"))
                   )
              ),
              list(aceEditor("fieldCodePCAModelo", height = "5vh", mode = "r",
                             theme = "monokai", value = "", readOnly = T),
                   lapply(c("Ind", "Var", "Bi"), function(i) {
                     conditionalPanel(
                       condition = paste0("input.tabPCA == 'tab", i, "'"),
                       campo.codigo(paste0("run.pca", i), paste0("ref.pca", i),
                                    paste0("fieldCode", i), height = "15vh"))
                   }),
                   lapply(c('VEE', 'CCI', 'CCV', 'CVC', 'PC1', 'PC2'), function(i) {
                     conditionalPanel(
                       condition = paste0("input.tabPCA == 'tab", i, "'"),
                       aceEditor(paste0("fieldCode", i), mode = "r", theme = "monokai",
                                 value = "", height = "15vh", readOnly = T))
                   })
              )
          ))
        )
      ),

      #Agrupaciones
      tabItem(tabName = "agrupacion", tabBox(
        id = "tabjerar", width = 12, title =
          tags$div(class = "multiple-select-var",
                   lapply(c("Horiz", "Vert", "Bar"), function(i) {
                     conditionalPanel(
                       condition = paste0("input.tabjerar == 'tab", i, "'"),
                       selectInput(inputId = paste0("sel", i), label = NULL,
                                   choices =  ""))
                   })
          ),
        tabPanel(
          title = 'Inercia', value = "tabInercia",
          wellPanel(fluidRow(uiOutput('inercia.cj')), style="height: 65vh;")
        ),
        tabPanel(title = 'Dendograma', value = "tabDendo",
                 plotOutput('plot.diag', height = "70vh")
        ),
        tabPanel(
          title = 'Mapa', value = "tabMapa", fluidRow(
            column(
              width = 8,
              plotOutput('plot.mapa', height = "70vh",
                         brush = brushOpts(id = "zoom.mapa", resetOnNew = TRUE))
            ),
            column(width = 4, DT::dataTableOutput('mostrar.mapa.zoom'),
                   hr(), plotOutput('plot.mapa.zoom', height = "41vh")))
        ),
        tabPanel(title = 'Horizontal', value = "tabHoriz",
                 plotOutput('plot.horiz', height = "70vh")
        ),
        tabPanel(title = 'Vertical', value = "tabVert",
                 plotOutput('plot.vert', height = "70vh")
        ),
        tabPanel(title = 'Radar', value = "tabRadar",
                 plotOutput('plot.radar', height = "70vh")
        ),
        tabPanel(title = 'Interpretación Categórico', value = "tabBar",
                 plotOutput('plot.bar.cat', height = "70vh")
        ),
        tabPanel(title = 'Resultados Numéricos', value = "salida.hc",
                 verbatimTextOutput("txthc"), hr(),
                 verbatimTextOutput("txtcentros")
        ),
        tabsOptions(
          botones = list(icon("gear"), icon("terminal"), icon("code")),
          widths = c(33.3, 100, 100), heights = c(100, 50, 50),
          tabs.content = list(
            list(h4("Opciones"), hr(),
                 sliderInput(inputId = "cant.cluster", min = 2, max = 10,
                             label = "Cantidad de Clusters:", value = 2),
                 selectInput(inputId = "sel.hc.method",
                             label = "Índice de Agregación", selectize = T,
                             choices =  c("ward.D2", "single", "complete", "average")),
                 selectInput(inputId = "sel.dist.method",
                             label = "Método para la Distancia", selectize = T,
                             choices =  c("euclidean", "maximum", "manhattan",
                                          "canberra", "binary", "minkowski")),
                 HTML("<label class='control-label'>Seleccionar Colores: </label>"),
                 fluidRow(
                   lapply(1:10, function(i)
                     tags$div(class = "select-color", colourpicker::colourInput(
                       paste0("hcColor", i), NULL, value = def.colors[i],
                       allowTransparent = T)))), hr(),
                 actionButton("HCbutton", width = "100%",
                              label = "Agregar Cluster a tabla de datos")),
            list(
              aceEditor("fieldCodeModelo", height = "8vh",mode = "r",
                        theme = "monokai", value = "", readOnly = T),
              lapply(c("Dendo", "Mapa", "Horiz", "Vert", "Radar", "Bar"), function(i) {
                conditionalPanel(
                  condition = paste0("input.tabjerar == 'tab", i, "'"),
                  campo.codigo(paste0("run.hc", i), paste0("ref.hc", i),
                               paste0("fieldCode", i), height = "13vh"))
              })
            ),
            list(h4("Código"), hr(),
                 h5("Calculo de los centros"),
                 aceEditor("fieldCodeCentr", mode = "r", theme = "monokai",
                           value = "", height = "22vh", readOnly = T),
                 lapply(list(
                   list(titulo = "Grafica todas las variables en Horizontal", id = "Horiz"),
                   list(titulo = "Grafica todas las variables en Vertical", id = "Vert"),
                   list(titulo = "Grafica todas las variables en Radar", id = "Radar")),
                   function(i) {
                     conditionalPanel(
                       condition = paste0("input.tabjerar == 'tab", i$id, "'"),
                       h5(i$titulo),
                       aceEditor(paste0("fieldFunc", i$id), mode = "r", theme = "monokai",
                                 value = "", height = "22vh", readOnly = T))
                 }))
            )
          )
        )
      ),

      #K-means
      tabItem(
        tabName = "kmedias", tabBox(
          id = "tabkmedias", width = 12, title =
            tags$div(
              class = "multiple-select-var",
              lapply(c("Khoriz", "Kvert", "Kbar"), function(i) {
                conditionalPanel(
                  condition = paste0("input.tabkmedias == 'tab", i, "'"),
                  selectInput(inputId = paste0("sel.", i),
                              label = NULL, choices = ""))
              })
            ),
          tabPanel(
            title = 'Inercia', value = "tabKinercia",
            wellPanel(fluidRow(uiOutput('inercia.k')), style="height: 65vh;")
          ),
          tabPanel(title = 'Codo Jambu', value = "tabJambu",
                   plotOutput('plot.jambu', height = "70vh")
          ),
          tabPanel(title = 'Mapa', value = "tabKmapa", fluidRow( column(
            width = 8,
            plotOutput('plot.kmapa', height = "70vh", brush =
                         brushOpts(id = "zoom.kmapa", resetOnNew = TRUE))),
            column(width = 4, DT::dataTableOutput('mostrar.kmapa.zoom'), hr(),
                   plotOutput('plot.kmapa.zoom', height = "41vh")))
          ),
          tabPanel(title = 'Horizontal', value = "tabKhoriz",
                   plotOutput('plot.khoriz', height = "70vh")
          ),
          tabPanel(title = 'Vertical', value = "tabKvert",
                   plotOutput('plot.kvert', height = "70vh")
          ),
          tabPanel(title = 'Radar', value = "tabKradar",
                   plotOutput('plot.kradar', height = "70vh")
          ),
          tabPanel(title = 'Interpretación Categórico', value = "tabKbar",
                   plotOutput('plot.kcat', height = "70vh")
          ),
          tabPanel(title = 'Resultados Numéricos', value = "salida.k",
                   verbatimTextOutput("txtk")
          ),
          tabsOptions(
            botones = list(icon("gear"), icon("terminal"), icon("code")),
            widths = c(33.3, 100, 100), heights = c(100, 50, 80),
            tabs.content = list(
              list(h4("Opciones"), hr(),
                   conditionalPanel(
                     condition = "input.tabkmedias == 'codoJambu'",
                     sliderInput(inputId = "iteracionesK", min = 2, value = 20,
                                 label = "Cantidad de iteraciones para K", max = 20)),
                   sliderInput(inputId = "cant.kmeans.cluster", min = 2, value = 2,
                               label = "Cantidad de Clusters", max = 10),
                   numericInput("num.nstart", step = 10, value = 1,
                                label = "Ejecuciones en Formas Fuertes (nstart)"),
                   numericInput("num.iter", step = 100, value = 10,
                                label = "Número de Iteraciones"),
                   selectInput(
                     inputId = "sel.algoritmo", label = "Algoritmo", selectize = T,
                     choices =  c("Hartigan-Wong", "Lloyd", "Forgy", "MacQueen")),
                   HTML("<label class='control-label'>Seleccionar Colores: </label>"),
                   fluidRow(
                     lapply(1:10, function(i)
                       tags$div(class = "select-color", colourpicker::colourInput(
                         paste0("kColor", i), NULL, value = def.colors[i],
                         allowTransparent = T)))
                   ), hr(),
                   actionButton("Kbutton", width = "100%",
                                label = "Agregar Cluster a tabla de datos")
              ),
              list(
                aceEditor("fieldCodeKModelo", height = "5vh", mode = "r",
                          theme = "monokai", value = "", readOnly = T),
                lapply(c("Jambu", "Kmapa", "Khoriz", "Kvert",
                         "Kradar", "Kbar"), function(i) {
                  conditionalPanel(
                    condition = paste0("input.tabkmedias == 'tab", i, "'"),
                    campo.codigo(paste0("run.", i), paste0("ref.", i),
                                 paste0("fieldCode", i), height = "15vh"))
                })
              ),
              list(h4("Código"), hr(),
                   lapply(list(
                     list(titulo = "Calculo del Codo de Jambu", id = "Jambu"),
                     list(titulo = "Grafica todas las variables en Horizontal", id = "Khoriz"),
                     list(titulo = "Grafica todas las variables en Vertical", id = "Kvert"),
                     list(titulo = "Grafica todas las variables en Radar", id = "Kradar")),
                     function(i) {
                       conditionalPanel(
                         condition = paste0("input.tabkmedias == 'tab", i$id, "'"),
                         h5(i$titulo),
                         aceEditor(paste0("fieldFunc", i$id), mode = "r", theme = "monokai",
                                   value = "", height = "50vh", readOnly = T))
                   })
              ),
              list(
                h4("Titulo"), hr(),
                textAreaInput("titleK", label = NULL, resize = "both"),
                h4("Interpretación"), hr(),
                textAreaInput("textoK", label = NULL, resize = "both"))
            )
          )
        )
      ),

      #Generar Reporte
      tabItem(tabName = "reporte",
              column(width = 5, box(title = "Reporte", width = 12,
                                    textInput("textTitulo", value = "Sin Titulo",
                                              width = "100%", label = "Digite el Titulo:"),
                                    textInput("textNombre", value = "PROMiDAT",
                                              width = "100%", label = "Digite su Nombre:"),
                                    hr(),
                                    downloadButton("descargar", "Descargar",
                                                   class = "center-button"))),
              column(width = 7,
                     box(title = "Código Reporte", width = 12, height = "50vh",
                         status = "primary", solidHeader = TRUE,
                         collapsible = TRUE,
                         aceEditor("fieldCodeReport", mode="markdown",
                                   value='', height = "43vh"))),
              fluidRow(column(width = 12,
                              box(title = "Salida R", width = 12, height = "35vh",
                                  verbatimTextOutput("txtreport"))))
      ),

      tabItem(tabName = "acercaDe",
              img(src="Logo.png",
                  style=paste0("padding-bottom:20px;margin-left: auto;",
                               "margin-right: auto;display: block;width: 50%;")),
              infoBoxPROMiDAT("Todos los derechos reservados a", "PROMiDAT S.A.",
                              icono = icon("copyright")),
              infoBoxPROMiDAT("Más información",
                              tags$a(href="https://www.promidat.com/", style = "color:white;",
                                     target = "_blank", "https://www.promidat.com"),
                              icono = icon("info")),
              infoBoxPROMiDAT("Versión del Sistema", "2.0.0",
                              icono = icon("file-code-o"))
      )
    ) #tabItems
  ) #dashboardBody
)) #UI




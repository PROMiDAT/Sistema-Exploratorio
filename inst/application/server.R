#
# This is the server logic of a Shiny web application. You can run the
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

shinyServer(function(input, output, session) {
  source('global.R', local = T)
  options(shiny.maxRequestSize=200*1024^2)
  options(DT.options =
            list(aLengthMenu = c(10, 30, 50), iDisplayLength = 10,
                 scrollX = TRUE, language =
                   list(search = "Buscar:",
                        info = "Mostrando _START_ - _END_ de _TOTAL_ entradas",
                        emptyTable = "No hay datos disponibles",
                        paginate = list(previous = "Anterior", "next" = "Siguiente",
                                     first = "Inicial", last = "Ultimo"))))

  session$onSessionEnded(function() {
    rm(envir = .GlobalEnv, list = ls(envir = .GlobalEnv))
    recover.cat()
    stopApp()
  })

  #' Carga Inicial
  #' @author Diego
  #' @return functions
  #' @export
  #'
  updateAceEditor(session, "fieldCodeResum", value = "summary(datos)")
  updateAceEditor(session, "fieldModelCor", value = modelo.cor())
  updateAceEditor(session, "fieldFuncJambu",
                  value = paste0(extract.code("lead"), "\n",
                                 extract.code("codo.jambu")))
  updateAceEditor(session, "fieldFuncNum", value =
                    extract.code("distribucion.numerico"))
  updateAceEditor(session, "fieldFuncCat", value =
                    extract.code("distribucion.categorico"))

  updateAceEditor(session, "fieldCodeCentr", value =
                    extract.code("calc.centros"))
  updateAceEditor(session, "fieldFuncHoriz", value =
                    extract.code("centros.horizontal.todos"))
  updateAceEditor(session, "fieldFuncVert", value =
                    extract.code("centros.vertical.todos"))
  updateAceEditor(session, "fieldFuncRadar",
                  value = paste0(extract.code("coord_radar"), "\n",
                                 extract.code("centros.radar")))

  updateAceEditor(session, "fieldFuncKhoriz", value =
                    extract.code("centros.horizontal.todos"))
  updateAceEditor(session, "fieldFuncKvert", value =
                    extract.code("centros.vertical.todos"))
  updateAceEditor(session, "fieldFuncKradar",
                  value = paste0(extract.code("coord_radar"), "\n",
                                 extract.code("centros.radar")))


  updateData <- reactiveValues(cor.modelo = NULL, pca.modelo = NULL,
                               hc.modelo = NULL, k.modelo = NULL)

  updatePlot <- reactiveValues(
    calc.normal=default.calc.normal(), normal=NULL, disp=NULL, pca.ind=NULL,
    pca.var=NULL, pca.bi=NULL, cor=NULL, pca.cvc=NULL, mapa=NULL, dya.num=NULL,
    dya.cat=NULL, diag=NULL, horiz=NULL, vert=NULL, radar=NULL, cat=NULL,
    jambu=NULL, kmapa=NULL, khoriz=NULL, kvert=NULL, kradar=NULL, kcat=NULL)

  disp.ranges <- reactiveValues(x = NULL, y = NULL)
  ind.ranges <- reactiveValues(x = NULL, y = NULL)
  bi.ranges <- reactiveValues(x = NULL, y = NULL)
  mapa.ranges <- reactiveValues(x = NULL, y = NULL)
  kmapa.ranges <- reactiveValues(x = NULL, y = NULL)

  observe({
    addClass(class = "disabled", selector = "#sidebarItemExpanded li[class^=treeview]")
    addClass(class = "disabled", selector = "#sidebarItemExpanded li a[data-value=acp]")
    addClass(class = "disabled", selector = "#sidebarItemExpanded li a[data-value=agrupacion]")
    addClass(class = "disabled", selector = "#sidebarItemExpanded li a[data-value=kmedias]")
    addClass(class = "disabled", selector = "#sidebarItemExpanded li a[data-value=reporte]")
    close.menu()
  })

  observeEvent(input$loadButton, {
    tryCatch({
       codigo.carga <- code.carga(
         nombre.filas = input$rowname, ruta = input$file1$datapath,
         separador = input$sep, sep.decimal = input$dec,
         encabezado = input$header)
       isolate(eval(parse(text = codigo.carga)))
       nombre.datos <<- stringi::stri_extract_first(str = input$file1$name,
                                                    regex = ".*(?=\\.)")
       if(ncol(datos) <= 1){
         showNotification(paste0("Error al cargar los Datos: Revisar separadores"),
                          duration = 10, type = "error")
         return(NULL)
       }
     }, error = function(e) {
       showNotification(paste0("Error al cargar los Datos: ", e),
                        duration = 10, type = "error")
       datos <<- NULL
       datos.originales <<- NULL
       return(NULL)
     })

    if(any(is.na(datos))){
      tryCatch({
        codigo.na <- paste0(code.NA(deleteNA = input$deleteNA), "\n",
                            "datos <<- datos.originales")
        isolate(eval(parse(text = codigo.na)))
        datos.reporte[[nombre.datos]] <<- datos
      }, error = function(e) {
        showNotification(paste0("Error al eliminar NAs: ", e),
                         duration = 10, type = "error")
        datos <<- NULL
        datos.originales <<- NULL
        return(NULL)
      })
    } else {
      codigo.na <- ""
      datos.reporte[[nombre.datos]] <<- datos
    }

    updateAceEditor(session, "fieldCodeData",
                    value = paste0(codigo.carga, "\n", codigo.na))

    toggleClass(condition = (is.null(datos) || ncol(datos) < 1),
                class = "disabled", selector = "#sidebarItemExpanded li[class^=treeview]")
    toggleClass(condition = (is.null(datos) || ncol(datos) < 1),
                class = "disabled", selector = "#sidebarItemExpanded li a[data-value=acp]")
    toggleClass(condition = (is.null(datos) || ncol(datos) < 1),
                class = "disabled", selector = "#sidebarItemExpanded li a[data-value=agrupacion]")
    toggleClass(condition = (is.null(datos) || ncol(datos) < 1),
                class = "disabled", selector = "#sidebarItemExpanded li a[data-value=kmedias]")
    toggleClass(condition = (is.null(datos) || ncol(datos) < 1),
                class = "disabled", selector = "#sidebarItemExpanded li a[data-value=reporte]")

    tryCatch({
      isolate(eval(parse(text = modelo.cor())))
      updateData$cor.modelo <- correlacion
      output$txtcor <- renderPrint(print(correlacion))
      updateSelectizeInput(session, "sel.normal",
                           choices = colnames(var.numericas(datos)))
      updateSelectizeInput(session, "select.var",
                           choices =colnames(var.numericas(datos)))
      updateSelectInput(session, "sel.distribucion.num",
                        choices = colnames(var.numericas(datos)))
      updateSelectInput(session, "sel.distribucion.cat",
                        choices = colnames(var.categoricas(datos)))
      updateSelectInput(session, "sel.resumen",
                        choices = colnames(datos))
      updateSelectInput(session, "selVert",
                        choices = c("Todos", colnames(var.numericas(datos))))
      updateSelectInput(session, "sel.Kvert",
                        choices = c("Todos", colnames(var.numericas(datos))))
      updateSelectInput(session, "sel.Kbar",
                        choices = colnames(var.categoricas(datos)))
      updateSelectInput(session, "selBar",
                        choices = colnames(var.categoricas(datos)))
      nmax <- calc.maxK(datos)
      updateSliderInput(session, "iteracionesK", max = nmax, value = nmax)
    }, error = function(e) {
      print(paste0("ERROR EN EVALUAR: ", e))
      return(datos <- NULL)
    })

    output$contents = DT::renderDT(mostrarData(), server = F)
    close.menu(is.null(datos))
  }, priority = 4)

  observeEvent(input$transButton, {
    var.noactivas <- c()
    code.res <- "datos <<- datos.originales \n"
    for (var in colnames(datos.originales)) {
      if(input[[paste0("box", var, contador)]]) {
        if(input[[paste0("sel", var, contador)]] == "categorico" &
           class(datos.originales[, var]) %in% c("numeric","integer")) {
          code.res <- paste0(code.res, code.trans(var, "categorico"), "\n")
        }
        if(input[[paste0("sel", var, contador)]] == "numerico" &
           !(class(datos.originales[, var]) %in% c("numeric","integer"))) {
          code.res <- paste0(code.res, code.trans(var, "numerico"), "\n")
        }
        if(input[[paste0("sel", var, contador)]] == "disyuntivo"){
          code.res <- paste0(code.res, code.trans(var, "disyuntivo"), "\n")
        }
      } else {
        var.noactivas <- c(var.noactivas, var)
      }
    }

    isolate(eval(parse(text = code.res)))
    env.report$codigo.reporte[[nombre.datos]] <<-
      paste0(env.report$codigo.reporte[[nombre.datos]],
             "\n\n## Transformación de Datos\n\n```{r}\n",
             code.res, "\nstr(datos)\n```")
    code.des <- ""
    if(length(var.noactivas) > 0){
      code.des <- code.desactivar(var.noactivas)
      isolate(eval(parse(text = code.des)))
      env.report$codigo.reporte[[nombre.datos]] <<-
        paste0(env.report$codigo.reporte[[nombre.datos]],
               "\n\n## Eliminación de Variables\n\n```{r}\n",
               code.des, "\nstr(datos)\n```")
    }
    updateAceEditor(session, "fieldCodeTrans",
                    value = paste0(code.res, "\n", code.des))

    tryCatch({
      isolate(eval(parse(text = modelo.cor())))
      updateData$cor.modelo <- correlacion
      output$txtcor <- renderPrint(print(correlacion))
      updateSelectizeInput(session, "sel.normal",
                           choices = colnames(var.numericas(datos)))
      updateSelectizeInput(session, "select.var",
                           choices = colnames(var.numericas(datos)))
      updateSelectInput(session, "sel.distribucion.num",
                        choices = colnames(var.numericas(datos)))
      updateSelectInput(session, "sel.distribucion.cat",
                        choices = colnames(var.categoricas(datos)))
      updateSelectInput(session, "sel.resumen",
                        choices = colnames(datos))
      updateSelectInput(session, "selVert",
                        choices = c("Todos", colnames(var.numericas(datos))))
      updateSelectInput(session, "sel.Kvert",
                        choices = c("Todos", colnames(var.numericas(datos))))
      updateSelectInput(session, "sel.Kbar",
                        choices = colnames(var.categoricas(datos)))
      updateSelectInput(session, "selBar",
                        choices = colnames(var.categoricas(datos)))
      nmax <- calc.maxK(datos)
      updateSliderInput(session, "iteracionesK", max = nmax, value = nmax)
    }, error = function(e) {
      return(datos <- NULL)
    })

    output$contents = DT::renderDT(mostrarData(), server = F)
    close.menu(is.null(datos))
  }, priority = 4)

  observeEvent(c(input$loadButton, input$transButton,
                 input$switch.scale, input$slider.npc), {
    tryCatch({
      if(!is.null(datos)){
        codigo <- def.pca.model(scale.unit = input$switch.scale,
                                npc = input$slider.npc)
        updateAceEditor(session, "fieldCodePCAModelo", value = codigo)
        isolate(eval(parse(text = codigo)))
        updateData$pca.modelo <- pca.modelo
        output$txtpca <- renderPrint(print(unclass(pca.modelo)))
        env.report$codigo.reporte[[nombre.datos]] <<-
          paste0(
            env.report$codigo.reporte[[nombre.datos]], "\n\n## Modelo ACP (",
            ifelse(input$switch.scale, "Centrada y Reducida",
                   "Sin Centrar ni Reducir"), ")\n\n```{r}\n", codigo, "\n```")
        updateSliderTextInput(session, "slider.ejes", choices =
                                c(1:input$slider.npc), selected = c(1,2))
      }
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  }, priority = 2)

  mostrarData <- function() {
    nombre.columnas <- c("ID", colnames(datos))
    tipo.columnas <- c("", sapply(colnames(datos), function(i)
      ifelse(class(datos[,i]) %in% c("numeric", "integer"),
             "Numérico", "Categórico")))
    sketch = htmltools::withTags(table(
      tableHeader(nombre.columnas),
      tableFooter(tipo.columnas)
    ))
    return(DT::datatable(head(datos, 150), selection = 'none',
                         editable = TRUE, container = sketch,
                         options = list(dom = 'frtip', scrollY = "40vh")))
  }
  output$contents = DT::renderDT(NULL, server = F)

  update.trans <- eventReactive(input$loadButton, {
    contador <<- contador + 1
    if(!is.null(datos) && ncol(datos) > 0) {
      res <-  data.frame(Variables = colnames(datos),
                         Tipo = c(1:ncol(datos)), Activa = c(1:ncol(datos)))
      res$Tipo <- sapply(colnames(datos), function(i)
        paste0('<select id="sel', i, contador, '">',
               '<option value="categorico">Categórico</option>',
               '<option value="numerico" ',
               ifelse(class(datos[, i]) %in% c("numeric","integer"),
                      ' selected="selected"', ''), '>Numérico</option>',
               '<option value="disyuntivo">Disyuntivo</option> </select>'))
      res$Activa <- sapply(colnames(datos), function(i)
        paste0('<input type="checkbox" id="box', i, contador, '" checked/>'))
    } else {
      res <-  as.data.frame(NULL)
      showNotification("Tiene que cargar los datos", duration = 10, type = "error")
    }
    return(res)
  })

  output$transData =
    DT::renderDataTable(update.trans(), escape = FALSE, selection = 'none',
                        server = FALSE, rownames = F,
                        options = list(dom = 't', paging = FALSE,
                                       ordering = FALSE, scrollY = "40vh"),
                        callback = JS(paste0(
                          "table.rows().every(function(i, tab, row) {\n",
                          "var $this = $(this.node());\n",
                          "$this.attr('id', this.data()[0]);\n",
                          "$this.addClass('shiny-input-checkbox');});\n",
                          "Shiny.unbindAll(table.table().node());\n",
                          "Shiny.bindAll(table.table().node());")))

  #' Resumen numérico
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$resumen.completo = DT::renderDataTable({
    return(obj.resum())
  }, options = list(dom = 'ft', scrollX = TRUE), rownames = F)

  obj.resum <- eventReactive(c(input$loadButton, input$transButton), {
    env.report$codigo.reporte[[nombre.datos]] <<-
      paste0(env.report$codigo.reporte[[nombre.datos]],
             createLog("Resumen Numérico", "summary(datos)"))
    data.frame(unclass(summary(datos)), check.names = FALSE,
               stringsAsFactors = FALSE)
  })

  output$resumen = renderUI({
    if(input$sel.resumen %in% colnames(var.numericas(datos))){
      HTML(resumen.numerico(datos, input$sel.resumen))
    } else {
      HTML(resumen.categorico(datos, input$sel.resumen))
    }
  })

  #' Gráfico de Test de normalidad
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.normal = renderPlot({
    tryCatch({
      cod.normal <<- updatePlot$normal
      res <- isolate(eval(parse(text = cod.normal)))
      updateAceEditor(session, "fieldCodeNormal", value = cod.normal)
      env.report$codigo.reporte[[nombre.datos]] <<-
        paste0(env.report$codigo.reporte[[nombre.datos]],
               createLog(paste0("Test de Normalidad - ",
                                input$sel.normal), cod.normal))
      return(res)
    }, error = function(e){
      if(ncol(var.numericas(datos)) <= 0){
        error.variables(T)
      } else {
        showNotification(paste0("ERROR AL GENERAR TEST DE NORMALIDAD: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$run.normal, {
    updatePlot$normal <- input$fieldCodeNormal
  })

  observeEvent(c(input$loadButton, input$transButton,
                 input$sel.normal, input$col.normal), {
    updatePlot$normal <- default.normal(data = "datos", vars = input$sel.normal,
                                        color = input$col.normal)
  })

  #' Resumen Test de normalidad
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton), {
    output$calculo.normal = DT::renderDataTable({
      tryCatch({
        codigo <- updatePlot$calc.normal
        res <- isolate(eval(parse(text = codigo)))
        updateAceEditor(session, "fieldCalcNormal", value = codigo)
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR AL CALCULAR TEST DE NORMALIDAD: ", e),
                         duration = 10, type = "error")
        return(NULL)
      })
    }, options = list(scrollY = "60vh"))
  })

  observeEvent(input$run.calc.normal, {
    updatePlot$calc.normal <- input$fieldCalcNormal
  })

  #' Gráfico de Dispersión
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton), {

    output$plot.disp = renderPlot({
       tryCatch({
         cod.disp <<- updatePlot$disp
         updateAceEditor(session, "fieldCodeDisp", value = cod.disp)
         if(!is.null(cod.disp) && cod.disp != ""){
           env.report$codigo.reporte[[nombre.datos]] <<-
             paste0(env.report$codigo.reporte[[nombre.datos]],
                    createLog(paste0("Dispersión - ",
                                     paste(input$select.var, collapse = ", ")),
                              cod.disp))
         }
         return(isolate(eval(parse(text = cod.disp))))
       }, error = function(e) {
         if(ncol(var.numericas(datos)) <= 1){
           error.variables(T)
         } else {
           showNotification(paste0("ERROR AL GENERAR DISPERSIÓN: ", e),
                            duration = 10, type = "error")
           return(NULL)
         }
       })
    })

    output$plot.disp.zoom <- renderPlot({
      tryCatch({
        cod.disp <<- updatePlot$disp
        res <- isolate(eval(parse(text = cod.disp)))
        res <- res + coord_cartesian(xlim = disp.ranges$x,
                                     ylim = disp.ranges$y, expand = FALSE)
        return(res)
      }, error = function(e) {
        return(NULL)
      })
    })

    output$mostrar.disp.zoom = DT::renderDataTable({
      tryCatch({
        return(brushedPoints(datos[, input$select.var], input$zoom.disp))
      }, error = function(e) {
        return(NULL)
      })
    }, options = list(dom = 't', scrollX = TRUE, scrollY = "20vh",
                      pageLength = nrow(datos)))

  })

  observe({
    brush <- input$zoom.disp
    if (!is.null(brush)) {
      disp.ranges$x <- c(brush$xmin, brush$xmax)
      disp.ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      disp.ranges$x <- NULL
      disp.ranges$y <- NULL
    }
  })

  observeEvent(input$run.disp, {
    updatePlot$disp <- input$fieldCodeDisp
  })

  observeEvent(c(input$loadButton, input$transButton,
                 input$select.var, input$col.disp), {
    if(length(input$select.var) < 2) {
      updatePlot$disp <- ""
    } else {
      updatePlot$disp <- default.disp(data = "datos", vars = input$select.var,
                                       color = input$col.disp)
    }
  })

  #' Gráfico de PCA (Individuos)
  #' @author Diego
  #' @return plot
  #' @export
  #'

  output$plot.ind = renderPlot({
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      cod.pca.ind <<- updatePlot$pca.ind
      res <- isolate(eval(parse(text = cod.pca.ind)))
      updateAceEditor(session, "fieldCodeInd", value = cod.pca.ind)
      env.report$codigo.reporte[[nombre.datos]] <<-
        paste0(env.report$codigo.reporte[[nombre.datos]],
               createLog("Plano Principal", cod.pca.ind))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(T)
      } else {
        showNotification(paste0("ERROR EN PCA (Individuos): ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
   })

   output$plot.ind.zoom <- renderPlot({
     tryCatch({
       ejex <- ind.ranges$x
       ejey <- ind.ranges$y
       if(is.null(ejex) & is.null(ejey)){
         return(NULL)
       } else {
         cod.ind <<- updatePlot$pca.ind
         res <- isolate(eval(parse(text = cod.ind)))
         res <- res + coord_cartesian(xlim = ejex, ylim = ejey, expand = FALSE)
         return(res)
       }
     }, error = function(e) {
       return(NULL)
     })
   })

   output$mostrar.ind.zoom = DT::renderDataTable({
      tryCatch({
        dimensiones <- as.data.frame(pca.modelo$ind$coord)
        return(brushedPoints(df = dimensiones[, c(input$slider.ejes)],
                             brush = input$zoom.ind,
                             xvar = names(dimensiones)[input$slider.ejes[1]],
                             yvar = names(dimensiones)[input$slider.ejes[2]]))
      }, error = function(e) {
        print(e)
        return(NULL)
      })
   }, options = list(dom = 't', scrollX = TRUE, scrollY = "20vh",
                     pageLength = nrow(datos)))

  observe({
    brush <- input$zoom.ind
    if (!is.null(brush)) {
      ind.ranges$x <- c(brush$xmin, brush$xmax)
      ind.ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      ind.ranges$x <- NULL
      ind.ranges$y <- NULL
    }
  })

  observeEvent(input$run.pcaInd, {
    updatePlot$pca.ind <- input$fieldCodeInd
  })

  observeEvent(c(input$col.pca.ind, input$ind.cos, input$slider.ejes), {
    updatePlot$pca.ind <-
      pca.individuos(ind.cos = input$ind.cos * 0.01, color = input$col.pca.ind,
                     ejes = input$slider.ejes)
  })

  #' Gráfico de PCA (Variables)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.var = renderPlot({
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      cod.pca.var <<- updatePlot$pca.var
      res <- isolate(eval(parse(text = cod.pca.var)))
      updateAceEditor(session, "fieldCodeVar", value = cod.pca.var)
      env.report$codigo.reporte[[nombre.datos]] <<-
        paste0(env.report$codigo.reporte[[nombre.datos]],
               createLog("Circulo de Correlaciones", cod.pca.var))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(T)
      } else {
        showNotification(paste0("ERROR EN PCA (Variables): ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$run.pcaVar, {
    updatePlot$pca.var <- input$fieldCodeVar
  })

  observeEvent(c(input$var.cos, input$col.pca.var, input$slider.ejes), {
    updatePlot$pca.var <-
      pca.variables(var.cos = input$var.cos * 0.01, color = input$col.pca.var,
                    ejes = input$slider.ejes)
  })

  #' Gráfico de PCA (Sobreposición)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.biplot = renderPlot({
      tryCatch({
        pca.modelo <<- updateData$pca.modelo
        cod.pca.bi <<- updatePlot$pca.bi
        res <- isolate(eval(parse(text = cod.pca.bi)))
        updateAceEditor(session, "fieldCodeBi", value = cod.pca.bi)
        env.report$codigo.reporte[[nombre.datos]] <<-
          paste0(env.report$codigo.reporte[[nombre.datos]],
                 createLog("Sobreposición del Circulo y el Plano", cod.pca.bi))
        return(res)
      }, error = function(e) {
        if(ncol(var.numericas(datos)) <= 1){
          error.variables(T)
        } else {
          showNotification(paste0("ERROR EN PCA (Sobreposición): ", e),
                           duration = 10, type = "error")
          return(NULL)
        }
      })
    })

    output$plot.bi.zoom <- renderPlot({
      tryCatch({
        ejex <- bi.ranges$x
        ejey <- bi.ranges$y
        if(is.null(ejex) & is.null(ejey)){
          return(NULL)
        } else {
          cod.bi <<- updatePlot$pca.bi
          res <- isolate(eval(parse(text = cod.bi)))
          res <- res + coord_cartesian(xlim = ejex, ylim = ejey,
                                       expand = FALSE)
          return(res)
        }
      }, error = function(e) {
        return(NULL)
      })
    })

    output$mostrar.bi.zoom = DT::renderDataTable({
      tryCatch({
        dimensiones <- as.data.frame(pca.modelo$ind$coord)
        return(brushedPoints(df = dimensiones[, c(input$slider.ejes)],
                             brush = input$zoom.bi,
                             xvar = names(dimensiones)[input$slider.ejes[1]],
                             yvar = names(dimensiones)[input$slider.ejes[2]]))
      }, error = function(e) {
        print(e)
        return(NULL)
      })
    }, options = list(dom = 't', scrollX = TRUE, scrollY = "20vh",
                      pageLength = nrow(datos)))

  observe({
    brush <- input$zoom.bi
    if (!is.null(brush)) {
      bi.ranges$x <- c(brush$xmin, brush$xmax)
      bi.ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      bi.ranges$x <- NULL
      bi.ranges$y <- NULL
    }
  })

  observeEvent(input$run.pcaBi, {
    updatePlot$pca.bi <- input$fieldCodeBi
  })

  observeEvent(c(input$col.pca.ind, input$ind.cos, input$var.cos,
                 input$col.pca.var, input$slider.ejes), {
    updatePlot$pca.bi <-
      pca.sobreposicion(ind.cos = input$ind.cos * 0.01,
                        var.cos = input$var.cos * 0.01,
                        col.ind = input$col.pca.ind,
                        col.var = input$col.pca.var,
                        ejes = input$slider.ejes)
  })

  #' Gráfico de PCA (Varianza Explicada para cada Eje)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plotVEE = renderPlot({
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      codigo <- code.pca.vee()
      updateAceEditor(session, "fieldCodeVEE", value = codigo)
      res <- isolate(eval(parse(text = codigo)))
      env.report$codigo.reporte[[nombre.datos]] <<-
        paste0(env.report$codigo.reporte[[nombre.datos]],
               createLog("Varianza Explicada para cada Eje", codigo))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1) {
        error.variables(T)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  #' Gráfico de PCA (Cosenos Cuadrados de los individuos)
  #' @author Diego
  #' @return plot
  #' @export

  output$plotCCI = renderPlot({
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      codigo <- code.pca.cci()
      updateAceEditor(session, "fieldCodeCCI", value = codigo)
      res <- isolate(eval(parse(text = codigo)))
      env.report$codigo.reporte[[nombre.datos]] <<-
        paste0(env.report$codigo.reporte[[nombre.datos]],
               createLog("Cosenos Cuadrados de los individuos", codigo))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(T)
      } else {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(c(input$loadButton, input$transButton,
                 input$switch.scale, input$slider.npc), {
    updatePlot$pca.cci <- code.pca.cci()
  })

  #' Gráfico de PCA (Cosenos Cuadrados de las Variables)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plotCCV = renderPlot({
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      codigo <- code.pca.ccv()
      updateAceEditor(session, "fieldCodeCCV", value = codigo)
      res <- isolate(eval(parse(text = codigo)))
      env.report$codigo.reporte[[nombre.datos]] <<-
        paste0(env.report$codigo.reporte[[nombre.datos]],
               createLog("Cosenos Cuadrados de las Variables", codigo))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(T)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  #' Gráfico de PCA (Correlación Variables con los Componenetes)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plotCVC = renderPlot({
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      codigo <- updatePlot$pca.cvc
      updateAceEditor(session, "fieldCodeCVC", value = codigo)
      res <- isolate(eval(parse(text = codigo)))
      env.report$codigo.reporte[[nombre.datos]] <<-
        paste0(env.report$codigo.reporte[[nombre.datos]],
               createLog("Correlación Variables con los Componenetes", codigo))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(T)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$cvc.metodo, {
    updatePlot$pca.cvc <- code.pca.cvp(input$cvc.metodo)
  })

  #' Gráfico de PCA (Contribución de las variables de la Dimensión 1)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plotPC1 = renderPlot({
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      codigo <- code.pca.pc1()
      updateAceEditor(session, "fieldCodePC1", value = codigo)
      res <- isolate(eval(parse(text = codigo)))
      env.report$codigo.reporte[[nombre.datos]] <<-
        paste0(env.report$codigo.reporte[[nombre.datos]],
               createLog("Contribución de las variables de la Dimensión 1", codigo))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(T)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  #' Gráfico de PCA (Contribución de las variables de la Dimensión 2)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plotPC2 = renderPlot({
    tryCatch({
      pca.modelo <<- updateData$pca.modelo
      codigo <- code.pca.pc2()
      updateAceEditor(session, "fieldCodePC2", value = codigo)
      res <- isolate(eval(parse(text = codigo)))
      env.report$codigo.reporte[[nombre.datos]] <<-
        paste0(env.report$codigo.reporte[[nombre.datos]],
               createLog("Contribución de las variables de la Dimensión 2", codigo))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(T)
      } else {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  #' Gráfico de Correlaciones
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.cor = renderPlot({
    tryCatch({
      updateData$cor.modelo
      cod.cor <- updatePlot$cor
      res <- isolate(eval(parse(text = cod.cor)))
      updateAceEditor(session, "fieldCodeCor", value = cod.cor)
      env.report$codigo.reporte[[nombre.datos]] <<-
        paste0(env.report$codigo.reporte[[nombre.datos]],
               createLog("Matriz de Correlaciones", cod.cor))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(T)
      } else {
        showNotification(paste0("ERROR EN Correlacion: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$run.code.cor, {
    updatePlot$cor <- input$fieldCodeCor
  })

  observeEvent(c(input$cor.metodo, input$cor.tipo), {
    updatePlot$cor <- correlaciones(metodo = input$cor.metodo,
                                    tipo = input$cor.tipo)
  })

  #' Gráfico de Distribuciones (Númericas)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.num = renderPlot({
    tryCatch({
      cod.dya.num  <<- updatePlot$dya.num
      res <- isolate(eval(parse(text = cod.dya.num)))
      updateAceEditor(session, "fieldCodeNum", value = cod.dya.num)
      env.report$codigo.reporte[[nombre.datos]] <<-
        paste0(env.report$codigo.reporte[[nombre.datos]],
               createLog(paste0("Distribución - ",
                                input$sel.distribucion.num), cod.dya.num))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 0){
        error.variables(T)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$run.dya.num, {
    updatePlot$dya.num <- input$fieldCodeNum
  })

  observeEvent(c(input$loadButton, input$transButton,
                 input$sel.distribucion.num, input$col.dist), {
    updatePlot$dya.num <-
      def.code.num(data = "datos", color = paste0("'", input$col.dist, "'"),
                   variable = paste0("'", input$sel.distribucion.num, "'"))
  })

  output$mostrar.atipicos = DT::renderDataTable({
    atipicos <- boxplot.stats(datos[, input$sel.distribucion.num])
    datos <- datos[datos[, input$sel.distribucion.num] %in% atipicos$out,
                   input$sel.distribucion.num, drop = F]
    datos <- datos[order(datos[, input$sel.distribucion.num]), , drop = F]
    datatable(datos, options = list(dom = 't', scrollX = TRUE, scrollY = "28vh",
                                      pageLength = nrow(datos))) %>%
      formatStyle(1, color = "white", backgroundColor = "#CBB051", target = "row")
  })

  #' Gráfico de Distribuciones (Categóricas)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.cat = renderPlot({
    tryCatch({
      cod.dya.cat  <<- updatePlot$dya.cat
      res <- isolate(eval(parse(text = cod.dya.cat)))
      updateAceEditor(session, "fieldCodeCat", value = cod.dya.cat)
      env.report$codigo.reporte[[nombre.datos]] <<-
        paste0(env.report$codigo.reporte[[nombre.datos]],
               createLog(paste0("Distribución - ",
                                input$sel.distribucion.cat), cod.dya.cat))
      return(res)
    }, error = function(e) {
      if(ncol(var.categoricas(datos)) <= 0){
        error.variables(F)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$run.dya.cat, {
    updatePlot$dya.cat <- input$fieldCodeCat
  })

  observeEvent(c(input$loadButton, input$transButton,
                 input$sel.distribucion.cat), {
    updatePlot$dya.cat <-
      def.code.cat(data = "datos",
                   variable = paste0("'", input$sel.distribucion.cat, "'"))
  })

  #' Inercia Clusterización Jerarquica
  #' @author Diego
  #' @return plot
  #' @export
  #'
  #'
  output$inercia.cj = renderUI({
    return(HTML(panel.inercia(modelo = updateData$hc.modelo$modelo,
                              cant.clusters = as.numeric(input$cant.cluster))))
  })

  #' Actualización del Modelo Clusterización Jerarquica
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton, input$cant.cluster,
                 input$sel.dist.method, input$sel.hc.method), {
    codigo <- def.model(data = "datos", cant = input$cant.cluster,
                        dist.method = input$sel.dist.method,
                        hc.method = input$sel.hc.method)
    tryCatch ({
      if(!is.null(datos) && !is.null(input$cant.cluster)){
        isolate(eval(parse(text = codigo)))
        updateData$hc.modelo <- hc.modelo
        updateAceEditor(session, "fieldCodeModelo", value = codigo)
        output$txthc <- renderPrint(print(unclass(hc.modelo)))
        output$txtcentros <- renderPrint(print(unclass(centros)))
        env.report$codigo.reporte[[nombre.datos]] <<-
          paste0(env.report$codigo.reporte[[nombre.datos]],
                 "\n\n## Clusterización Jerarquica (Clusters = ",
                 input$cant.cluster, ", Distancia = ",
                 input$sel.dist.method, ", Selección = ",
                 input$sel.hc.method, ")\n\n```{r}\n", codigo, "\n```")
      }
    }, error = function(e) {
      print(paste0("ERROR EN HC: ", e))
      return(NULL)
    })

    if(!is.null(datos) && !is.null(input$cant.cluster)) {
      nuevos.colores <- sapply(1:input$cant.cluster, function(i)
        paste0("'", input[[paste0("hcColor", i)]], "'"))
      color <- ifelse(input$selHoriz %in% c("", "Todos"), "red",
                      nuevos.colores[as.numeric(input$selHoriz)])
      updatePlot$diag <<- diagrama(cant = input$cant.cluster,
                                   colores = nuevos.colores)
      updatePlot$mapa <<- cluster.mapa(colores = nuevos.colores)
      updatePlot$horiz <<- cluster.horiz(sel = input$selHoriz,
                                         colores = nuevos.colores, color = color)
      updatePlot$vert <<- cluster.vert(sel = input$selVert,
                                       colores = nuevos.colores)
      updatePlot$radar <<- cluster.radar(colores = nuevos.colores)
      updatePlot$cat <<- cluster.cat(var = input$selBar,
                                     colores = nuevos.colores)
      updateAceEditor(session, "fieldCodeDendo", value = updatePlot$diag)
      updateAceEditor(session, "fieldCodeMapa", value = updatePlot$mapa)
      updateAceEditor(session, "fieldCodeHoriz", value = updatePlot$horiz)
      updateAceEditor(session, "fieldCodeVert", value = updatePlot$vert)
      updateAceEditor(session, "fieldCodeRadar", value = updatePlot$radar)
      updateAceEditor(session, "fieldCodeBar", value = updatePlot$cat)
    }
  })

  #' Gráfico de Clusterización Jerarquica (Diagrama)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.diag = renderPlot({
    tryCatch({
      hc.modelo <<- updateData$hc.modelo
      code.diagrama <<- updatePlot$diag
      res <- isolate(eval(parse(text = code.diagrama)))
      env.report$codigo.reporte[[nombre.datos]] <<-
        paste0(env.report$codigo.reporte[[nombre.datos]],
               createLog("Dendograma", code.diagrama))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(T)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$run.hcDendo, {
    updatePlot$diag <- input$fieldCodeDendo
  })

  #' Gráfico de Clusterización Jerarquica (Mapa)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.mapa = renderPlot({
    tryCatch({
      hc.modelo <<- updateData$hc.modelo
      pca.modelo <<- updateData$pca.modelo
      code.mapa <<- updatePlot$mapa
      res <- isolate(eval(parse(text = code.mapa)))
      env.report$codigo.reporte[[nombre.datos]] <<-
        paste0(env.report$codigo.reporte[[nombre.datos]],
               createLog("", code.mapa))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(T)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  output$plot.mapa.zoom <- renderPlot({
    tryCatch({
      ejex <- mapa.ranges$x
      ejey <- mapa.ranges$y
      if(is.null(ejex) & is.null(ejey)){
        return(NULL)
      } else {
        cod.mapa <<- updatePlot$mapa
        res <- isolate(eval(parse(text = cod.mapa)))
        res <- res + coord_cartesian(xlim = ejex, ylim = ejey, expand = FALSE) +
          theme(legend.position="none")
        return(res)
      }
    }, error = function(e) {
      return(NULL)
    })
  })

  output$mostrar.mapa.zoom = DT::renderDataTable({
    tryCatch({
      dimensiones <- as.data.frame(pca.modelo$ind$coord)
      return(brushedPoints(df = dimensiones[, c(input$slider.ejes)],
                           brush = input$zoom.mapa,
                           xvar = names(dimensiones)[input$slider.ejes[1]],
                           yvar = names(dimensiones)[input$slider.ejes[2]]))
    }, error = function(e) {
      print(e)
      return(NULL)
    })
  }, options = list(dom = 't', scrollX = TRUE, scrollY = "20vh",
                    pageLength = nrow(datos)))

  observe({
    brush <- input$zoom.mapa
    if (!is.null(brush)) {
      mapa.ranges$x <- c(brush$xmin, brush$xmax)
      mapa.ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      mapa.ranges$x <- NULL
      mapa.ranges$y <- NULL
    }
  })

  observeEvent(input$run.hcMapa, {
    updatePlot$mapa <- input$fieldCodeMapa
  })

  #' Gráfico de Clusterización Jerarquica (Horizontal)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.horiz = renderPlot({
    tryCatch({
      hc.modelo <<- updateData$hc.modelo
      code.horiz <<- updatePlot$horiz
      res <- isolate(eval(parse(text = code.horiz)))
      env.report$codigo.reporte[[nombre.datos]] <<-
        paste0(env.report$codigo.reporte[[nombre.datos]],
               createLog("Interpretación Horizontal", code.horiz))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(T)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$run.hcHoriz, {
    updatePlot$horiz <- input$fieldCodeHoriz
  })

  observeEvent(input$selHoriz, {
    if(!is.null(datos)){
      nuevos.colores <- sapply(1:input$cant.cluster, function(i)
        paste0("'", input[[paste0("hcColor", i)]], "'"))
      color <- ifelse(input$selHoriz %in% c("", "Todos"), "red",
                      nuevos.colores[as.numeric(input$selHoriz)])
      updatePlot$horiz <<- cluster.horiz(sel = input$selHoriz,
                                         colores = nuevos.colores, color = color)
      updateAceEditor(session, "fieldCodeHoriz", value = updatePlot$horiz)
    }
  })

  #' Gráfico de Clusterización Jerarquica (Vertical)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.vert = renderPlot({
    tryCatch({
      hc.modelo <<- updateData$hc.modelo
      code.vert <<- updatePlot$vert
      res <- isolate(eval(parse(text = code.vert)))
      env.report$codigo.reporte[[nombre.datos]] <<-
        paste0(env.report$codigo.reporte[[nombre.datos]],
               createLog("Interpretación Vertical", code.vert))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(T)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$run.hcVert, {
    updatePlot$vert <- input$fieldCodeVert
  })

  observeEvent(input$selVert, {
    if(!is.null(datos) && !is.null(input$cant.cluster)) {
      nuevos.colores <- sapply(1:input$cant.cluster, function(i)
        paste0("'", input[[paste0("hcColor", i)]], "'"))
      updatePlot$vert <<- cluster.vert(sel = input$selVert,
                                       colores = nuevos.colores)
      updateAceEditor(session, "fieldCodeVert", value = updatePlot$vert)
    }
  })

  #' Gráfico de Clusterización Jerarquica (Radar)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.radar = renderPlot({
    tryCatch({
      hc.modelo <<- updateData$hc.modelo
      code.radar <<- updatePlot$radar
      res <- isolate(eval(parse(text = code.radar)))
      env.report$codigo.reporte[[nombre.datos]] <<-
        paste0(env.report$codigo.reporte[[nombre.datos]],
               createLog("Gráfico Radar", code.radar))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(T)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$run.hcRadar, {
    updatePlot$radar <- input$fieldCodeRadar
  })

  #' Gráfico de Clusterización Jerarquica (Categóricas)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.bar.cat = renderPlot({
    tryCatch({
      hc.modelo <<- updateData$hc.modelo
      code.cat <<- updatePlot$cat
      res <- isolate(eval(parse(text = code.cat)))
      env.report$codigo.reporte[[nombre.datos]] <<-
        paste0(env.report$codigo.reporte[[nombre.datos]],
               createLog("Interpretación Variables Categóricas", code.cat))
      return(res)
    }, warning = function(w) {
      showNotification(paste0("ADVERTENCIA: ", w), duration = 10,
                       type = "warning")
      return(NULL)
    }, error = function(e) {
      if(ncol(var.categoricas(datos)) <= 0){
        error.variables(F)
      } else {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$run.hcBar, {
    updatePlot$cat <- input$fieldCodeBar
  })

  observeEvent(input$selBar, {
    if(!is.null(datos)){
      nuevos.colores <- sapply(1:input$cant.cluster, function(i)
        paste0("'", input[[paste0("hcColor", i)]], "'"))
      updatePlot$cat <<- cluster.cat(var = input$selBar,
                                     colores = nuevos.colores)
      updateAceEditor(session, "fieldCodeBar", value = updatePlot$cat)
    }
  })


  #' Actializacion del Modelo K-medias
  #' @author Diego
  #' @return plot
  #' @export
  #'
  #'
  observeEvent(c(input$loadButton, input$transButton, input$cant.kmeans.cluster,
                 input$num.iter, input$num.nstart, input$sel.algoritmo), {
    tryCatch ({
      codigo <- def.k.model(data = "datos", cant = input$cant.kmeans.cluster,
                            iter.max = input$num.iter,
                            nstart = input$num.nstart,
                            algorithm = input$sel.algoritmo)
      isolate(eval(parse(text = codigo)))
      updateData$k.modelo <- k.modelo
      updateAceEditor(session, "fieldCodeKModelo", value = codigo)
      output$txtk <- renderPrint(print(unclass(k.modelo)))
      env.report$codigo.reporte[[nombre.datos]] <<-
        paste0(env.report$codigo.reporte[[nombre.datos]],
               "\n\n## K-medias (Clusters = ", input$cant.kmeans.cluster,
               ", Formas Fuertes = ", input$num.nstart,
               ", Iteraciones = ", input$num.iter, ", Algoritmo = ",
               input$sel.algoritmo, ")\n\n```{r}\n",
               codigo, "\n```")
    }, error = function(e) {
      return(NULL)
    })

    if(!is.null(datos) && !is.null(input$cant.kmeans.cluster)) {
      nuevos.colores <- sapply(1:input$cant.kmeans.cluster, function(i)
        paste0("'", input[[paste0("kColor", i)]], "'"))
      color <- ifelse(input$sel.Khoriz %in% c("", "Todos"), "red",
                      nuevos.colores[as.numeric(input$sel.Khoriz)])
      updatePlot$jambu <<- def.code.jambu(k = input$iteracionesK)
      updatePlot$kmapa <<- cluster.mapa(esHC = F, colores = nuevos.colores)
      updatePlot$khoriz <<- cluster.horiz(esHC = F, sel = input$sel.Khoriz,
                                           colores = nuevos.colores, color = color)
      updatePlot$kvert <<- cluster.vert(esHC = F, sel = input$sel.Kvert,
                                         colores = nuevos.colores)
      updatePlot$kradar <<- cluster.radar(esHC = F, colores = nuevos.colores)
      updatePlot$kcat <<- cluster.cat(esHC = F, var = input$sel.Kbar,
                                       colores = nuevos.colores)
      updateAceEditor(session, "fieldCodeJambu", value = updatePlot$jambu)
      updateAceEditor(session, "fieldCodeKmapa", value = updatePlot$kmapa)
      updateAceEditor(session, "fieldCodeKhoriz", value = updatePlot$khoriz)
      updateAceEditor(session, "fieldCodeKvert", value = updatePlot$kvert)
      updateAceEditor(session, "fieldCodeKradar", value = updatePlot$kradar)
      updateAceEditor(session, "fieldCodeKbar", value = updatePlot$kcat)
    }
  })

  #' Inercia K-medias
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$inercia.k = renderUI({
    k.modelo <<- updateData$k.modelo
    return(HTML(panel.inercia(esHC = F, k.modelo)))
  })

  #' Gráfico de K-medias (Codo de Jambu)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.jambu = renderPlot({
    tryCatch({
      code.jambu <<- updatePlot$jambu
      isolate(eval(parse(text = code.jambu)))
      updateAceEditor(session, "fieldCodeJambu", value = code.jambu)
      res <- isolate(eval(parse(text = code.jambu)))
      env.report$codigo.reporte[[nombre.datos]] <<-
        paste0(env.report$codigo.reporte[[nombre.datos]],
               createLog("Codo de Jambu", code.jambu))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(T)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$run.Jambu, {
    updatePlot$jambu <- input$fieldCodeJambu
  })

  observeEvent(c(input$loadButton, input$transButton, input$iteracionesK), {
    updatePlot$jambu <- def.code.jambu(k = input$iteracionesK)
  })

  #' Gráfico de K-medias (Mapa)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.kmapa = renderPlot({
    tryCatch({
      k.modelo <<- updateData$k.modelo
      pca.modelo <<- updateData$pca.modelo
      code.kmapa <<- updatePlot$kmapa
      res <- isolate(eval(parse(text = code.kmapa)))
      env.report$codigo.reporte[[nombre.datos]] <<-
        paste0(env.report$codigo.reporte[[nombre.datos]],
               createLog("Interpretación Mapa", code.kmapa))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(T)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  output$plot.kmapa.zoom <- renderPlot({
    tryCatch({
      ejex <- kmapa.ranges$x
      ejey <- kmapa.ranges$y
      if(is.null(ejex) & is.null(ejey)){
        return(NULL)
      } else {
        cod.kmapa <<- updatePlot$kmapa
        res <- isolate(eval(parse(text = cod.kmapa)))
        res <- res + coord_cartesian(xlim = ejex, ylim = ejey, expand = FALSE) +
          theme(legend.position="none")
        return(res)
      }
    }, error = function(e) {
      return(NULL)
    })
  })

  output$mostrar.kmapa.zoom = DT::renderDataTable({
    tryCatch({
      dimensiones <- as.data.frame(pca.modelo$ind$coord)
      return(brushedPoints(df = dimensiones[, c(input$slider.ejes)],
                           brush = input$zoom.kmapa,
                           xvar = names(dimensiones)[input$slider.ejes[1]],
                           yvar = names(dimensiones)[input$slider.ejes[2]]))
    }, error = function(e) {
      print(e)
      return(NULL)
    })
  }, options = list(dom = 't', scrollX = TRUE, scrollY = "20vh",
                    pageLength = nrow(datos)))

  observe({
    brush <- input$zoom.kmapa
    if (!is.null(brush)) {
      kmapa.ranges$x <- c(brush$xmin, brush$xmax)
      kmapa.ranges$y <- c(brush$ymin, brush$ymax)

    } else {
      kmapa.ranges$x <- NULL
      kmapa.ranges$y <- NULL
    }
  })

  observeEvent(input$run.Kmapa, {
    updatePlot$kmapa <- input$fieldCodeKmapa
  })

  #' Gráfico de K-medias (Horizontal)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.khoriz = renderPlot({
    tryCatch({
      k.modelo <<- updateData$k.modelo
      code.khoriz <<- updatePlot$khoriz
      res <- isolate(eval(parse(text = code.khoriz)))
      env.report$codigo.reporte[[nombre.datos]] <<-
        paste0(env.report$codigo.reporte[[nombre.datos]],
               createLog("Interpretación Horizontal", code.khoriz))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(T)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$run.Khoriz, {
    updatePlot$khoriz <- input$fieldCodeKhoriz
  })

  observeEvent(input$sel.Khoriz, {
    if(!is.null(datos)){
      nuevos.colores <- sapply(1:input$cant.kmeans.cluster, function(i)
        paste0("'", input[[paste0("kColor", i)]], "'"))
      color <- ifelse(input$sel.Khoriz %in% c("", "Todos"), "red",
                      nuevos.colores[as.numeric(input$sel.Khoriz)])
      updatePlot$khoriz <- cluster.horiz(esHC = F, sel = input$sel.Khoriz,
                                         colores = nuevos.colores, color = color)
      updateAceEditor(session, "fieldCodeKhoriz", value = updatePlot$khoriz)
    }
  })

  #' Gráfico de K-medias (Vertical)
  #' @author Diego
  #' @return plot
  #' @export
  #'
    output$plot.kvert = renderPlot({
      tryCatch({
        k.modelo <<- updateData$k.modelo
        code.kvert <<- updatePlot$kvert
        res <- isolate(eval(parse(text = code.kvert)))
        env.report$codigo.reporte[[nombre.datos]] <<-
          paste0(env.report$codigo.reporte[[nombre.datos]],
                 createLog("Interpretación Vertical", code.kvert))
        return(res)
      }, error = function(e) {
        if(ncol(var.numericas(datos)) <= 1){
          error.variables(T)
        } else {
          showNotification(paste0("ERROR: ", e),
                           duration = 10, type = "error")
          return(NULL)
        }
      })
    })

  observeEvent(input$run.KVert, {
    updatePlot$kvert <- input$fieldCodeKvert
  })

  observeEvent(input$sel.Kvert, {
    if(!is.null(datos) && !is.null(input$cant.kmeans.cluster)) {
      nuevos.colores <- sapply(1:input$cant.kmeans.cluster, function(i)
        paste0("'", input[[paste0("kColor", i)]], "'"))
      updatePlot$kvert <<- cluster.vert(esHC = F, sel = input$sel.Kvert,
                                         colores = nuevos.colores)
      updateAceEditor(session, "fieldCodeKvert", value = updatePlot$kvert)
    }
  })

  #' Gráfico de K-medias (Radar)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.kradar = renderPlot({
    tryCatch({
      k.modelo <<- updateData$k.modelo
      code.kradar <<- updatePlot$kradar
      res <- isolate(eval(parse(text = code.kradar)))
      env.report$codigo.reporte[[nombre.datos]] <<-
        paste0(env.report$codigo.reporte[[nombre.datos]],
               createLog("Interpretación Radar", code.kradar))
      return(res)
    }, error = function(e) {
      if(ncol(var.numericas(datos)) <= 1){
        error.variables(T)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$run.Kradar, {
    updatePlot$kradar <- input$fieldCodeKradar
  })

  #' Gráfico de K-medias (Categórico)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  output$plot.kcat = renderPlot({
    tryCatch({
      k.modelo <<- updateData$k.modelo
      code.kcat <<- updatePlot$kcat
      res <- isolate(eval(parse(text = code.kcat)))
      env.report$codigo.reporte[[nombre.datos]] <<-
        paste0(env.report$codigo.reporte[[nombre.datos]],
               createLog("Interpretación Variables Categóricas", code.kcat))
      return(res)
    }, error = function(e) {
      if(ncol(var.categoricas(datos)) <= 0){
        error.variables(F)
      } else {
        showNotification(paste0("ERROR: ", e),
                         duration = 10, type = "error")
        return(NULL)
      }
    })
  })

  observeEvent(input$run.Kbar, {
    updatePlot$kcat <- input$fieldCodeKbar
  })

  observeEvent(input$sel.Kbar, {
    if(!is.null(datos)){
      nuevos.colores <- sapply(1:input$cant.kmeans.cluster, function(i)
        paste0("'", input[[paste0("kColor", i)]], "'"))
      updatePlot$kcat <<- cluster.cat(esHC = F, var = input$sel.Kbar,
                                       colores = nuevos.colores)
      updateAceEditor(session, "fieldCodeKbar", value = updatePlot$kcat)
    }
  })

  #' Mostrar Colores (k-means)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$cant.kmeans.cluster), {
    if(!is.null(datos) && !is.null(input$cant.kmeans.cluster)) {
      updateSelectInput(session, "sel.Khoriz",
                        choices = c("Todos", 1:input$cant.kmeans.cluster))
      for (i in 1:10) {
        if(i <= input$cant.kmeans.cluster) {
          shinyjs::show(paste0("kColor", i))
        } else {
          shinyjs::hide(paste0("kColor", i))
        }
      }
    }
  })

  #' Mostrar Colores (Cluster jerarquico)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$cant.cluster), {
    if(!is.null(datos) && !is.null(input$cant.cluster)){
      updateSelectInput(session, "selHoriz",
                        choices = c("Todos", 1:input$cant.cluster))
      for (i in 1:10) {
        if(i <= input$cant.cluster) {
          shinyjs::show(paste0("hcColor", i))
        } else {
          shinyjs::hide(paste0("hcColor", i))
        }
      }
    }
  })

  observeEvent(c(input$hcColor1, input$hcColor2, input$hcColor3, input$hcColor4,
                 input$hcColor5, input$hcColor6, input$hcColor7, input$hcColor8,
                 input$hcColor9, input$hcColor10), {
    if(!is.null(datos)){
      nuevos.colores <- sapply(1:input$cant.cluster, function(i)
        paste0("'", input[[paste0("hcColor", i)]], "'"))
      color <- ifelse(input$selHoriz %in% c("", "Todos"), "red",
                      nuevos.colores[as.numeric(input$selHoriz)])
      updatePlot$diag <<- diagrama(cant = input$cant.cluster,
                                   colores = nuevos.colores)
      updatePlot$mapa <<- cluster.mapa(colores = nuevos.colores)
      updatePlot$horiz <<- cluster.horiz(sel = input$selHoriz,
                                         colores = nuevos.colores, color = color)
      updatePlot$vert <<- cluster.vert(sel = input$selVert,
                                       colores = nuevos.colores)
      updatePlot$radar <<- cluster.radar(colores = nuevos.colores)
      updatePlot$cat <<- cluster.cat(var = input$selBar,
                                     colores = nuevos.colores)
      updateAceEditor(session, "fieldCodeDendo", value = updatePlot$diag)
      updateAceEditor(session, "fieldCodeMapa", value = updatePlot$mapa)
      updateAceEditor(session, "fieldCodeHoriz", value = updatePlot$horiz)
      updateAceEditor(session, "fieldCodeVert", value = updatePlot$vert)
      updateAceEditor(session, "fieldCodeRadar", value = updatePlot$radar)
      updateAceEditor(session, "fieldCodeBar", value = updatePlot$cat)
    }
  })

  observeEvent(c(input$kColor1, input$kColor2, input$kColor3, input$kColor4,
                 input$kColor5, input$kColor6, input$kColor7, input$kColor8,
                 input$kColor9, input$kColor10), {
    if(!is.null(datos)){
      nuevos.colores <- sapply(1:input$cant.kmeans.cluster, function(i)
        paste0("'", input[[paste0("kColor", i)]], "'"))
      color <- ifelse(input$sel.Khoriz %in% c("", "Todos"), "red",
                      nuevos.colores[as.numeric(input$sel.Khoriz)])
      updatePlot$kmapa <<- cluster.mapa(esHC = F, colores = nuevos.colores)
      updatePlot$khoriz <<- cluster.horiz(esHC = F, sel = input$sel.Khoriz,
                                           colores = nuevos.colores, color = color)
      updatePlot$kvert <<- cluster.vert(esHC = F, sel = input$sel.Kvert,
                                         colores = nuevos.colores)
      updatePlot$kradar <<- cluster.radar(esHC = F, colores = nuevos.colores)
      updatePlot$kcat <<- cluster.cat(esHC = F, var = input$sel.Kbar,
                                       colores = nuevos.colores)
      updateAceEditor(session, "fieldCodeKmapa", value = updatePlot$kmapa)
      updateAceEditor(session, "fieldCodeKhoriz", value = updatePlot$khoriz)
      updateAceEditor(session, "fieldCodeKvert", value = updatePlot$kvert)
      updateAceEditor(session, "fieldCodeKradar", value = updatePlot$kradar)
      updateAceEditor(session, "fieldCodeKbar", value = updatePlot$kcat)
    }
  })

  observeEvent(input$HCbutton, {
    C.Jerarquica <- hc.modelo$clusters
    datos <<- cbind(datos, C.Jerarquica)
    datos$C.Jerarquica <<- paste0("CJ", datos$C.Jerarquica)
    datos$C.Jerarquica <<- as.factor(datos$C.Jerarquica)
    output$contents = DT::renderDT(mostrarData(), server = F)
    showNotification("Los clústeres fueron correctamente agregados.",
                     duration = 5, type = "message")
    updateSelectInput(session, "sel.distribucion.cat",
                      choices = colnames(var.categoricas(datos)))
  })

  observeEvent(input$Kbutton, {
    datos <<- cbind(datos, Kmedias = k.modelo$cluster)
    datos$Kmedias <<- paste0("K", datos$Kmedias)
    datos$Kmedias <<- as.factor(datos$Kmedias)
    output$contents = DT::renderDT(mostrarData(), server = F)
    showNotification("Los clústeres fueron correctamente agregados",
                     duration = 5, type = "message")
    updateSelectInput(session, "sel.distribucion.cat",
                      choices = colnames(var.categoricas(datos)))
  })

  output$downloaDatos <- downloadHandler(
    filename = function() {
      input$file1$name
    },
    content = function(file) {
      write.csv(datos, file, row.names = input$rowname)
    }
  )

  observeEvent(input$principal, {
    if(input$principal == "reporte"){
      updateAceEditor(session, "fieldCodeReport",
                      value = def.reporte(titulo = input$textTitulo,
                                          nombre = input$textNombre, input))
    }
  })

  observeEvent(input$textTitulo, {
    updateAceEditor(session, "fieldCodeReport", value =
                      str_replace(input$fieldCodeReport, "title: '.*'",
                                  paste0("title: '", input$textTitulo, "'")))
  })

  observeEvent(input$textNombre, {
    updateAceEditor(session, "fieldCodeReport", value =
                      str_replace(input$fieldCodeReport, "author: '.*'",
                                  paste0("author: '", input$textNombre, "'")))
  })

  output$descargar <- downloadHandler(
    filename = function() {
      paste(input$textTitulo,'-', input$textNombre, '.zip', sep='')
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;

      namermd <- paste(input$textTitulo,'-', input$textNombre, '.rmd', sep='')
      writeLines(input$fieldCodeReport, namermd)
      files <- c(namermd, files)

      src <- normalizePath(namermd)
      withCallingHandlers({
        overwrite.cat()
        salida.code <<- ""
        shinyjs::html("txtreport", salida.code)
        out <- rmarkdown::render(src,  params = NULL, rmarkdown::word_document(),
                                 envir = env.report)
      }, message = function(m) {
        salida.code <<- paste0(m$message, salida.code)
          shinyjs::html(id = "txtreport", html = salida.code)
        }
      )
      recover.cat()
      file.rename(out, paste(input$textTitulo,'-', input$textNombre, '.docx', sep=''))
      files <- c(paste(input$textTitulo,'-', input$textNombre, '.docx', sep=''), files)

      zip::zip(file, files)
    }
  )

  close.menu <- function(valor = T){
    select <- 'a[href^="#shiny-tab-parte1"]'
    if(valor){
      shinyjs::hide(selector = "ul.menu-open");
      shinyjs::disable(selector = select)
    }else{
      shinyjs::enable(selector = select)
    }
  }
})

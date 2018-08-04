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
  options(DT.options = list(aLengthMenu = c(10, 30, 50), iDisplayLength = 10, scrollX = TRUE))

  session$onSessionEnded(function() {
    borrar <- ls(envir = .GlobalEnv)
    borrar <- borrar[!(borrar %in% .GlobalEnv$foto)]
    rm(envir = .GlobalEnv, list = borrar)
    stopApp()
  })
  
  #' Carga Inicial
  #' @author Diego
  #' @return functions
  #' @export
  #'
  isolate(eval(parse(text = def.func.jambu())))
  isolate(eval(parse(text = func.dya.num)))
  isolate(eval(parse(text = func.dya.cat)))
  
  isolate(eval(parse(text = func.centros)))
  isolate(eval(parse(text = func.horiz)))
  isolate(eval(parse(text = func.vert)))
  isolate(eval(parse(text = func.radar)))
  
  isolate(eval(parse(text = func.khoriz)))
  isolate(eval(parse(text = func.kvert)))
  isolate(eval(parse(text = func.kradar)))
  
  updateAceEditor(session, "fieldModelCor", value = modelo.cor())
  updateAceEditor(session, "fieldFuncJambu", value = def.func.jambu())
  updateAceEditor(session, "fieldFuncNum", value = func.dya.num)
  updateAceEditor(session, "fieldFuncCat", value = func.dya.cat)

  updateAceEditor(session, "fieldCodeCentr", value = func.centros)
  updateAceEditor(session, "fieldFuncHoriz", value = func.horiz)
  updateAceEditor(session, "fieldFuncVert", value = func.vert)
  updateAceEditor(session, "fieldFuncRadar", value = func.radar)
  
  updateAceEditor(session, "fieldFuncKhoriz", value = func.khoriz)
  updateAceEditor(session, "fieldFuncKvert", value = func.kvert)
  updateAceEditor(session, "fieldFuncKradar", value = func.kradar)
  
  
  updatePlot <- reactiveValues(calc.normal=default.calc.normal(), normal=NULL, disp=NULL, pca.ind=NULL, pca.var=NULL, pca.bi=NULL, cor=NULL,
                               pca.vee=NULL, pca.cci=NULL, pca.ccv=NULL, pca.cvc=NULL, pca.pc1=NULL, pca.pc2=NULL,
                               dya.num=NULL, dya.cat=NULL, diag=NULL, mapa=NULL, horiz=NULL, vert=NULL, radar=NULL,
                               cat=NULL, jambu=NULL, kmapa=NULL, khoriz=NULL, kvert=NULL, kradar=NULL, kcat=NULL)

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
       codigo.reporte <<- list()
       codigo.carga <- code.carga(nombre.filas = input$rowname, ruta = input$file1$datapath,
                                  separador = input$sep, sep.decimal = input$dec, encabezado = input$header)
       isolate(eval(parse(text = codigo.carga)))
       if(ncol(datos) < 1){
         showNotification(paste0("Error al cargar los Datos: Revisar separadores"), duration = 10, type = "error")
         return(NULL)
       }
     }, error = function(e) {
       showNotification(paste0("Error al cargar los Datos: ", e), duration = 10, type = "error")
       datos <<- NULL
       datos.originales <<- NULL
       return(NULL)
     })

    if(any(is.na(datos))){
      tryCatch({
        codigo.na <- paste0(code.NA(deleteNA = input$deleteNA), "\n", "datos <<- datos.originales")
        isolate(eval(parse(text = codigo.na)))
      }, error = function(e) {
        showNotification(paste0("Error al eliminar NAs: ", e), duration = 10, type = "error")
        datos <<- NULL
        datos.originales <<- NULL
        return(NULL)
      })
    } else {
      codigo.na <- ""
    }

    updateAceEditor(session, "fieldCodeData", value = paste0(codigo.carga, "\n", codigo.na))

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
      output$txtcor <- renderPrint(print(correlacion))
      updateSelectizeInput(session, "sel.normal", choices = colnames(var.numericas(datos)))
      updateSelectizeInput(session, "select.var", choices = colnames(var.numericas(datos)))
      updateSelectInput(session, "sel.distribucion.num", choices = colnames(var.numericas(datos)))
      updateSelectInput(session, "sel.distribucion.cat", choices = colnames(var.categoricas(datos)))
      updateSelectInput(session, "sel.resumen", choices = colnames(datos))
      updateSelectInput(session, "sel.verticales", choices = c("Todos", colnames(var.numericas(datos))))
      updateSelectInput(session, "sel.kmeans.verticales", choices = c("Todos", colnames(var.numericas(datos))))
      updateSelectInput(session, "sel.kcat.var", choices = colnames(var.categoricas(datos)))
      updateSelectInput(session, "sel.cat.var", choices = colnames(var.categoricas(datos)))
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
        if(input[[paste0("sel", var, contador)]] == "categorico" & class(datos.originales[, var]) %in% c("numeric","integer")) {
          code.res <- paste0(code.res, code.trans(var, "categorico"), "\n")
        }
        if(input[[paste0("sel", var, contador)]] == "numerico" & !(class(datos.originales[, var]) %in% c("numeric","integer"))) {
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
    if(length(var.noactivas) > 0)
      isolate(eval(parse(text = code.desactivar(var.noactivas))))

    updateAceEditor(session, "fieldCodeTrans", value = paste0(code.res, "\n", code.desactivar(var.noactivas)))
    
    tryCatch({
      isolate(eval(parse(text = modelo.cor())))
      output$txtcor <- renderPrint(print(correlacion))
      updateSelectizeInput(session, "sel.normal", choices = colnames(var.numericas(datos)))
      updateSelectizeInput(session, "select.var", choices = colnames(var.numericas(datos)))
      updateSelectInput(session, "sel.distribucion.num", choices = colnames(var.numericas(datos)))
      updateSelectInput(session, "sel.distribucion.cat", choices = colnames(var.categoricas(datos)))
      updateSelectInput(session, "sel.resumen", choices = colnames(datos))
      updateSelectInput(session, "sel.verticales", choices = c("Todos", colnames(var.numericas(datos))))
      updateSelectInput(session, "sel.kmeans.verticales", choices = c("Todos", colnames(var.numericas(datos))))
      updateSelectInput(session, "sel.kcat.var", choices = colnames(var.categoricas(datos)))
      updateSelectInput(session, "sel.cat.var", choices = colnames(var.categoricas(datos)))
    }, error = function(e) {
      return(datos <- NULL)
    })

    output$contents = DT::renderDT(mostrarData(), server = F)
    close.menu(is.null(datos))
  }, priority = 4)
  
  observeEvent(c(input$loadButton, input$transButton, input$switch.scale, input$slider.npc), {
    tryCatch({
      if(!is.null(datos)){
        updateAceEditor(session, "fieldCodePCAModelo", value = def.pca.model(scale.unit = input$switch.scale, npc = input$slider.npc))
        isolate(eval(parse(text = def.pca.model(scale.unit = input$switch.scale, npc = input$slider.npc))))
        output$txtpca <- renderPrint(print(unclass(pca.modelo)))
      }
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  }, priority = 2)

  mostrarData <- function() {
    nombre.columnas <- c("ID", colnames(datos))
    tipo.columnas <- c("", sapply(colnames(datos),
                           function(i) ifelse(class(datos[,i]) %in% c("numeric", "integer"), "Numérico", "Categórico")))
    sketch = htmltools::withTags(table(
      tableHeader(nombre.columnas),
      tableFooter(tipo.columnas)
    ))
    return(DT::datatable(datos, selection = 'none', editable = TRUE, extensions = 'Buttons', container = sketch,
              options = list(dom = 'Bfrtip', buttons = list(list(extend = 'csv', filename = "datos", text = 'Descargar')))))
  }
  output$contents = DT::renderDT(NULL, server = F)

  update.trans <- eventReactive(input$loadButton, {
    contador <<- contador + 1
    if(!is.null(datos) && ncol(datos) > 0) {
      res <-  data.frame(Variables = colnames(datos), Tipo = c(1:ncol(datos)), Activa = c(1:ncol(datos)))
      res$Tipo <- sapply(colnames(datos), function(i) paste0('<select id="sel', i, contador, '"> <option value="categorico">Categórico</option>
                                                           <option value="numerico" ', ifelse(class(datos[, i]) %in% c("numeric","integer"),
                                                                                              ' selected="selected"', ''),
                                                             '>Numérico</option> <option value="disyuntivo">Disyuntivo</option> </select>'))
      res$Activa <- sapply(colnames(datos), function(i) paste0('<input type="checkbox" id="box', i, contador, '" checked/>'))
    }else{
      res <-  as.data.frame(NULL)
      showNotification("Tiene que cargar los datos", duration = 10, type = "error")
    }
    return(res)
  })

  output$transData = DT::renderDataTable(update.trans(), escape = FALSE, selection = 'none', server = FALSE,
                                   options = list(dom = 't', paging = FALSE, ordering = FALSE), rownames = F,
                                   callback = JS("table.rows().every(function(i, tab, row) {
                                                   var $this = $(this.node());
                                                   $this.attr('id', this.data()[0]);
                                                   $this.addClass('shiny-input-checkbox');});
                                                  Shiny.unbindAll(table.table().node());
                                                  Shiny.bindAll(table.table().node());"))

  #' Resumen numérico
  #' @author Diego
  #' @return plot
  #' @export
  #'
  obj.resum <- eventReactive(c(input$loadButton, input$transButton), {
    codigo.reporte[["resumen"]] <<- c(paste0("## Resumen Numérico \n", "```{r} \n",
                                             "summary(datos) \n", "```"))
    data.frame(unclass(summary(datos)), check.names = FALSE, stringsAsFactors = FALSE)
  })

  output$resumen.completo = DT::renderDataTable({
    return(obj.resum())
  }, options = list(dom = 'ft', scrollX = TRUE), rownames = F)

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
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.normal = renderPlot({
      tryCatch({
        cod.normal <<- updatePlot$normal
        res <- isolate(eval(parse(text = cod.normal)))
        updateAceEditor(session, "fieldCodeNormal", value = cod.normal)
        codigo.reporte[[paste0("normalidad.", input$sel.normal)]] <<- paste0("## Test de Normalidad \n```{r}\n", cod.normal, "\n```")
        return(res)
      }, error = function(e){
          showNotification(paste0("ERROR AL GENERAR TEST DE NORMALIDAD: ", e), duration = 10, type = "error")
      })
    })
  })

  observeEvent(input$run.normal, {
    updatePlot$normal <- input$fieldCodeNormal
  })

  observeEvent(c(input$sel.normal, input$col.normal), {
    updatePlot$normal <- default.normal(data = "datos", vars = input$sel.normal, color = input$col.normal)
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
        showNotification(paste0("ERROR AL CALCULAR TEST DE NORMALIDAD: ", e), duration = 10, type = "error")
      })
    })
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
        res <- isolate(eval(parse(text = cod.disp)))
        if(!is.null(cod.disp) && cod.disp != ""){
          codigo.reporte[[paste0("normalidad.", paste(input$select.var, collapse = "."))]] <<-
            paste0("## Dispersión \n```{r}\n", cod.disp, "\n```")
        }
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR AL GENERAR DISPERSIÓN: ", e), duration = 10, type = "error")
      })
    })
  })

  observeEvent(input$run.disp, {
    updatePlot$disp <- input$fieldCodeDisp
  })

  observeEvent(c(input$select.var, input$col.disp), {
    if(length(input$select.var) < 2) {
      updatePlot$disp <- ""
    } else {
      updatePlot$disp <<- default.disp(data = "datos", vars = input$select.var, color = input$col.disp)
    }
  })

  #' Gráfico de PCA (Individuos)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton, input$fieldCodePCAModelo), {
    output$plot.ind = renderPlot({
      tryCatch({
        cod.pca[["individuos"]] <<- updatePlot$pca.ind
        res <- isolate(eval(parse(text = cod.pca[["individuos"]])))
        updateAceEditor(session, "fieldCodeInd", value = cod.pca[["individuos"]])
        codigo.reporte[["pca.ind"]] <<-
          paste0("## ACP de los individuos \n```{r}\n", cod.pca[["individuos"]], "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR EN PCA (Individuos): ", e), duration = 10, type = "error")
      })
    })
  }, priority = 3)

  observeEvent(input$run.pca.ind, {
    updatePlot$pca.ind <- isolate(input$fieldCodeInd)
  })

  observeEvent(c(input$col.pca.ind, input$ind.cos), {
    updatePlot$pca.ind <- isolate(pca.individuos(ind.cos = input$ind.cos * 0.01, color = input$col.pca.ind))
  })

  #' Gráfico de PCA (Variables)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton, input$fieldCodePCAModelo), {
    output$plot.var = renderPlot({
      tryCatch({
        cod.pca[["variables"]] <<- updatePlot$pca.var
        res <- isolate(eval(parse(text = cod.pca[["variables"]])))
        updateAceEditor(session, "fieldCodeVar", value = cod.pca[["variables"]])
        codigo.reporte[["pca.var"]] <<-
          paste0("## ACP de las variables \n```{r}\n", cod.pca[["variables"]], "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR EN PCA (Variables): ", e), duration = 10, type = "error")
      })
    })
  }, priority = 3)

  observeEvent(input$run.pca.var, {
    updatePlot$pca.var <- input$fieldCodeVar
  })

  observeEvent(c(input$var.cos, input$col.pca.var), {
    updatePlot$pca.var <- pca.variables(var.cos = input$var.cos * 0.01, color = input$col.pca.var)
  })

  #' Gráfico de PCA (Sobreposición)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton, input$fieldCodePCAModelo),{
    output$plot.biplot = renderPlot({
      tryCatch({
        cod.pca[["sobreposicion"]] <<- updatePlot$pca.bi
        res <- isolate(eval(parse(text = cod.pca[["sobreposicion"]])))
        updateAceEditor(session, "fieldCodeBi", value = cod.pca[["sobreposicion"]])
        codigo.reporte[["pca.bi"]] <<-
          paste0("## ACP Sobreposición \n```{r}\n", cod.pca[["sobreposicion"]], "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR EN PCA (Sobreposición): ", e), duration = 10, type = "error")
      })
    })
  }, priority = 3)

  observeEvent(input$run.pca.bi, {
    updatePlot$pca.bi <- input$fieldCodeBi
  })

  observeEvent(c(input$col.pca.ind, input$ind.cos, input$var.cos, input$col.pca.var), {
    updatePlot$pca.bi <- pca.sobreposicion(ind.cos = input$ind.cos * 0.01, var.cos = input$var.cos * 0.01,
                                           col.ind = input$col.pca.ind, col.var = input$col.pca.var)
  })

  #' Gráfico de PCA (Varianza Explicada para cada Eje)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton, input$fieldCodePCAModelo), {
    output$plotVEE = renderPlot({
      tryCatch({
        codigo <- updatePlot$pca.vee
        updateAceEditor(session, "fieldCodeAyuda", value = codigo)
        res <- isolate(eval(parse(text = codigo)))
        codigo.reporte[["vee"]] <<-
          paste0("## Varianza Explicada para cada Eje \n```{r}\n",
                 codigo,
                 "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  observeEvent(c(input$loadButton, input$transButton, input$switch.scale, input$slider.npc), {
    updatePlot$pca.vee <- code.pca.vee()
  })

  #' Gráfico de PCA (Cosenos Cuadrados de los individuos)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton, input$fieldCodePCAModelo), {
    output$plotCCI = renderPlot({
      tryCatch({
        codigo <- updatePlot$pca.cci
        updateAceEditor(session, "fieldCodeAyuda", value = codigo)
        res <- isolate(eval(parse(text = codigo)))
        codigo.reporte[["cci"]] <<-
          paste0("## Cosenos Cuadrados de los individuos \n```{r}\n",
                 codigo,
                 "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  observeEvent(c(input$loadButton, input$transButton, input$switch.scale, input$slider.npc), {
    updatePlot$pca.cci <- code.pca.cci()
  })

  #' Gráfico de PCA (Cosenos Cuadrados de las Variables)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton, input$fieldCodePCAModelo), {
    output$plotCCV = renderPlot({
      tryCatch({
        codigo <- updatePlot$pca.ccv
        updateAceEditor(session, "fieldCodeAyuda", value = codigo)
        res <- isolate(eval(parse(text = codigo)))
        codigo.reporte[["ccv"]] <<-
          paste0("## Cosenos Cuadrados de las Variables \n```{r}\n",
                 codigo,
                 "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  observeEvent(c(input$loadButton, input$transButton, input$switch.scale, input$slider.npc), {
    updatePlot$pca.ccv <- code.pca.ccv()
  })

  #' Gráfico de PCA (Correlación Variables con los Componenetes)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton, input$fieldCodePCAModelo), {
    output$plotCVC = renderPlot({
      tryCatch({
        codigo <- updatePlot$pca.cvc
        updateAceEditor(session, "fieldCodeAyuda", value = codigo)
        res <- isolate(eval(parse(text = codigo)))
        codigo.reporte[["cvc"]] <<-
          paste0("## Correlación Variables con los Componenetes \n```{r}\n",
                 codigo,
                 "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  observeEvent(c(input$loadButton, input$transButton, input$switch.scale, input$slider.npc), {
    updatePlot$pca.cvc <- code.pca.cvp()
  })

  #' Gráfico de PCA (Contribución de las variables de la Dimensión 1)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton, input$fieldCodePCAModelo), {
    output$plotCP1 = renderPlot({
      tryCatch({
        codigo <- updatePlot$pca.pc1
        updateAceEditor(session, "fieldCodeAyuda", value = codigo)
        res <- isolate(eval(parse(text = codigo)))
        codigo.reporte[["cp1"]] <<-
          paste0("## Contribución de las variables de la Dimensión 1 \n```{r}\n",
                 codigo,
                 "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  observeEvent(c(input$loadButton, input$transButton, input$switch.scale, input$slider.npc), {
    updatePlot$pca.pc1 <- code.pca.pc1()
  })

  #' Gráfico de PCA (Contribución de las variables de la Dimensión 2)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton, input$fieldCodePCAModelo), {
    output$plotCP2 = renderPlot({
      tryCatch({
        codigo <- updatePlot$pca.pc2
        updateAceEditor(session, "fieldCodeAyuda", value = codigo)
        res <- isolate(eval(parse(text = codigo)))
        codigo.reporte[["cp2"]] <<-
          paste0("## Contribución de las variables de la Dimensión 2 \n```{r}\n",
                 codigo,
                 "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  observeEvent(c(input$loadButton, input$transButton, input$switch.scale, input$slider.npc), {
    updatePlot$pca.pc2 <- code.pca.pc2()
  })

  #' Gráfico de Correlaciones
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton, input$fieldModelCor), {
    output$plot.cor = renderPlot({
      tryCatch({
        cod.cor <- updatePlot$cor
        res <- isolate(eval(parse(text = cod.cor)))
        updateAceEditor(session, "fieldCodeCor", value = cod.cor)
        codigo.reporte[["correlacion"]] <<-
          paste0("## Correlación \n```{r}\n", cod.cor, "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR EN Correlacion: ", e),
                         duration = 10,
                         type = "error")
      })
    })
  })

  observeEvent(input$run.code.cor, {
    updatePlot$cor <- input$fieldCodeCor
  })

  observeEvent(c(input$cor.metodo, input$cor.tipo), {
    updatePlot$cor <- correlaciones(metodo = input$cor.metodo, tipo = input$cor.tipo)
  })

  #' Gráfico de Distribuciones (Númericas)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.num = renderPlot({
      tryCatch({
        cod.dya.num  <<- updatePlot$dya.num
        res <- isolate(eval(parse(text = cod.dya.num)))
        updateAceEditor(session, "fieldCodeNum", value = cod.dya.num)
        codigo.reporte[[paste0("dya.num.", input$sel.distribucion.num)]] <<-
          paste0("## Distribución y atipicidad \n```{r}\n",
                 cod.dya.num,
                 "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  observeEvent(input$run.dya.num, {
    updatePlot$dya.num <- input$fieldCodeNum
  })

  observeEvent(c(input$sel.distribucion.num, input$col.dist), {
    updatePlot$dya.num <<- def.code.num(data = "datos", color = paste0("'", input$col.dist, "'"),
                                        variable = paste0("'", input$sel.distribucion.num, "'"))
  })

  output$mostrar.atipicos = DT::renderDataTable({
    atipicos <- boxplot.stats(datos[, input$sel.distribucion.num])
    datos <- datos[datos[, input$sel.distribucion.num] %in% atipicos$out, input$sel.distribucion.num, drop = F]
    return(datos[order(datos[, input$sel.distribucion.num]), , drop = F])
  }, options = list(dom = 't', scrollX = TRUE, scrollY = "10vh"))

  #' Gráfico de Distribuciones (Categóricas)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.cat = renderPlot({
      tryCatch({
        cod.dya.cat  <<- updatePlot$dya.cat
        res <- isolate(eval(parse(text = cod.dya.cat)))
        updateAceEditor(session, "fieldCodeCat", value = cod.dya.cat)
        codigo.reporte[[paste0("dya.cat.", input$sel.distribucion.cat)]] <<-
          paste0("## Distribución \n```{r}\n", cod.dya.cat, "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  observeEvent(input$run.dya.cat, {
    updatePlot$dya.cat <- input$fieldCodeCat
  })

  observeEvent(input$sel.distribucion.cat, {
    updatePlot$dya.cat <<- def.code.cat(data = "datos", variable = paste0("'", input$sel.distribucion.cat, "'"))
  })
  
  #' Actualización del Modelo Clusterización Jerarquica
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton, input$cant.cluster, input$sel.dist.method, input$sel.hc.method), {
    codigo <- def.model(data = "datos", cant = input$cant.cluster,
                        dist.method = input$sel.dist.method, hc.method = input$sel.hc.method)
    tryCatch ({
      if(!is.null(datos)){
        isolate(eval(parse(text = codigo)))
        output$txthc <- renderPrint(print(unclass(hc.modelo)))
        output$txtcentros <- renderPrint(print(unclass(centros)))
      }
    }, error = function(e) {
      print(paste0("ERROR EN HC: ", e))
      return(NULL)
    })
    
    nuevos.colores <- sapply(1:input$cant.cluster, function(i) paste0("'", input[[paste0("hcColor", i)]], "'"))
    color <- ifelse(input$sel.cluster %in% c("", "Todos"), "red", nuevos.colores[as.numeric(input$sel.cluster)])
    updatePlot$diag <<- diagrama(cant = input$cant.cluster, colores = nuevos.colores)
    updatePlot$mapa <<- cluster.mapa(cant = input$cant.cluster, colores = nuevos.colores)
    updatePlot$horiz <<- cluster.horiz(sel = paste0("'", input$sel.cluster, "'"), colores = nuevos.colores, color = color)
    updatePlot$vert <<- cluster.vert(sel = paste0("'", input$sel.verticales, "'"), colores = nuevos.colores)
    updatePlot$radar <<- def.radar(colores = nuevos.colores)
    updatePlot$cat <<- cluster.cat(var = input$sel.cat.var, cant = as.numeric(input$cant.cluster))
    updateAceEditor(session, "fieldCodeModelo", value = codigo)
    updateAceEditor(session, "fieldCodeDiag", value = updatePlot$diag)
    updateAceEditor(session, "fieldCodeMapa", value = updatePlot$mapa)
    updateAceEditor(session, "fieldCodeHoriz", value = updatePlot$horiz)
    updateAceEditor(session, "fieldCodeVert", value = updatePlot$vert)
    updateAceEditor(session, "fieldCodeRadar", value = updatePlot$radar)
    updateAceEditor(session, "fieldCodeBarras", value = updatePlot$cat)
  })

  #' Gráfico de Clusterización Jerarquica (Diagrama)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.diag = renderPlot({
      tryCatch({
        code.diagrama <<- updatePlot$diag
        res <- isolate(eval(parse(text = code.diagrama)))
        codigo.reporte[["diagrama"]] <<-
          paste0("## Dendograma \n```{r}\n", code.diagrama, "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  observeEvent(input$run.hc.diag, {
    updatePlot$diag <- input$fieldCodeDiag
  })

  #' Gráfico de Clusterización Jerarquica (Mapa)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.mapa = renderPlot({
      tryCatch({
        code.mapa <<- updatePlot$mapa
        res <- isolate(eval(parse(text = code.mapa)))
        codigo.reporte[["mapa"]] <<-
          paste0("## Mapa \n```{r}\n", code.mapa, "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  observeEvent(input$run.hc.mapa, {
    updatePlot$mapa <- input$fieldCodeMapa
  })

  #' Gráfico de Clusterización Jerarquica (Horizontal)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.horiz = renderPlot({
      tryCatch({
        code.horiz <<- updatePlot$horiz
        res <- isolate(eval(parse(text = code.horiz)))
        codigo.reporte[["horiz"]] <<-
          paste0("## Interpretación Horizontal \n```{r}\n", code.horiz, "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  observeEvent(input$run.hc.horiz, {
    updatePlot$horiz <- input$fieldCodeHoriz
  })

  observeEvent(input$sel.cluster, {
    nuevos.colores <- sapply(1:input$cant.cluster, function(i) paste0("'", input[[paste0("hcColor", i)]], "'"))
    color <- ifelse(input$sel.cluster %in% c("", "Todos"), "red", nuevos.colores[as.numeric(input$sel.cluster)])
    updatePlot$horiz <<- cluster.horiz(sel = paste0("'", input$sel.cluster, "'"), colores = nuevos.colores, color = color)
    updateAceEditor(session, "fieldCodeHoriz", value = updatePlot$horiz)
  })

  #' Gráfico de Clusterización Jerarquica (Vertical)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.vert = renderPlot({
      tryCatch({
        code.vert <<- updatePlot$vert
        res <- isolate(eval(parse(text = code.vert)))
        codigo.reporte[["vert"]] <<-
          paste0("## Interpretación Vertical \n```{r}\n", code.vert, "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  observeEvent(input$run.hc.vert, {
    updatePlot$vert <- input$fieldCodeVert
  })

  observeEvent(input$sel.verticales, {
    nuevos.colores <- sapply(1:input$cant.cluster, function(i) paste0("'", input[[paste0("hcColor", i)]], "'"))
    updatePlot$vert <<- cluster.vert(sel = paste0("'", input$sel.verticales, "'"), colores = nuevos.colores)
    updateAceEditor(session, "fieldCodeVert", value = updatePlot$vert)
  })

  #' Gráfico de Clusterización Jerarquica (Radar)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.radar = renderPlot({
      tryCatch({
        code.radar <<- updatePlot$radar
        res <- isolate(eval(parse(text = code.radar)))
        codigo.reporte[["radar"]] <<-
          paste0("## Gráfico Radar \n```{r}\n", code.radar, "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  observeEvent(input$run.hc.radar, {
    updatePlot$radar <- input$fieldCodeRadar
  })

  #' Gráfico de Clusterización Jerarquica (Categóricas)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.bar.cat = renderPlot({
      tryCatch({
        code.cat <<- updatePlot$cat
        res <- isolate(eval(parse(text = code.cat)))
        codigo.reporte[["bar.cat"]] <<-
          paste0("## Interpretación Variables Categóricas \n```{r}\n", code.cat, "\n```")
        return(res)
      }, warning = function(w) {
        showNotification(paste0("ADVERTENCIA: ", w), duration = 10, type = "warning")
        return(NULL)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  observeEvent(input$run.hc.barras, {
    updatePlot$cat <- input$fieldCodeBarras
  })

  observeEvent(input$sel.cat.var, {
    updatePlot$cat <<- cluster.cat(var = input$sel.cat.var)
    updateAceEditor(session, "fieldCodeBarras", value = updatePlot$cat)
  })
  
  
  #' Actializacion del Modelo K-medias
  #' @author Diego
  #' @return plot
  #' @export
  #'
  #'
  observeEvent(c(input$loadButton, input$transButton, input$cant.kmeans.cluster, input$num.iter, input$slider.nstart), {
    tryCatch ({
      codigo <- def.k.model(data = "datos", cant = input$cant.kmeans.cluster,
                            iter.max = input$num.iter, nstart = input$slider.nstart)
      isolate(eval(parse(text = codigo)))
      updateAceEditor(session, "fieldCodeKModelo", value = codigo)
      output$txtk <- renderPrint(print(unclass(k.modelo)))
    }, error = function(e) {
      return(NULL)
    })
    
    nuevos.colores <- sapply(1:input$cant.kmeans.cluster, function(i) paste0("'", input[[paste0("kColor", i)]], "'"))
    color <- ifelse(input$sel.kmeans.cluster %in% c("", "Todos"), "red", nuevos.colores[as.numeric(input$sel.kmeans.cluster)])
    updatePlot$jambu <<- def.code.jambu()
    updatePlot$kmapa <<- cluster.kmapa(colores = nuevos.colores)
    updatePlot$khoriz <<- cluster.khoriz(sel = paste0("'", input$sel.kmeans.cluster, "'"), colores = nuevos.colores, color = color)
    updatePlot$kvert <<- cluster.kvert(sel = paste0("'", input$sel.kmeans.verticales, "'"), colores = nuevos.colores)
    updatePlot$kradar <<- def.kradar(colores = nuevos.colores)
    updatePlot$kcat <<- cluster.kcat(var = input$sel.kcat.var)
    updateAceEditor(session, "fieldCodeJambu", value = updatePlot$jambu)
    updateAceEditor(session, "fieldCodeKmapa", value = updatePlot$kmapa)
    updateAceEditor(session, "fieldCodeKhoriz", value = updatePlot$khoriz)
    updateAceEditor(session, "fieldCodeKvert", value = updatePlot$kvert)
    updateAceEditor(session, "fieldCodeKradar", value = updatePlot$kradar)
    updateAceEditor(session, "fieldCodeKbarras", value = updatePlot$kcat)
  })

  #' Inercia K-medias
  #' @author Diego
  #' @return plot
  #' @export
  #'
  #'
  output$resumen.kmedias = renderUI({
    return(obj.inercia())
  })

  obj.inercia <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeKModelo), {
    return(HTML(resumen.kmeans(k.modelo)))
  })

  #' Gráfico de K-medias (Codo de Jambu)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.jambu = renderPlot({
      tryCatch({
        code.jambu <<- updatePlot$jambu
        isolate(eval(parse(text = code.jambu)))
        res <- isolate(eval(parse(text = code.jambu)))
        codigo.reporte[["jambu"]] <<-
          paste0("## Codo de Jambu \n```{r}\n", code.jambu, "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  observeEvent(input$run.k.jambu, {
    updatePlot$jambu <- input$fieldCodeJambu
  })

  #' Gráfico de K-medias (Mapa)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.kmapa = renderPlot({
      tryCatch({
        code.kmapa <<- updatePlot$kmapa
        res <- isolate(eval(parse(text = code.kmapa)))
        codigo.reporte[["kmapa"]] <<-
          paste0("## Mapa (K-medias) \n```{r}\n", code.kmapa, "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  observeEvent(input$run.k.mapa, {
    updatePlot$kmapa <- input$fieldCodeKmapa
  })

  #' Gráfico de K-medias (Horizontal)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.khoriz = renderPlot({
      tryCatch({
        code.khoriz <<- updatePlot$khoriz
        res <- isolate(eval(parse(text = code.khoriz)))
        codigo.reporte[["khoriz"]] <<-
          paste0("## Interpretación Horizontal (K-medias) \n```{r}\n", code.khoriz, "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  observeEvent(input$run.k.horiz, {
    updatePlot$khoriz <- input$fieldCodeKhoriz
  })

  observeEvent(input$sel.kmeans.cluster, {
    nuevos.colores <- sapply(1:input$cant.kmeans.cluster, function(i) paste0("'", input[[paste0("kColor", i)]], "'"))
    color <- ifelse(input$sel.kmeans.cluster %in% c("", "Todos"), "red", nuevos.colores[as.numeric(input$sel.kmeans.cluster)])
    updatePlot$khoriz <<- cluster.khoriz(sel = paste0("'", input$sel.kmeans.cluster, "'"), colores = nuevos.colores, color = color)
    updateAceEditor(session, "fieldCodeKhoriz", value = updatePlot$khoriz)
  })

  #' Gráfico de K-medias (Vertical)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.kvert = renderPlot({
      tryCatch({
        code.kvert <<- updatePlot$kvert
        res <- isolate(eval(parse(text = code.kvert)))
        codigo.reporte[["kvert"]] <<-
          paste0("## Interpretación Vertical (K-medias) \n```{r}\n", code.kvert, "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  observeEvent(input$run.k.vert, {
    updatePlot$kvert <- input$fieldCodeKvert
  })

  observeEvent(input$sel.kmeans.verticales, {
    nuevos.colores <- sapply(1:input$cant.kmeans.cluster, function(i) paste0("'", input[[paste0("kColor", i)]], "'"))
    updatePlot$kvert <<- cluster.kvert(sel = paste0("'", input$sel.kmeans.verticales, "'"), colores = nuevos.colores)
    updateAceEditor(session, "fieldCodeKvert", value = updatePlot$kvert)
  })

  #' Gráfico de K-medias (Radar)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.kradar = renderPlot({
      tryCatch({
        code.kradar <<- updatePlot$kradar
        res <- isolate(eval(parse(text = code.kradar)))
        codigo.reporte[["kradar"]] <<-
          paste0("## Gráfico Radar (K-medias) \n```{r}\n", code.kradar, "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      })
    })
  })

  observeEvent(input$run.k.radar, {
    updatePlot$kradar <- input$fieldCodeKradar
  })

  #' Gráfico de K-medias (Categórico)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$transButton), {
    output$plot.kcat = renderPlot({
      tryCatch({
        code.kcat <<- updatePlot$kcat
        res <- isolate(eval(parse(text = code.kcat)))
        codigo.reporte[["kcat"]] <<-
          paste0("## Interpretación Categórico (K-medias) \n```{r}\n", code.kcat, "\n```")
        return(res)
      }, error = function(e) {
        showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
        return(NULL)
      })
    })
  })

  observeEvent(input$run.k.barras, {
    updatePlot$kcat <- input$fieldCodeKbarras
  })

  observeEvent(input$sel.kcat.var, {
    updatePlot$kcat <<- cluster.kcat(var = input$sel.kcat.var)
    updateAceEditor(session, "fieldCodeKbarras", value = updatePlot$kcat)
  })

  #' Mostrar Colores (k-means)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$cant.kmeans.cluster), {
    updateSelectInput(session, "sel.kmeans.cluster", choices = c("Todos", 1:input$cant.kmeans.cluster))
    for (i in 1:10) {
      if(i <= input$cant.kmeans.cluster) {
        shinyjs::show(paste0("kColor", i))
      } else {
        shinyjs::hide(paste0("kColor", i))
      }
    }
  })

  #' Mostrar Colores (Cluster jerarquico)
  #' @author Diego
  #' @return plot
  #' @export
  #'
  observeEvent(c(input$loadButton, input$cant.cluster), {
    updateSelectInput(session, "sel.cluster", choices = c("Todos", 1:input$cant.cluster))
    for (i in 1:10) {
      if(i <= input$cant.cluster) {
        shinyjs::show(paste0("hcColor", i))
      } else {
        shinyjs::hide(paste0("hcColor", i))
      }
    }
  })

  observeEvent(c(input$hcColor1, input$hcColor2, input$hcColor3, input$hcColor4, input$hcColor5,
                 input$hcColor6, input$hcColor7, input$hcColor8, input$hcColor9, input$hcColor10), {
    nuevos.colores <- sapply(1:input$cant.cluster, function(i) paste0("'", input[[paste0("hcColor", i)]], "'"))
    color <- ifelse(input$sel.cluster %in% c("", "Todos"), "red", nuevos.colores[as.numeric(input$sel.cluster)])
    updatePlot$diag <<- diagrama(cant = input$cant.cluster, colores = nuevos.colores)
    updatePlot$mapa <<- cluster.mapa(cant = input$cant.cluster, colores = nuevos.colores)
    updatePlot$horiz <<- cluster.horiz(sel = paste0("'", input$sel.cluster, "'"), colores = nuevos.colores, color = color)
    updatePlot$vert <<- cluster.vert(sel = paste0("'", input$sel.verticales, "'"), colores = nuevos.colores)
    updatePlot$radar <<- def.radar(colores = nuevos.colores)
    updatePlot$cat <<- cluster.cat(var = input$sel.cat.var, cant = as.numeric(input$cant.cluster))
    updateAceEditor(session, "fieldCodeDiag", value = updatePlot$diag)
    updateAceEditor(session, "fieldCodeMapa", value = updatePlot$mapa)
    updateAceEditor(session, "fieldCodeHoriz", value = updatePlot$horiz)
    updateAceEditor(session, "fieldCodeVert", value = updatePlot$vert)
    updateAceEditor(session, "fieldCodeRadar", value = updatePlot$radar)
    updateAceEditor(session, "fieldCodeBarras", value = updatePlot$cat)
  })

  observeEvent(c(input$kColor1, input$kColor2, input$kColor3, input$kColor4, input$kColor5,
                 input$kColor6, input$kColor7, input$kColor8, input$kColor9, input$kColor10), {
    nuevos.colores <- sapply(1:input$cant.kmeans.cluster, function(i) paste0("'", input[[paste0("kColor", i)]], "'"))
    color <- ifelse(input$sel.kmeans.cluster %in% c("", "Todos"), "red", nuevos.colores[as.numeric(input$sel.kmeans.cluster)])
    updatePlot$kmapa <<- cluster.kmapa(colores = nuevos.colores)
    updatePlot$khoriz <<- cluster.khoriz(sel = paste0("'", input$sel.kmeans.cluster, "'"), colores = nuevos.colores, color = color)
    updatePlot$kvert <<- cluster.kvert(sel = paste0("'", input$sel.kmeans.verticales, "'"), colores = nuevos.colores)
    updatePlot$kradar <<- def.kradar(colores = nuevos.colores)
    updatePlot$kcat <<- cluster.kcat(var = input$sel.kcat.var)
    updateAceEditor(session, "fieldCodeKmapa", value = updatePlot$kmapa)
    updateAceEditor(session, "fieldCodeKhoriz", value = updatePlot$khoriz)
    updateAceEditor(session, "fieldCodeKvert", value = updatePlot$kvert)
    updateAceEditor(session, "fieldCodeKradar", value = updatePlot$kradar)
    updateAceEditor(session, "fieldCodeKbarras", value = updatePlot$kcat)
  })

  output$knitDoc <- renderUI({
    return(obj.reporte())
  })

  observeEvent(input$btnReporte, {
    updateAceEditor(session, "fieldCodeReport", value = def.reporte(titulo = input$textTitulo, nombre = input$textNombre, input))
  })

  obj.reporte <- eventReactive(input$fieldCodeReport, {
    updateAceEditor(session, "fieldCodeReport", value = input$fieldCodeReport)
    return(isolate(HTML(knit2html(text = input$fieldCodeReport, fragment.only = T, quiet = T))))
  })

  output$descargar <- downloadHandler(
    filename = function() {
      paste('data-', Sys.Date(), '.zip', sep='')
    },
    content = function(file) {
      owd <- setwd(tempdir())
      on.exit(setwd(owd))
      files <- NULL;

      namermd <- paste('data-', Sys.Date(), '.rmd', sep='')
      writeLines(input$fieldCodeReport, namermd)
      files <- c(namermd, files)

      src <- normalizePath(namermd)
      out <- rmarkdown::render(src,  params = NULL, rmarkdown::word_document())
      file.rename(out, paste('data-', Sys.Date(), '.docx', sep=''))
      files <- c(paste('data-', Sys.Date(), '.docx', sep=''), files)

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

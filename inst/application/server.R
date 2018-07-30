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
  Sys.setenv("LANGUAGE"="ES")
  options(encoding = "utf8")
  options(DT.options = list(aLengthMenu = c(10, 30, 50), iDisplayLength = 10, scrollX = TRUE))

  session$onSessionEnded(function() {
    borrar <- ls(envir = .GlobalEnv)
    borrar <- c(borrar[!(borrar %in% .GlobalEnv$foto)], "foto")
    rm(envir = .GlobalEnv, list = borrar)
    stopApp()
  })

  observe({
    addClass(class = "disabled", selector = "#sidebarItemExpanded li[class^=treeview]")
    addClass(class = "disabled", selector = "#sidebarItemExpanded li a[data-value=acp]")
    addClass(class = "disabled", selector = "#sidebarItemExpanded li a[data-value=agrupacion]")
    addClass(class = "disabled", selector = "#sidebarItemExpanded li a[data-value=kmedias]")
    addClass(class = "disabled", selector = "#sidebarItemExpanded li a[data-value=reporte]")
    close.menu()
  })

  observeEvent(input$loadButton, {
    codigo.reporte <<- list()
    codigo.carga <- code.carga(nombre.filas = input$rowname, ruta = input$file1$datapath,
                               separador = input$sep, sep.decimal = input$dec, encabezado = input$header)

     tryCatch({
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
      codigo.na <- paste0(code.NA(deleteNA = input$deleteNA), "\n", "datos <<- datos.originales")
      tryCatch({
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
      isolate(eval(parse(text = default.centros())))
      isolate(eval(parse(text = modelo.cor())))
      isolate(eval(parse(text = def.pca.model(scale.unit = input$switch.scale, npc = input$slider.npc))))
      output$txtpca <- renderPrint(print(unclass(pca.modelo)))
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

      updateAceEditor(session, "fieldModelCor", value = modelo.cor())
      updateAceEditor(session, "fieldCodePCAModelo", value = def.pca.model(scale.unit = input$switch.scale, npc = input$slider.npc))
      updateAceEditor(session, "fieldFuncJambu", value = def.func.jambu())
      updateAceEditor(session, "fieldCodeJambu", value = def.code.jambu())

      updateAceEditor(session, "fieldCodeResum", value = cod.resum())
      updateAceEditor(session, "fieldFuncNum", value = func.dya.num)
      updateAceEditor(session, "fieldFuncCat", value = func.dya.cat)

      updateAceEditor(session, "fieldCodeCentr", value = func.centros)
      updateAceEditor(session, "fieldFuncHoriz", value = func.horiz)
      updateAceEditor(session, "fieldFuncVert", value = func.vert)
      updateAceEditor(session, "fieldFuncRadar", value = func.radar)

      updateAceEditor(session, "fieldFuncJambu", value = def.func.jambu())
      updateAceEditor(session, "fieldFuncKhoriz", value = func.khoriz)
      updateAceEditor(session, "fieldFuncKvert", value = func.kvert)
      updateAceEditor(session, "fieldFuncKradar", value = func.kradar)
    }, error = function(e) {
      print(paste0("ERROR EN EVALUAR: ", e))
      return(datos <- NULL)
    })

    output$contents = DT::renderDT(mostrarData(), server = F)
    close.menu(is.null(datos))
  })

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

    updateSelectizeInput(session, "sel.normal", choices = colnames(var.numericas(datos)))
    updateSelectizeInput(session, "select.var", choices = colnames(var.numericas(datos)))
    updateSelectInput(session, "sel.distribucion.num", choices = colnames(var.numericas(datos)))
    updateSelectInput(session, "sel.distribucion.cat", choices = colnames(var.categoricas(datos)))
    updateSelectInput(session, "sel.resumen", choices = colnames(datos))
    updateSelectInput(session, "sel.verticales", choices = c("Todos", colnames(var.numericas(datos))))
    updateSelectInput(session, "sel.kmeans.verticales", choices = c("Todos", colnames(var.numericas(datos))))
    updateSelectInput(session, "sel.kcat.var", choices = colnames(var.categoricas(datos)))
    updateSelectInput(session, "sel.cat.var", choices = colnames(var.categoricas(datos)))

    updateAceEditor(session, "fieldModelCor", value = modelo.cor())
    updateAceEditor(session, "fieldCodePCAModelo", value = def.pca.model(scale.unit = input$switch.scale, npc = input$slider.npc))
    updateAceEditor(session, "fieldFuncJambu", value = def.func.jambu())
    updateAceEditor(session, "fieldCodeJambu", value = def.code.jambu())

    updateAceEditor(session, "fieldCodeResum", value = cod.resum())
    updateAceEditor(session, "fieldFuncNum", value = func.dya.num)
    updateAceEditor(session, "fieldFuncCat", value = func.dya.cat)

    updateAceEditor(session, "fieldCodeCentr", value = func.centros)
    updateAceEditor(session, "fieldFuncHoriz", value = func.horiz)
    updateAceEditor(session, "fieldFuncVert", value = func.vert)
    updateAceEditor(session, "fieldFuncRadar", value = func.radar)

    updateAceEditor(session, "fieldFuncJambu", value = def.func.jambu())
    updateAceEditor(session, "fieldFuncKhoriz", value = func.khoriz)
    updateAceEditor(session, "fieldFuncKvert", value = func.kvert)
    updateAceEditor(session, "fieldFuncKradar", value = func.kradar)
    tryCatch({
      isolate(eval(parse(text = default.centros())))
      isolate(eval(parse(text = modelo.cor())))
      isolate(eval(parse(text = def.pca.model(scale.unit = input$switch.scale, npc = input$slider.npc))))
      output$txtpca <- renderPrint(print(unclass(pca.modelo)))
      output$txtcor <- renderPrint(print(correlacion))
    }, error = function(e) {
      return(datos <- NULL)
    })

    output$contents = DT::renderDT(mostrarData(), server = F)
    close.menu(is.null(datos))
  })

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

  observeEvent(c(input$switch.scale, input$slider.npc), {
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

  output$hcColores = renderUI({
    return(obj.hc.colores())
  })

  output$kColores = renderUI({
    return(obj.k.colores())
  })

  output$mostrar.atipicos = DT::renderDataTable({
    atipicos <- boxplot.stats(datos[, input$sel.distribucion.num])
    datos <- datos[datos[, input$sel.distribucion.num] %in% atipicos$out, input$sel.distribucion.num, drop = F]
    return(datos[order(datos[, input$sel.distribucion.num]), , drop = F])
  }, options = list(dom = 't', scrollX = TRUE, scrollY = "10vh"))

  output$plot.normal = renderPlot({
    return(obj.normal())
  })

  output$calculo.normal = DT::renderDataTable(obj.calc.normal())

  output$plot.disp = renderPlot({
    return(obj.disp())
  })

  output$plot.ind = renderPlot({
    return(obj.ind())
  })

  output$plot.var = renderPlot({
    return(obj.var())
  })

  output$plot.biplot = renderPlot({
    return(obj.biplot())
  })

  output$plotVEE = renderPlot({
    return(obj.vee())
  })

  output$plotCCI = renderPlot({
    return(obj.cci())
  })

  output$plotCCV = renderPlot({
    return(obj.ccv())
  })

  output$plotCVC = renderPlot({
    return(obj.cvc())
  })

  output$plotCP1 = renderPlot({
    return(obj.cp1())
  })

  output$plotCP2 = renderPlot({
    return(obj.cp2())
  })

  output$plot.cor = renderPlot({
    return(obj.cor())
  })

  output$plot.num = renderPlot({
    return(obj.dya.num())
  })

  output$plot.cat = renderPlot({
    return(obj.dya.cat())
  })

  output$plot.diag = renderPlot({
    return(obj.diagrama())
  })

  output$plot.mapa = renderPlot({
    return(obj.mapa())
  })

  output$plot.horiz = renderPlot({
    return(obj.horiz())
  })

  output$plot.vert = renderPlot({
    return(obj.vert())
  })

  output$plot.radar = renderPlot({
    return(obj.radar())
  })

  output$plot.bar.cat = renderPlot({
    return(obj.bar.cat())
  })

  output$pcakmedias = renderPlot({
    pca.kmedias(var.numericas(datos))
  })

  output$resumen.kmedias = renderUI({
    return(obj.inercia())
  })

  output$plot.jambu = renderPlot({
    return(obj.jambu())
  })

  output$plot.kmapa = renderPlot({
    return(obj.kmapa())
  })

  output$plot.khoriz = renderPlot({
    return(obj.khoriz())
  })

  output$plot.kvert = renderPlot({
    return(obj.kvert())
  })

  output$plot.kradar = renderPlot({
    return(obj.kradar())
  })

  output$plot.kcat = renderPlot({
    return(obj.kcat())
  })

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

  obj.resum <- eventReactive(c(input$loadButton, input$transButton), {
    codigo.reporte[["resumen"]] <<- c(paste0("## Resumen Numérico \n", "```{r} \n",
                                             "summary(datos) \n", "```"))
    data.frame(unclass(summary(datos)), check.names = FALSE, stringsAsFactors = FALSE)
  })

  observeEvent(c(input$loadButton, input$transButton, input$sel.normal, input$col.normal), {
    cod.normal <<- default.normal(data = "datos", vars = input$sel.normal, color = input$col.normal)
    updateAceEditor(session, "fieldCodeNormal", value = cod.normal)
  })

  obj.normal <- eventReactive(input$fieldCodeNormal, {
    tryCatch({
      res <- isolate(eval(parse(text = input$fieldCodeNormal)))
      codigo.reporte[[paste0("normalidad.", input$sel.normal)]] <<- paste0("## Test de Normalidad \n```{r}\n", input$fieldCodeNormal, "\n```")
      return(res)
    }, error = function(e){
      showNotification(paste0("ERROR AL GENERAR TEST DE NORMALIDAD: ", e), duration = 10, type = "error")
    })
  })

  #observeEvent({c(input$loadButton, input$transButton)}, {
  #  cod.normal <<- default.calc.normal()
  #  updateAceEditor(session, "fieldCalcNormal", value = cod.normal)
  #})

  obj.calc.normal <- eventReactive(c(input$loadButton, input$transButton), {
    tryCatch({
      res <- isolate(eval(parse(text = default.calc.normal())))
      updateAceEditor(session, "fieldCalcNormal", value = default.calc.normal())
      return(res)
    }, error = function(e){
      showNotification(paste0("ERROR AL CALCULAR TEST DE NORMALIDAD: ", e), duration = 10, type = "error")
    })
  })

  obj.disp <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeDisp, input$select.var, input$col.disp), {
    tryCatch({
      if(length(input$select.var) < 2) {
        updateAceEditor(session, "fieldCodeDisp", value = "")
        return(NULL)
      } else {
        cod.disp <<- default.disp(data = "datos", vars = input$select.var, color = input$col.disp)
        updateAceEditor(session, "fieldCodeDisp", value = cod.disp)
        res <- isolate(eval(parse(text = cod.disp)))
        codigo.reporte[[paste0("normalidad.", paste(input$select.var, collapse = "."))]] <<-
          paste0("## Dispersión \n```{r}\n", cod.disp, "\n```")
        return(res)
      }
    }, error = function(e){
      showNotification(paste0("ERROR AL GENERAR DISPERSIÓN: ", e), duration = 10, type = "error")
    })
  })

  observeEvent(c(input$loadButton, input$transButton, input$fieldModelCor, input$cor.metodo, input$cor.tipo), {
    cod.cor <<- correlaciones(metodo = input$cor.metodo, tipo = input$cor.tipo)
    updateAceEditor(session, "fieldCodeCor", value = cod.cor)
  })

  obj.cor <- eventReactive(input$fieldCodeCor, {
    tryCatch({
      res <- isolate(eval(parse(text = input$fieldCodeCor)))
      codigo.reporte[["correlacion"]] <<- paste0("## Correlación \n```{r}\n", input$fieldCodeCor, "\n```")
      return(res)
    }, error = function(e) {
      showNotification(paste0("ERROR EN Correlacion: ", e), duration = 10, type = "error")
    })
  })

  observeEvent(c(input$pca.num, input$col.pca.ind, input$ind.cos), {
    cod.pca[["individuos"]] <<- pca.individuos(ind.cos = input$ind.cos * 0.01, color = input$col.pca.ind)
    updateAceEditor(session, "fieldCodeInd", value = cod.pca[["individuos"]])
    cod.pca[["sobreposicion"]] <<- pca.sobreposicion(ind.cos = input$ind.cos * 0.01, var.cos = input$var.cos * 0.01,
                                                     col.ind = input$col.pca.ind, col.var = input$col.pca.var)
    updateAceEditor(session, "fieldCodeBi", value = cod.pca[["sobreposicion"]])
  })

  obj.ind <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeInd, input$fieldCodePCAModelo), {
    tryCatch({
      res <- isolate(eval(parse(text = cod.pca[["individuos"]])))
      codigo.reporte[["pca.ind"]] <<- paste0("## ACP de los individuos \n```{r}\n", cod.pca[["individuos"]], "\n```")
      return(res)
    }, error = function(e) {
      showNotification(paste0("ERROR EN PCA (Individuos): ", e), duration = 10, type = "error")
    })
  })

  observeEvent(c(input$pca.num, input$var.cos, input$col.pca.var), {
    cod.pca[["variables"]] <<- pca.variables(var.cos = input$var.cos * 0.01, color = input$col.pca.var)
    updateAceEditor(session, "fieldCodeVar", value = cod.pca[["variables"]])
    cod.pca[["sobreposicion"]] <<- pca.sobreposicion(ind.cos = input$ind.cos * 0.01, var.cos = input$var.cos * 0.01,
                                                     col.ind = input$col.pca.ind, col.var = input$col.pca.var)
    updateAceEditor(session, "fieldCodeBi", value = cod.pca[["sobreposicion"]])
  })

  obj.var <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeVar, input$fieldCodePCAModelo), {
    tryCatch({
      res <- isolate(eval(parse(text = cod.pca[["variables"]])))
      codigo.reporte[["pca.var"]] <<- paste0("## ACP de las variables \n```{r}\n", cod.pca[["variables"]], "\n```")
      return(res)
    }, error = function(e) {
      showNotification(paste0("ERROR EN PCA (Variables): ", e), duration = 10, type = "error")
    })
  })

  obj.biplot <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeBi, input$fieldCodePCAModelo), {
    tryCatch({
      res <- isolate(eval(parse(text = cod.pca[["sobreposicion"]])))
      codigo.reporte[["pca.bi"]] <<- paste0("## ACP Sobreposición \n```{r}\n", cod.pca[["sobreposicion"]], "\n```")
      return(res)
    }, error = function(e) {
      showNotification(paste0("ERROR EN PCA (Sobreposición): ", e), duration = 10, type = "error")
    })
  })

  obj.vee <- eventReactive(c(input$loadButton, input$transButton, input$switch.scale, input$slider.npc), {
    tryCatch({
      codigo <- code.pca.vee()
      updateAceEditor(session, "fieldCodeAyuda", value = codigo)
      res <- isolate(eval(parse(text = codigo)))
      codigo.reporte[["vee"]] <<- paste0("## Varianza Explicada para cada Eje \n```{r}\n", codigo, "\n```")
      return(res)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })

  obj.cci <- eventReactive(c(input$loadButton, input$transButton, input$switch.scale, input$slider.npc), {
    tryCatch({
      codigo <- code.pca.cci()
      updateAceEditor(session, "fieldCodeAyuda", value = codigo)
      res <- isolate(eval(parse(text = codigo)))
      codigo.reporte[["cci"]] <<- paste0("## Cosenos Cuadrados de los individuos \n```{r}\n", codigo, "\n```")
      return(res)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })

  obj.ccv <- eventReactive(c(input$loadButton, input$transButton, input$switch.scale, input$slider.npc), {
    tryCatch({
      codigo <- code.pca.ccv()
      updateAceEditor(session, "fieldCodeAyuda", value = codigo)
      res <- isolate(eval(parse(text = codigo)))
      codigo.reporte[["ccv"]] <<- paste0("## Cosenos Cuadrados de las Variables \n```{r}\n", codigo, "\n```")
      return(res)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })

  obj.cvc <- eventReactive(c(input$loadButton, input$transButton, input$switch.scale, input$slider.npc), {
    tryCatch({
      codigo <- code.pca.cvp()
      updateAceEditor(session, "fieldCodeAyuda", value = codigo)
      res <- isolate(eval(parse(text = codigo)))
      codigo.reporte[["cvc"]] <<- paste0("## Correlación Variables con los Componenetes \n```{r}\n", codigo, "\n```")
      return(res)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })

  obj.cp1 <- eventReactive(c(input$loadButton, input$transButton, input$switch.scale, input$slider.npc), {
    tryCatch({
      codigo <- code.pca.pc1()
      updateAceEditor(session, "fieldCodeAyuda", value = codigo)
      res <- isolate(eval(parse(text = codigo)))
      codigo.reporte[["cp1"]] <<- paste0("## Contribución de las variables de la Dimensión 1 \n```{r}\n", codigo, "\n```")
      return(res)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })

  obj.cp2 <- eventReactive(c(input$loadButton, input$transButton, input$switch.scale, input$slider.npc), {
    tryCatch({
      codigo <- code.pca.pc2()
      updateAceEditor(session, "fieldCodeAyuda", value = codigo)
      res <- isolate(eval(parse(text = codigo)))
      codigo.reporte[["cp2"]] <<- paste0("## Contribución de las variables de la Dimensión 2 \n```{r}\n", codigo, "\n```")
      return(res)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })

  obj.dya.num <- eventReactive(c(input$loadButton, input$transButton, input$fieldFuncNum, input$fieldCodeNum), {
    tryCatch({
      cod.dya.num  <<- input$fieldCodeNum
      func.dya.num <<- input$fieldFuncNum
      isolate(eval(parse(text = func.dya.num)))
      res <- isolate(eval(parse(text = cod.dya.num)))
      codigo.reporte[["dya.num"]] <<- paste0("## Distribución y atipicidad \n```{r}\n", cod.dya.num, "\n```")
      return(res)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })

  obj.dya.cat <- eventReactive(c(input$loadButton, input$transButton, input$fieldFuncCat, input$fieldCodeCat), {
    tryCatch({
      cod.dya.cat  <<- input$fieldCodeCat
      func.dya.cat <<- input$fieldFuncCat
      isolate(eval(parse(text = func.dya.cat)))
      res <- isolate(eval(parse(text = cod.dya.cat)))
      codigo.reporte[["cp1"]] <<- paste0("## Distribución \n```{r}\n", cod.dya.cat, "\n```")
      return(res)
    }, error = function(e){
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })

  obj.diagrama <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeDiag, input$fieldCodeModelo), {
    tryCatch({
      code.diagrama <<- input$fieldCodeDiag
      res <- isolate(eval(parse(text = code.diagrama)))
      codigo.reporte[["diagrama"]] <<- paste0("## Dendograma \n```{r}\n", code.diagrama, "\n```")
      return(res)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })

  obj.mapa <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeMapa, input$fieldCodeModelo, input$fieldCodePCAModelo), {
    tryCatch({
      code.mapa <<- input$fieldCodeMapa
      res <- isolate(eval(parse(text = code.mapa)))
      codigo.reporte[["mapa"]] <<- paste0("## Mapa \n```{r}\n", code.mapa, "\n```")
      return(res)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })

  obj.horiz <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeHoriz,
                               input$fieldFuncHoriz, input$fieldCodeCentr, input$fieldCodeModelo), {
    tryCatch({
      code.horiz <<- input$fieldCodeHoriz
      func.horiz <<- input$fieldFuncHoriz
      func.centros <<- input$fieldCodeCentr
      isolate(eval(parse(text = func.horiz)))
      isolate(eval(parse(text = func.centros)))
      res <- isolate(eval(parse(text = code.horiz)))
      codigo.reporte[["horiz"]] <<- paste0("## Interpretación Horizontal \n```{r}\n", code.horiz, "\n```")
      return(res)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })

  obj.vert <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeVert, input$fieldCodeCentr, input$fieldCodeModelo), {
    tryCatch({
      code.vert <<- input$fieldCodeVert
      func.vert <<- input$fieldFuncVert
      func.centros <<- input$fieldCodeCentr
      isolate(eval(parse(text = func.vert)))
      isolate(eval(parse(text = func.centros)))
      res <- isolate(eval(parse(text = code.vert)))
      codigo.reporte[["vert"]] <<- paste0("## Interpretación Vertical \n```{r}\n", code.vert, "\n```")
      return(res)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })

  obj.radar <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeRadar,
                               input$fieldFuncRadar, input$fieldCodeCentr, input$fieldCodeModelo), {
    tryCatch({
      code.radar <<- input$fieldCodeRadar
      func.radar <<- input$fieldFuncRadar
      func.centros <<- input$fieldCodeCentr
      isolate(eval(parse(text = func.centros)))
      isolate(eval(parse(text = func.radar)))
      res <- isolate(eval(parse(text = code.radar)))
      codigo.reporte[["radar"]] <<- paste0("## Gráfico Radar \n```{r}\n", code.radar, "\n```")
      return(res)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })

  obj.bar.cat <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeBarras, input$fieldCodeModelo), {
    tryCatch({
      code.cat <<- input$fieldCodeBarras
      res <- isolate(eval(parse(text = code.cat)))
      codigo.reporte[["bar.cat"]] <<- paste0("## Interpretación Variables Categóricas \n```{r}\n", code.cat, "\n```")
      return(res)
    }, warning = function(w){
      showNotification(paste0("ADVERTENCIA: ", w), duration = 10, type = "warning")
      return(NULL)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })

  obj.inercia <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeKModelo), {
    return(HTML(resumen.kmeans(k.modelo)))
  })

  obj.jambu <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeJambu, input$fieldCodeKModelo, input$fieldFuncJambu), {
    tryCatch({
      isolate(eval(parse(text = input$fieldFuncJambu)))
      res <- isolate(eval(parse(text = input$fieldCodeJambu)))
      codigo.reporte[["jambu"]] <<- paste0("## Codo de Jambu \n```{r}\n", input$fieldCodeJambu, "\n```")
      return(res)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })

  obj.kmapa <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeKmapa, input$fieldCodeKModelo, input$fieldCodePCAModelo), {
    tryCatch({
      code.kmapa <<- input$fieldCodeKmapa
      res <- isolate(eval(parse(text = code.kmapa)))
      codigo.reporte[["kmapa"]] <<- paste0("## Mapa (K-medias) \n```{r}\n", code.kmapa, "\n```")
      return(res)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })

  obj.khoriz <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeKhoriz, input$fieldFuncKhoriz, input$fieldCodeKModelo), {
    tryCatch({
      code.khoriz <<- input$fieldCodeKhoriz
      func.khoriz <<- input$fieldFuncKhoriz
      isolate(eval(parse(text = func.khoriz)))
      res <- isolate(eval(parse(text = code.khoriz)))
      codigo.reporte[["khoriz"]] <<- paste0("## Interpretación Horizontal (K-medias) \n```{r}\n", code.khoriz, "\n```")
      return(res)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })

  obj.kvert <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeKvert, input$fieldFuncKvert, input$fieldCodeKModelo), {
    tryCatch({
      code.kvert <<- input$fieldCodeKvert
      func.kvert <<- input$fieldFuncKvert
      isolate(eval(parse(text = func.kvert)))
      res <- isolate(eval(parse(text = code.kvert)))
      codigo.reporte[["kvert"]] <<- paste0("## Interpretación Vertical (K-medias) \n```{r}\n", code.kvert, "\n```")
      return(res)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })

  obj.kradar <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeKradar, input$fieldFuncKradar, input$fieldCodeKModelo), {
    tryCatch({
      code.kradar <<- input$fieldCodeKradar
      func.kradar <<- input$fieldFuncKradar
      isolate(eval(parse(text = func.kradar)))
      res <- isolate(eval(parse(text = code.kradar)))
      codigo.reporte[["kradar"]] <<- paste0("## Gráfico Radar (K-medias) \n```{r}\n", code.kradar, "\n```")
      return(res)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
    })
  })

  obj.kcat <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeKbarras, input$fieldCodeKModelo), {
    tryCatch({
      code.kcat <<- input$fieldCodeKbarras
      res <- isolate(eval(parse(text = code.kcat)))
      codigo.reporte[["kcat"]] <<- paste0("## Interpretación Categórico (K-medias) \n```{r}\n", code.kcat, "\n```")
      return(res)
    }, error = function(e) {
      showNotification(paste0("ERROR: ", e), duration = 10, type = "error")
      return(NULL)
    })
  })

  observeEvent(c(input$sel.distribucion.num, input$col.dist), {
    cod.dya.num <<- def.code.num(data = "datos", variable = paste0("'", input$sel.distribucion.num, "'"),
                                 color = paste0("'", input$col.dist, "'"))
    updateAceEditor(session, "fieldCodeNum", value = cod.dya.num)
  })

  observeEvent(c(input$sel.distribucion.cat), {
    cod.dya.cat <<- def.code.cat(data = "datos", variable = paste0("'", input$sel.distribucion.cat, "'"))
    updateAceEditor(session, "fieldCodeCat", value = cod.dya.cat)
  })

  observeEvent(c(input$loadButton, input$cant.cluster, input$transButton, input$sel.dist.method, input$sel.hc.method), {
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
    code.diagrama <<- diagrama(cant = input$cant.cluster, colores = nuevos.colores)
    code.mapa <<- cluster.mapa(cant = input$cant.cluster, colores = nuevos.colores)
    code.horiz <<- cluster.horiz(sel = paste0("'", input$sel.cluster, "'"), colores = nuevos.colores, color = color)
    code.vert <<- cluster.vert(sel = paste0("'", input$sel.verticales, "'"), colores = nuevos.colores)
    code.radar <<- def.radar(colores = nuevos.colores)
    code.cat <<- cluster.cat(var = input$sel.cat.var, cant = as.numeric(input$cant.cluster))
    updateAceEditor(session, "fieldCodeDiag", value = code.diagrama)
    updateAceEditor(session, "fieldCodeMapa", value = code.mapa)
    updateAceEditor(session, "fieldCodeHoriz", value = code.horiz)
    updateAceEditor(session, "fieldCodeVert", value = code.vert)
    updateAceEditor(session, "fieldCodeRadar", value = code.radar)
    updateAceEditor(session, "fieldCodeBarras", value = code.cat)
    updateAceEditor(session, "fieldCodeModelo", value = codigo)
  })

  observeEvent(input$sel.cluster, {
    nuevos.colores <- sapply(1:input$cant.cluster, function(i) paste0("'", input[[paste0("hcColor", i)]], "'"))
    color <- ifelse(input$sel.cluster %in% c("", "Todos"), "red", nuevos.colores[as.numeric(input$sel.cluster)])
    code.horiz <<- cluster.horiz(sel = paste0("'", input$sel.cluster, "'"), colores = nuevos.colores, color = color)
    updateAceEditor(session, "fieldCodeHoriz", value = code.horiz)
  })

  observeEvent(input$sel.verticales, {
    nuevos.colores <- sapply(1:input$cant.cluster, function(i) paste0("'", input[[paste0("hcColor", i)]], "'"))
    code.vert <<- cluster.vert(sel = paste0("'", input$sel.verticales, "'"), colores = nuevos.colores)
    updateAceEditor(session, "fieldCodeVert", value = code.vert)
  })

  observeEvent(input$sel.cat.var, {
    code.cat <<- cluster.cat(var = input$sel.cat.var)
    updateAceEditor(session, "fieldCodeBarras", value = code.cat)
  })

  observeEvent(c(input$hcColor1, input$hcColor2, input$hcColor3, input$hcColor4, input$hcColor5,
                 input$hcColor6, input$hcColor7, input$hcColor8, input$hcColor9, input$hcColor10), {
    nuevos.colores <- sapply(1:input$cant.cluster, function(i) paste0("'", input[[paste0("hcColor", i)]], "'"))
    color <- ifelse(input$sel.cluster %in% c("", "Todos"), "red", nuevos.colores[as.numeric(input$sel.cluster)])
    code.diagrama <<- diagrama(cant = input$cant.cluster, colores = nuevos.colores)
    code.mapa <<- cluster.mapa(cant = input$cant.cluster, colores = nuevos.colores)
    code.horiz <<- cluster.horiz(sel = paste0("'", input$sel.cluster, "'"), colores = nuevos.colores, color = color)
    code.vert <<- cluster.vert(sel = paste0("'", input$sel.verticales, "'"), colores = nuevos.colores)
    code.radar <<- def.radar(colores = nuevos.colores)
    code.cat <<- cluster.cat(var = input$sel.cat.var, cant = as.numeric(input$cant.cluster))
    updateAceEditor(session, "fieldCodeDiag", value = code.diagrama)
    updateAceEditor(session, "fieldCodeMapa", value = code.mapa)
    updateAceEditor(session, "fieldCodeHoriz", value = code.horiz)
    updateAceEditor(session, "fieldCodeVert", value = code.vert)
    updateAceEditor(session, "fieldCodeRadar", value = code.radar)
    updateAceEditor(session, "fieldCodeBarras", value = code.cat)
  })

  observeEvent(c(input$loadButton, input$cant.kmeans.cluster, input$transButton, input$num.iter, input$slider.nstart), {
    tryCatch ({
      codigo <- def.k.model(data = "datos", cant = input$cant.kmeans.cluster,
                            iter.max = input$num.iter, nstart = input$slider.nstart)
      isolate(eval(parse(text = codigo)))
      output$txtk <- renderPrint(print(unclass(k.modelo)))
    }, error = function(e) {
      return(NULL)
    })

    nuevos.colores <- sapply(1:input$cant.kmeans.cluster, function(i) paste0("'", input[[paste0("kColor", i)]], "'"))
    color <- ifelse(input$sel.kmeans.cluster %in% c("", "Todos"), "red", nuevos.colores[as.numeric(input$sel.kmeans.cluster)])
    code.kmapa <<- cluster.kmapa(colores = nuevos.colores)
    code.khoriz <<- cluster.khoriz(sel = paste0("'", input$sel.kmeans.cluster, "'"), colores = nuevos.colores, color = color)
    code.kvert <<- cluster.kvert(sel = paste0("'", input$sel.kmeans.verticales, "'"), colores = nuevos.colores)
    code.kradar <<- def.kradar(colores = nuevos.colores)
    code.kcat <<- cluster.kcat(var = input$sel.kcat.var)
    updateAceEditor(session, "fieldCodeKmapa", value = code.kmapa)
    updateAceEditor(session, "fieldCodeKhoriz", value = code.khoriz)
    updateAceEditor(session, "fieldCodeKvert", value = code.kvert)
    updateAceEditor(session, "fieldCodeKradar", value = code.kradar)
    updateAceEditor(session, "fieldCodeKbarras", value = code.kcat)
    updateAceEditor(session, "fieldCodeKModelo", value = codigo)
  })

  observeEvent(input$sel.kmeans.cluster, {
    nuevos.colores <- sapply(1:input$cant.kmeans.cluster, function(i) paste0("'", input[[paste0("kColor", i)]], "'"))
    color <- ifelse(input$sel.kmeans.cluster %in% c("", "Todos"), "red", nuevos.colores[as.numeric(input$sel.kmeans.cluster)])
    code.khoriz <<- cluster.khoriz(sel = paste0("'", input$sel.kmeans.cluster, "'"), colores = nuevos.colores, color = color)
    updateAceEditor(session, "fieldCodeKhoriz", value = code.khoriz)
  })

  observeEvent(input$sel.kmeans.verticales, {
    nuevos.colores <- sapply(1:input$cant.kmeans.cluster, function(i) paste0("'", input[[paste0("kColor", i)]], "'"))
    code.kvert <<- cluster.kvert(sel = paste0("'", input$sel.kmeans.verticales, "'"), colores = nuevos.colores)
    updateAceEditor(session, "fieldCodeKvert", value = code.kvert)
  })

  observeEvent(input$sel.kcat.var, {
    code.kcat <<- cluster.kcat(var = input$sel.kcat.var)
    updateAceEditor(session, "fieldCodeKbarras", value = code.kcat)
  })

  observeEvent(c(input$kColor1, input$kColor2, input$kColor3, input$kColor4, input$kColor5,
                 input$kColor6, input$kColor7, input$kColor8, input$kColor9, input$kColor10), {
    nuevos.colores <- sapply(1:input$cant.kmeans.cluster, function(i) paste0("'", input[[paste0("kColor", i)]], "'"))
    color <- ifelse(input$sel.kmeans.cluster %in% c("", "Todos"), "red", nuevos.colores[as.numeric(input$sel.kmeans.cluster)])
    code.kmapa <<- cluster.kmapa(colores = nuevos.colores)
    code.khoriz <<- cluster.khoriz(sel = paste0("'", input$sel.kmeans.cluster, "'"), colores = nuevos.colores, color = color)
    code.kvert <<- cluster.kvert(sel = paste0("'", input$sel.kmeans.verticales, "'"), colores = nuevos.colores)
    code.kradar <<- def.kradar(colores = nuevos.colores)
    code.kcat <<- cluster.kcat(var = input$sel.kcat.var)
    updateAceEditor(session, "fieldCodeKmapa", value = code.kmapa)
    updateAceEditor(session, "fieldCodeKhoriz", value = code.khoriz)
    updateAceEditor(session, "fieldCodeKvert", value = code.kvert)
    updateAceEditor(session, "fieldCodeKradar", value = code.kradar)
    updateAceEditor(session, "fieldCodeKbarras", value = code.kcat)
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

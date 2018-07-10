#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)

shinyServer(function(input, output, session) { 
  source('global.R', local = T)
  options(shiny.maxRequestSize=200*1024^2)
  options(DT.options = list(aLengthMenu = c(10, 30, 50), iDisplayLength = 10, scrollX = TRUE))

  observeEvent(c(input$file1, input$header, input$columname, input$sep, input$dec), {
    codigo <- code.carga(nombre.columnas = input$columname, ruta = input$file1$datapath, 
                         separador = input$sep, sep.decimal = input$dec, encabezado = input$header)
    updateAceEditor(session, "fieldCodeData", value = codigo)

     tryCatch({
       isolate(eval(parse(text = codigo)))
     }, error = function(e) {
       datos <<- NULL
       datos.originales <<- NULL
       return(NULL)
     })
    
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
    
    tryCatch({
      isolate(eval(parse(text = default.centros())))
      isolate(eval(parse(text = modelo.cor())))
      isolate(eval(parse(text = def.pca.model(scale.unit = input$switch.scale, npc = input$slider.npc))))
    }, error = function(e) {
      return(datos <- NULL)
    })
  })
  
  observeEvent(input$transButton, {
    var.activas <- c()
    code.res <- "datos <<- datos.originales \n"
    for (var in colnames(datos.originales)) {
      if(input[[paste0("box", var)]]) {
        var.activas <- c(var.activas, var)
        if(input[[paste0("sel", var)]] == "categorico" & class(datos.originales[, var]) %in% c("numeric","integer")) {
          code.res <- paste0(code.res, code.trans(var, "categorico"), "\n")
        }
        if(input[[paste0("sel", var)]] == "numerico" & !(class(datos.originales[, var]) %in% c("numeric","integer"))) {
          code.res <- paste0(code.res, code.trans(var, "numerico"), "\n")
        }
      }
    }
    updateAceEditor(session, "fieldCodeTrans", value = code.res)
    
    isolate(eval(parse(text = code.desactivar(var.activas))))
    isolate(eval(parse(text = code.res)))
    
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
    
    tryCatch({
      isolate(eval(parse(text = default.centros())))
      isolate(eval(parse(text = modelo.cor())))
      isolate(eval(parse(text = def.pca.model(scale.unit = input$switch.scale, npc = input$slider.npc))))
    }, error = function(e) {
      return(datos <- NULL)
    })
  })
  
  update <- reactive({
    inFile <- c(input$file1, input$transButton, input$header, input$columname, input$sep, input$dec)
    datos
  })
  output$contents = DT::renderDT(update(), selection = 'none', server = FALSE, editable = TRUE)
  
  update.trans <- reactive({
    inFile <- c(input$file1, input$header, input$columname, input$sep, input$dec)
    n <- ncol(datos)
    res <-  data.frame(Variables = colnames(datos), Tipo = c(1:n), Activa = c(1:n))
    res$Tipo <- sapply(colnames(datos), function(i) paste0('<select id="sel', i, '"> <option value="categorico">Categórico</option> 
      <option value="numerico" ', ifelse(class(datos[, i]) %in% c("numeric","integer"), ' selected="selected"', ''),
      '>Numérico</option> </select>'))
    res$Activa <- sapply(colnames(datos), function(i) paste0('<input type="checkbox" id="box', i, '" checked/>'))
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
      updateAceEditor(session, "fieldCodePCAModelo", value = def.pca.model(scale.unit = input$switch.scale, npc = input$slider.npc))
      isolate(eval(parse(text = def.pca.model(scale.unit = input$switch.scale, npc = input$slider.npc))))
    }, error = function(e) {
      return(NULL)
    })
  })
  
  output$resumen.completo = shiny::renderDataTable({
    return(obj.resum())
  }, options = list(dom = 'ft', scrollX = TRUE))
  
  output$resumen = renderUI({
    if(input$sel.resumen %in% colnames(var.numericas(datos))){
      HTML(resumen.numerico(datos, input$sel.resumen))
    } else {
      HTML(resumen.categorico(datos, input$sel.resumen))
    }
  })
  
  output$mostrar.atipicos = DT::renderDataTable({
    atipicos <- boxplot.stats(datos[, input$sel.distribucion.num]) 
    datos <- datos[datos[, input$sel.distribucion.num] %in% atipicos$out, input$sel.distribucion.num, drop = F]
    return(datos[order(datos[, input$sel.distribucion.num]), , drop = F])
  }, options = list(dom = 't', scrollX = TRUE, scrollY = "10vh"))
  
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
    kmedias <- kmeans(var.numericas(datos), as.numeric(input$cant.kmeans.cluster), iter.max = 200, nstart = 300) 
    HTML(resumen.kmeans(kmedias))
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
  
  obj.resum <- reactive({
    plotNo <- c(input$file1, input$transButton, input$header, input$columname, input$sep, input$dec, input$fieldCodeResum)
    isolate(eval(parse(text = input$fieldCodeResum)))
  })
  
  obj.disp <- reactive({
    plotNo <- c(input$file1, input$transButton, input$header, input$columname, input$sep, input$dec)
    cod.disp <<- input$fieldCodeDisp
    isolate(eval(parse(text = cod.disp)))
  })
  
  obj.cor <- reactive({
    plotNo <- c(input$file1, input$transButton, input$header, input$columname, input$sep, input$dec, input$fieldModelCor)
    cod.cor <<- input$fieldCodeCor
    return(isolate(eval(parse(text = cod.cor))))
  })
  
  obj.ind <- reactive({
    plotNo <- c(input$file1, input$transButton, input$header, input$columname, input$sep, 
                input$dec,input$pca.num, input$fieldModelCor, input$fieldCodePCAModelo)
    cod.pca[["individuos"]] <<- input$fieldCodeInd
    isolate(eval(parse(text = cod.pca[["individuos"]])))
  })
  
  obj.var <- reactive({
    plotNo <- c(input$file1, input$transButton, input$header, input$columname, input$sep, 
                input$dec, input$pca.num, input$fieldModelCor, input$fieldCodePCAModelo)
    cod.pca[["variables"]] <<- input$fieldCodeVar
    isolate(eval(parse(text = cod.pca[["variables"]])))
  })
  
  obj.biplot <- reactive({
    plotNo <- c(input$file1, input$transButton, input$header, input$columname, input$sep, 
                input$dec, input$pca.num, input$fieldModelCor, input$fieldCodePCAModelo)
    cod.pca[["sobreposicion"]] <<- input$fieldCodeBi
    isolate(eval(parse(text = cod.pca[["sobreposicion"]])))
  })
  
  obj.dya.num <- reactive({
    plotNo <- c(input$file1, input$transButton, input$header, input$columname, input$sep, 
                input$dec, input$fieldFuncNum, input$sel.distribucion.num, input$col.dist)
    cod.dya.num  <<- input$fieldCodeNum
    func.dya.num <<- input$fieldFuncNum
    isolate(eval(parse(text = func.dya.num)))
    return(isolate(eval(parse(text = cod.dya.num))))
  })
  
  obj.dya.cat <- reactive({
    plotNo <- c(input$file1, input$transButton, input$header, input$columname, input$sep, 
                input$dec, input$fieldFuncCat, input$sel.distribucion.cat, input$col.dist)
    cod.dya.cat  <<- input$fieldCodeCat
    func.dya.cat <<- input$fieldFuncCat
    isolate(eval(parse(text = func.dya.cat)))
    return(isolate(eval(parse(text = cod.dya.cat))))
  })
  
  obj.diagrama <- reactive({
    plotNo <- c(input$file1, input$transButton, input$header, input$columname, 
                input$sep, input$dec, input$fieldCodeDiag, input$fieldCodeModelo)
    code.diagrama <<- input$fieldCodeDiag
    isolate(eval(parse(text = code.diagrama)))
  })
  
  obj.mapa <- reactive({
    plotNo <- c(input$file1, input$transButton, input$header, input$columname, input$sep, 
                input$dec, input$fieldCodeMapa, input$fieldCodeModelo, input$fieldCodePCAModelo)
    code.mapa <<- input$fieldCodeMapa
    isolate(eval(parse(text = code.mapa)))
  })
  
  obj.horiz <- reactive({
    plotNo <- c(input$file1, input$transButton, input$header, input$columname, input$sep, input$dec, 
                input$fieldCodeHoriz, input$fieldFuncHoriz, input$fieldCodeCentr, input$fieldCodeModelo)
    code.horiz <<- input$fieldCodeHoriz
    func.horiz <<- input$fieldFuncHoriz
    func.centros <<- input$fieldCodeCentr
    isolate(eval(parse(text = func.horiz)))
    isolate(eval(parse(text = func.centros)))
    return(isolate(eval(parse(text = code.horiz))))
  })
  
  obj.vert <- reactive({
    plotNo <- c(input$file1, input$transButton, input$header, input$columname, input$sep, input$dec, 
                input$fieldCodeVert, input$fieldCodeVert, input$fieldCodeCentr, input$fieldCodeModelo)
    code.vert <<- input$fieldCodeVert
    func.vert <<- input$fieldFuncVert
    func.centros <<- input$fieldCodeCentr
    isolate(eval(parse(text = func.vert)))
    isolate(eval(parse(text = func.centros)))
    return(isolate(eval(parse(text = code.vert))))
  })
  
  obj.radar <- reactive({
    plotNo <- c(input$file1, input$transButton, input$header, input$columname, input$sep, input$dec, 
                input$fieldCodeRadar, input$fieldFuncRadar, input$fieldCodeCentr, input$fieldCodeModelo)
    code.radar <<- input$fieldCodeRadar
    func.radar <<- input$fieldFuncRadar
    func.centros <<- input$fieldCodeCentr
    isolate(eval(parse(text = func.centros)))
    isolate(eval(parse(text = func.radar)))
    return(isolate(eval(parse(text = code.radar))))
  })
  
  obj.bar.cat <- reactive({
    plotNo <- c(input$file1, input$transButton, input$header, input$columname, 
                input$sep, input$dec, input$fieldCodeBarras, input$fieldCodeModelo)
    code.cat <<- input$fieldCodeBarras
    return(isolate(eval(parse(text = code.cat))))
  })
  
  obj.jambu <- reactive({
    plotNo <- c(input$file1, input$transButton, input$header, input$columname, input$sep, 
                input$dec, input$fieldCodeJambu, input$fieldCodeKModelo, input$fieldFuncJambu)
    isolate(eval(parse(text = input$fieldFuncJambu)))
    return(isolate(eval(parse(text = input$fieldCodeJambu))))
  })
  
  obj.kmapa <- reactive({
    plotNo <- c(input$file1, input$transButton, input$header, input$columname, input$sep, 
                input$dec, input$fieldCodeKmapa, input$fieldCodeKModelo, input$fieldCodePCAModelo)
    code.kmapa <<- input$fieldCodeKmapa
    isolate(eval(parse(text = code.kmapa)))
  })
  
  obj.khoriz <- reactive({
    plotNo <- c(input$file1, input$transButton, input$header, input$columname, input$sep,
                input$dec, input$fieldCodeKhoriz, input$fieldFuncKhoriz, input$fieldCodeKModelo)
    code.khoriz <<- input$fieldCodeKhoriz
    func.khoriz <<- input$fieldFuncKhoriz
    isolate(eval(parse(text = func.khoriz)))
    return(isolate(eval(parse(text = code.khoriz))))
  })
  
  obj.kvert <- reactive({
    plotNo <- c(input$file1, input$transButton, input$header, input$columname, input$sep,
                input$dec, input$fieldCodeKvert, input$fieldFuncKvert, input$fieldCodeKModelo)
    code.kvert <<- input$fieldCodeKvert
    func.kvert <<- input$fieldFuncKvert
    isolate(eval(parse(text = func.kvert)))
    return(isolate(eval(parse(text = code.kvert))))
  })
  
  obj.kradar <- reactive({
    plotNo <- c(input$file1, input$transButton, input$header, input$columname, input$sep, 
                input$dec, input$fieldCodeKradar, input$fieldFuncKradar, input$fieldCodeKModelo)
    code.kradar <<- input$fieldCodeKradar
    func.kradar <<- input$fieldFuncKradar
    isolate(eval(parse(text = func.kradar)))
    return(isolate(eval(parse(text = code.kradar))))
  })
  
  obj.kcat <- reactive({
    plotNo <- c(input$file1, input$transButton, input$header, input$columname, 
                input$sep, input$dec, input$fieldCodeKbarras, input$fieldCodeKModelo)
    code.kcat <<- input$fieldCodeKbarras
    return(isolate(eval(parse(text = code.kcat))))
  })
  
  observe({
    updateSelectInput(session, "sel.cluster", choices = c("Todos", 1:input$cant.cluster))
    updateSelectInput(session, "sel.kmeans.cluster", choices = c("Todos", 1:input$cant.kmeans.cluster))
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
    
    updateAceEditor(session, "fieldCodeReport", value = def.reporte())
  })
  
  observeEvent(c(input$sel.distribucion.num), {
    cod.dya.num <<- def.code.num(data = "datos", variable = paste0("'", input$sel.distribucion.num, "'"))
    updateAceEditor(session, "fieldCodeNum", value = cod.dya.num)
  })
  
  observeEvent(c(input$sel.distribucion.cat), {
    cod.dya.cat <<- def.code.cat(data = "datos", variable = paste0("'", input$sel.distribucion.cat, "'"))
    updateAceEditor(session, "fieldCodeCat", value = cod.dya.cat)
  })
  
  observeEvent(c(input$select.var), {
    cod.disp <<- default.disp(data = "datos", vars = input$select.var)
    updateAceEditor(session, "fieldCodeDisp", value = cod.disp)
  })
  
  observeEvent(c(input$cor.metodo, input$cor.tipo), {
    cod.cor <<- correlaciones(metodo = input$cor.metodo, tipo = input$cor.tipo)
    updateAceEditor(session, "fieldCodeCor", value = cod.cor)
  })
  
  observeEvent(c(input$file1, input$header, input$columname, input$sep, input$dec, input$cant.cluster, 
                 input$transButton, input$sel.dist.method, input$sel.hc.method), {
    tryCatch ({
      isolate(eval(parse(text = def.model(data = "datos", cant = input$cant.cluster, 
                                          dist.method = input$sel.dist.method, hc.method = input$sel.hc.method))))
      updateAceEditor(session, "fieldCodeModelo", value = def.model(data = "datos", cant = input$cant.cluster, 
                                                                    dist.method = input$sel.dist.method, hc.method = input$sel.hc.method))
    }, error = function(e) {
      print(e)
      return(NULL)
    })
    
    code.diagrama <<- diagrama(cant = input$cant.cluster)
    code.mapa <<- cluster.mapa(cant = input$cant.cluster)
    code.horiz <<- cluster.horiz(sel = paste0("'", input$sel.cluster, "'"))
    code.vert <<- cluster.vert(sel = paste0("'", input$sel.cluster, "'"))
    code.radar <<- def.radar()
    code.cat <<- cluster.cat(var = input$sel.cat.var, cant = as.numeric(input$cant.cluster))
    updateAceEditor(session, "fieldCodeDiag", value = code.diagrama)
    updateAceEditor(session, "fieldCodeMapa", value = code.mapa)
    updateAceEditor(session, "fieldCodeHoriz", value = code.horiz)
    updateAceEditor(session, "fieldCodeVert", value = code.vert)
    updateAceEditor(session, "fieldCodeRadar", value = code.radar)
    updateAceEditor(session, "fieldCodeBarras", value = code.cat)
  })
  
  observeEvent(input$sel.cluster, {
    code.horiz <<- cluster.horiz(sel = paste0("'", input$sel.cluster, "'"))
    updateAceEditor(session, "fieldCodeHoriz", value = code.horiz)
  })
  
  observeEvent(input$sel.verticales, {
    code.vert <<- cluster.vert(sel = paste0("'", input$sel.verticales, "'"))
    updateAceEditor(session, "fieldCodeVert", value = code.vert)
  })
  
  observeEvent(input$sel.cat.var, {
    code.cat <<- cluster.cat(var = input$sel.cat.var)
    updateAceEditor(session, "fieldCodeBarras", value = code.cat)
  })
  
  observeEvent(c(input$file1, input$header, input$columname, input$sep, input$dec,
                 input$cant.kmeans.cluster, input$transButton, input$num.iter, input$slider.nstart), {
    tryCatch ({
      isolate(eval(parse(text = def.k.model(data = "datos", cant = input$cant.kmeans.cluster,
                                            iter.max = input$num.iter, nstart = input$slider.nstart))))
      updateAceEditor(session, "fieldCodeKModelo", value = def.k.model(data = "datos", cant = input$cant.kmeans.cluster,
                                                                       iter.max = input$num.iter, nstart = input$slider.nstart))
    }, error = function(e) {
      return(NULL)
    })
    
    code.kmapa <<- cluster.kmapa()
    code.khoriz <<- cluster.khoriz(sel = paste0("'", input$sel.kmeans.cluster, "'"))
    code.kvert <<- cluster.kvert(sel = paste0("'", input$sel.kmeans.cluster, "'"))
    code.kradar <<- def.kradar()
    code.kcat <<- cluster.kcat(var = input$sel.kcat.var)
    updateAceEditor(session, "fieldCodeKmapa", value = code.kmapa)
    updateAceEditor(session, "fieldCodeKhoriz", value = code.khoriz)
    updateAceEditor(session, "fieldCodeKvert", value = code.kvert)
    updateAceEditor(session, "fieldCodeKradar", value = code.kradar)
    updateAceEditor(session, "fieldCodeKbarras", value = code.kcat)
  })
  
  observeEvent(input$sel.kmeans.cluster, {
    code.khoriz <<- cluster.khoriz(sel = paste0("'", input$sel.kmeans.cluster, "'"))
    updateAceEditor(session, "fieldCodeKhoriz", value = code.khoriz)
  })
  
  observeEvent(input$sel.kmeans.verticales, {
    code.kvert <<- cluster.kvert(sel = paste0("'", input$sel.kmeans.verticales, "'"))
    updateAceEditor(session, "fieldCodeKvert", value = code.kvert)
  })
  
  observeEvent(input$sel.kcat.var, {
    code.kcat <<- cluster.kcat(var = input$sel.kcat.var)
    updateAceEditor(session, "fieldCodeKbarras", value = code.kcat)
  })
  
  observeEvent(c(input$ind.cos, input$col.pca.ind), {
    tryCatch({
      cod.pca[["individuos"]] <<- pca.individuos(ind.cos = input$ind.cos * 0.01, color = input$col.pca.ind)
      cod.pca[["sobreposicion"]] <<- pca.sobreposicion(ind.cos = input$ind.cos * 0.01, var.cos = input$var.cos * 0.01,
                                                       col.ind = input$col.pca.ind, col.var = input$col.pca.var)
      updateAceEditor(session, "fieldCodeBi", value = cod.pca[["sobreposicion"]])
      updateAceEditor(session, "fieldCodeInd", value = cod.pca[["individuos"]])
    }, error = function(e) {
      return(datos <- NULL)
    })
  })
  
  observeEvent(c(input$var.cos, input$col.pca.var), {
    tryCatch({
      cod.pca[["variables"]] <<- pca.variables(var.cos = input$var.cos * 0.01, color = input$col.pca.var)
      cod.pca[["sobreposicion"]] <<- pca.sobreposicion(ind.cos = input$ind.cos * 0.01, var.cos = input$var.cos * 0.01,
                                                       col.ind = input$col.pca.ind, col.var = input$col.pca.var)
      updateAceEditor(session, "fieldCodeBi", value = cod.pca[["sobreposicion"]])
      updateAceEditor(session, "fieldCodeVar", value = cod.pca[["variables"]])
    }, error = function(e) {
      print(e)
      return(datos <- NULL)
    })
  })
  
  output$knitDoc <- renderUI({
    c(input$fieldCodeReport)
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
      out <- render(src,  params = NULL, html_document())
      file.rename(out, paste('data-', Sys.Date(), '.html', sep=''))
      files <- c(paste('data-', Sys.Date(), '.html', sep=''), files)
      
      zip(file, files)
    }
  )
})




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
    stopApp()
  })

  observeEvent(input$loadButton, {
    codigo <- code.carga(nombre.filas = input$rowname, ruta = input$file1$datapath,
                         separador = input$sep, sep.decimal = input$dec, encabezado = input$header)
    updateAceEditor(session, "fieldCodeData", value = codigo)

     tryCatch({
       isolate(eval(parse(text = codigo)))
     }, error = function(e) {
       print(paste0("ERROR EN CARGAR: ", e))
       datos <<- NULL
       datos.originales <<- NULL
       return(NULL)
     })

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

    tryCatch({
      isolate(eval(parse(text = default.centros())))
      isolate(eval(parse(text = modelo.cor())))
      isolate(eval(parse(text = def.pca.model(scale.unit = input$switch.scale, npc = input$slider.npc))))
      output$txtpca <- renderPrint(print(unclass(pca.modelo)))
      output$txtcor <- renderPrint(print(correlacion))
    }, error = function(e) {
      return(datos <- NULL)
    })
  })

  observeEvent(input$transButton, {
    var.noactivas <- c()
    code.res <- "datos <<- datos.originales \n"
    for (var in colnames(datos.originales)) {
      if(input[[paste0("box", var)]]) {
        if(input[[paste0("sel", var)]] == "categorico" & class(datos.originales[, var]) %in% c("numeric","integer")) {
          code.res <- paste0(code.res, code.trans(var, "categorico"), "\n")
        }
        if(input[[paste0("sel", var)]] == "numerico" & !(class(datos.originales[, var]) %in% c("numeric","integer"))) {
          code.res <- paste0(code.res, code.trans(var, "numerico"), "\n")
        }
        if(input[[paste0("sel", var)]] == "disyuntivo"){
          code.res <- paste0(code.res, code.trans(var, "disyuntivo"), "\n")
        }
      } else {
        var.noactivas <- c(var.noactivas, var)
      }
    }

    isolate(eval(parse(text = code.res)))
    if(length(var.noactivas) > 0)
      isolate(eval(parse(text = code.desactivar(var.noactivas))))

    updateAceEditor(session, "fieldCodeTrans", value = code.res)

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

    tryCatch({
      isolate(eval(parse(text = default.centros())))
      isolate(eval(parse(text = modelo.cor())))
      isolate(eval(parse(text = def.pca.model(scale.unit = input$switch.scale, npc = input$slider.npc))))
      output$txtpca <- renderPrint(print(unclass(pca.modelo)))
      output$txtcor <- renderPrint(print(correlacion))
    }, error = function(e) {
      return(datos <- NULL)
    })
  })

  mostrarData <- eventReactive(c(input$loadButton, input$transButton), {
    nombre.columnas <- c("ID", colnames(datos))
    tipo.columnas <- c("", sapply(colnames(datos),
                           function(i) ifelse(class(datos[,i]) %in% c("numeric", "integer"), "Numérico", "Categórico")))
    sketch = htmltools::withTags(table(
      tableHeader(nombre.columnas),
      tableFooter(tipo.columnas)
    ))
    return(DT::datatable(datos, selection = 'none', editable = TRUE, extensions = 'Buttons', container = sketch,
              options = list(dom = 'Bfrtip', buttons = list(list(extend = 'csv', filename = "datos", text = 'Descargar')))))
  })
  output$contents = DT::renderDT(mostrarData())

  update.trans <- reactive({
    inFile <- c(input$loadButton)
    n <- ncol(datos)
    res <-  data.frame(Variables = colnames(datos), Tipo = c(1:n), Activa = c(1:n))
    res$Tipo <- sapply(colnames(datos), function(i) paste0('<select id="sel', i, '"> <option value="categorico">Categórico</option>
      <option value="numerico" ', ifelse(class(datos[, i]) %in% c("numeric","integer"), ' selected="selected"', ''),
      '>Numérico</option> <option value="disyuntivo">Disyuntivo</option> </select>'))
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
      output$txtpca <- renderPrint(print(unclass(pca.modelo)))
    }, error = function(e) {
      print(paste0("ERROR EN PCA: ", e))
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
    data.frame(unclass(summary(datos)), check.names = FALSE, stringsAsFactors = FALSE)
  })

  obj.normal <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeNormal), {
    cod.normal <<- input$fieldCodeNormal
    isolate(eval(parse(text = cod.normal)))
  })

  obj.disp <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeDisp), {
    cod.disp <<- input$fieldCodeDisp
    isolate(eval(parse(text = cod.disp)))
  })

  obj.cor <- eventReactive(c(input$loadButton, input$transButton, input$fieldModelCor, input$fieldCodeCor), {
    cod.cor <<- input$fieldCodeCor
    return(isolate(eval(parse(text = cod.cor))))
  })

  obj.ind <- eventReactive(c(input$loadButton, input$transButton, input$pca.num, input$fieldCodeInd, input$fieldCodePCAModelo), {
    cod.pca[["individuos"]] <<- input$fieldCodeInd
    isolate(eval(parse(text = cod.pca[["individuos"]])))
  })

  obj.var <- eventReactive(c(input$loadButton, input$transButton, input$pca.num, input$fieldCodeVar, input$fieldCodePCAModelo), {
    cod.pca[["variables"]] <<- input$fieldCodeVar
    isolate(eval(parse(text = cod.pca[["variables"]])))
  })

  obj.biplot <- eventReactive(c(input$loadButton, input$transButton, input$pca.num, input$fieldCodeBi, input$fieldCodePCAModelo), {
    cod.pca[["sobreposicion"]] <<- input$fieldCodeBi
    isolate(eval(parse(text = cod.pca[["sobreposicion"]])))
  })

  obj.vee <- eventReactive(c(input$loadButton, input$transButton, input$switch.scale, input$slider.npc), {
    codigo <- code.pca.vee()
    updateAceEditor(session, "fieldCodeAyuda", value = codigo)
    return(isolate(eval(parse(text = codigo))))

  })

  obj.cci <- eventReactive(c(input$loadButton, input$transButton, input$switch.scale, input$slider.npc), {
    codigo <- code.pca.cci()
    updateAceEditor(session, "fieldCodeAyuda", value = codigo)
    return(isolate(eval(parse(text = codigo))))

  })

  obj.ccv <- eventReactive(c(input$loadButton, input$transButton, input$switch.scale, input$slider.npc), {
    codigo <- code.pca.ccv()
    updateAceEditor(session, "fieldCodeAyuda", value = codigo)
    return(isolate(eval(parse(text = codigo))))

  })

  obj.cvc <- eventReactive(c(input$loadButton, input$transButton, input$switch.scale, input$slider.npc), {
    codigo <- code.pca.cvp()
    updateAceEditor(session, "fieldCodeAyuda", value = codigo)
    return(isolate(eval(parse(text = codigo))))
  })

  obj.cp1 <- eventReactive(c(input$loadButton, input$transButton, input$switch.scale, input$slider.npc), {
    codigo <- code.pca.pc1()
    updateAceEditor(session, "fieldCodeAyuda", value = codigo)
    return(isolate(eval(parse(text = codigo))))

  })

  obj.cp2 <- eventReactive(c(input$loadButton, input$transButton, input$switch.scale, input$slider.npc), {
    codigo <- code.pca.pc2()
    updateAceEditor(session, "fieldCodeAyuda", value = codigo)
    return(isolate(eval(parse(text = codigo))))
  })

  obj.dya.num <- eventReactive(c(input$loadButton, input$transButton, input$fieldFuncNum, input$fieldCodeNum), {
    cod.dya.num  <<- input$fieldCodeNum
    func.dya.num <<- input$fieldFuncNum
    isolate(eval(parse(text = func.dya.num)))
    return(isolate(eval(parse(text = cod.dya.num))))
  })

  obj.dya.cat <- eventReactive(c(input$loadButton, input$transButton, input$fieldFuncCat, input$fieldCodeCat), {
    cod.dya.cat  <<- input$fieldCodeCat
    func.dya.cat <<- input$fieldFuncCat
    isolate(eval(parse(text = func.dya.cat)))
    return(isolate(eval(parse(text = cod.dya.cat))))
  })

  obj.diagrama <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeDiag, input$fieldCodeModelo), {
    code.diagrama <<- input$fieldCodeDiag
    isolate(eval(parse(text = code.diagrama)))
  })

  obj.mapa <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeMapa, input$fieldCodeModelo, input$fieldCodePCAModelo), {
    code.mapa <<- input$fieldCodeMapa
    isolate(eval(parse(text = code.mapa)))
  })

  obj.horiz <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeHoriz,
                               input$fieldFuncHoriz, input$fieldCodeCentr, input$fieldCodeModelo), {
    code.horiz <<- input$fieldCodeHoriz
    func.horiz <<- input$fieldFuncHoriz
    func.centros <<- input$fieldCodeCentr
    isolate(eval(parse(text = func.horiz)))
    isolate(eval(parse(text = func.centros)))
    return(isolate(eval(parse(text = code.horiz))))
  })

  obj.vert <- eventReactive(c(input$loadButton, input$transButton,
                              input$fieldCodeVert, input$fieldCodeVert, input$fieldCodeCentr, input$fieldCodeModelo), {
    code.vert <<- input$fieldCodeVert
    func.vert <<- input$fieldFuncVert
    func.centros <<- input$fieldCodeCentr
    isolate(eval(parse(text = func.vert)))
    isolate(eval(parse(text = func.centros)))
    return(isolate(eval(parse(text = code.vert))))
  })

  obj.radar <- eventReactive(c(input$loadButton, input$transButton,
                               input$fieldCodeRadar, input$fieldFuncRadar, input$fieldCodeCentr, input$fieldCodeModelo), {
    code.radar <<- input$fieldCodeRadar
    func.radar <<- input$fieldFuncRadar
    func.centros <<- input$fieldCodeCentr
    isolate(eval(parse(text = func.centros)))
    isolate(eval(parse(text = func.radar)))
    return(isolate(eval(parse(text = code.radar))))
  })

  obj.bar.cat <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeBarras, input$fieldCodeModelo), {
    code.cat <<- input$fieldCodeBarras
    return(isolate(eval(parse(text = code.cat))))
  })

  obj.inercia <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeKModelo), {
    return(HTML(resumen.kmeans(k.modelo)))
  })

  obj.jambu <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeJambu, input$fieldCodeKModelo, input$fieldFuncJambu), {
    isolate(eval(parse(text = input$fieldFuncJambu)))
    return(isolate(eval(parse(text = input$fieldCodeJambu))))
  })

  obj.kmapa <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeKmapa, input$fieldCodeKModelo, input$fieldCodePCAModelo), {
    code.kmapa <<- input$fieldCodeKmapa
    isolate(eval(parse(text = code.kmapa)))
  })

  obj.khoriz <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeKhoriz, input$fieldFuncKhoriz, input$fieldCodeKModelo), {
    code.khoriz <<- input$fieldCodeKhoriz
    func.khoriz <<- input$fieldFuncKhoriz
    isolate(eval(parse(text = func.khoriz)))
    return(isolate(eval(parse(text = code.khoriz))))
  })

  obj.kvert <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeKvert, input$fieldFuncKvert, input$fieldCodeKModelo), {
    code.kvert <<- input$fieldCodeKvert
    func.kvert <<- input$fieldFuncKvert
    isolate(eval(parse(text = func.kvert)))
    return(isolate(eval(parse(text = code.kvert))))
  })

  obj.kradar <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeKradar, input$fieldFuncKradar, input$fieldCodeKModelo), {
    code.kradar <<- input$fieldCodeKradar
    func.kradar <<- input$fieldFuncKradar
    isolate(eval(parse(text = func.kradar)))
    return(isolate(eval(parse(text = code.kradar))))
  })

  obj.kcat <- eventReactive(c(input$loadButton, input$transButton, input$fieldCodeKbarras, input$fieldCodeKModelo), {
    code.kcat <<- input$fieldCodeKbarras
    return(isolate(eval(parse(text = code.kcat))))
  })

  observe({
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

    updateAceEditor(session, "fieldCodeReport", value = def.reporte(input))
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

  observeEvent(c(input$sel.normal, input$col.normal), {
    cod.normal <<- default.normal(data = "datos", vars = input$sel.normal, color = input$col.normal)
    updateAceEditor(session, "fieldCodeNormal", value = cod.normal)
  })

  observeEvent(c(input$select.var, input$col.disp), {
    cod.disp <<- default.disp(data = "datos", vars = input$select.var, color = input$col.disp)
    updateAceEditor(session, "fieldCodeDisp", value = cod.disp)
  })

  observeEvent(c(input$cor.metodo, input$cor.tipo), {
    cod.cor <<- correlaciones(metodo = input$cor.metodo, tipo = input$cor.tipo)
    updateAceEditor(session, "fieldCodeCor", value = cod.cor)
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
    code.vert <<- cluster.vert(sel = paste0("'", input$sel.cluster, "'"), colores = nuevos.colores)
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
    code.vert <<- cluster.vert(sel = paste0("'", input$sel.cluster, "'"), colores = nuevos.colores)
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
    code.kvert <<- cluster.kvert(sel = paste0("'", input$sel.kmeans.cluster, "'"), colores = nuevos.colores)
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
    code.kvert <<- cluster.kvert(sel = paste0("'", input$sel.kmeans.cluster, "'"), colores = nuevos.colores)
    code.kradar <<- def.kradar(colores = nuevos.colores)
    code.kcat <<- cluster.kcat(var = input$sel.kcat.var)
    updateAceEditor(session, "fieldCodeKmapa", value = code.kmapa)
    updateAceEditor(session, "fieldCodeKhoriz", value = code.khoriz)
    updateAceEditor(session, "fieldCodeKvert", value = code.kvert)
    updateAceEditor(session, "fieldCodeKradar", value = code.kradar)
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
      print(paste0("ERROR EN PCA: ", e))
      return(datos <- NULL)
    })
  })

  output$knitDoc <- renderUI({
    obj.reporte()
  })

  obj.reporte <- eventReactive(input$btnReporte, {
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

      zip(file, files)
    }
  )
})

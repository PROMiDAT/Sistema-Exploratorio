

###################### Variables ##############################################
#' Carga de Variables
#' @author Diego
#' @return
#' @export
#'
contador <<- 0
datos <<- NULL
datos.originales <<- NULL
nombre.datos <<- ""
datos.reporte <<- list()
env.report <<- new.env()
env.report$codigo.reporte <- list()
def.colors <<- c("#F8766D", "#00BFC4", "#00BA38", "#C77CFF", "#00B0F6",
                 "#EEEE00", "#CD661D", "#006400","#EE82EE", "#000080")

###################### Funciones R ############################################
#' Funciones R
#' @author Diego
#' @return functions
#' @export
#'
overwrite.cat <- function(){
  unlockBinding("cat", .BaseNamespaceEnv)

  .BaseNamespaceEnv$cat <- function(..., file = "", sep = " ", fill = FALSE,
                                    labels = NULL, append = FALSE) {
    file <- stderr()
    sep <- ""

    msg <- .makeMessage(..., domain = NULL, appendLF = TRUE)
    call <- sys.call()
    cond <- simpleMessage(msg, call)

    if (is.character(file))
      if (file == "")
        file <- stdout()
    else if (substring(file, 1L, 1L) == "|") {
      file <- pipe(substring(file, 2L), "w")
      on.exit(close(file))
    }
    else {
      file <- file(file, ifelse(append, "a", "w"))
      on.exit(close(file))
    }
    defaultHandler <- function(c) {
      base:::.Internal(cat(as.list(conditionMessage(c)), file, sep, fill, labels, append))
    }
    withRestarts({
      signalCondition(cond)
      defaultHandler(cond)
    }, muffleMessage = function() NULL)
    invisible()
  }

  lockBinding("cat",.BaseNamespaceEnv)
}

recover.cat <- function(){
  unlockBinding("cat", .BaseNamespaceEnv)

  .BaseNamespaceEnv$cat <- function (..., file = "", sep = " ", fill = FALSE, labels = NULL,
                                     append = FALSE)
  {
    if (is.character(file))
      if (file == "")
        file <- stdout()
      else if (substring(file, 1L, 1L) == "|") {
        file <- pipe(substring(file, 2L), "w")
        on.exit(close(file))
      }
      else {
        file <- file(file, ifelse(append, "a", "w"))
        on.exit(close(file))
      }
      .Internal(cat(list(...), file, sep, fill, labels, append))
  }

  lockBinding("cat",.BaseNamespaceEnv)
}

error.variables <- function(num = T){
  if(num){
    img <- raster::stack("www/errorNumericas.png")
  }else{
    img <- raster::stack("www/errorCategoricas.png")
  }
  raster::plotRGB(img)
}

###################### Funciones Shiny ########################################
#' Funciones shiny
#' @author Diego
#' @return functions
#' @export
#'
createLog <- function(titulo = "", code = ""){
  paste0("\n\n## ", titulo, "\n\n#### Interpretación\n```{r}\n", code, "\n```")
}

extract.code <- function(funcion) {
  code <- paste(head(eval(parse(text = funcion)), 100), collapse = "\n")
  code <- paste(funcion, "<-", code)
  return(code)
}

tabsOptions <- function(botones = list(icon("gear"), icon("terminal")),
                        widths = c(50, 100), heights = c(100, 50),
                        tabs.content = list("", "")) {
  res <- ""
  codeButtons <- ""
  cant <- length(botones)
  if(cant == 2) {widgets <- c("left", "right")}
  if(cant == 3) {widgets <- c("left", "center", "right")}
  if(cant == 4) {widgets <- c("left", "centerleft", "centeright", "right")}
  if(cant == 5) {widgets <- c("left", "centerleft", "center", "centeright", "right")}
  for (i in 1:cant) {
    res <- paste0(res, tags$div(class = paste0("box-option box-option-", widgets[i]),
                                style = paste0("width:", widths[i], "%;height:",
                                               heights[i], "%;"),
                                tabs.content[[i]]), "\n")
    codeButtons <- paste0(codeButtons, "<button style='width:",
                          100/cant, "%' data-widget='", widgets[i], "'>",
                          botones[[i]], "</button>\n")
  }
  res <- paste0(res, tags$div(class = "btn-options", style = "position:relative;",
                              width = "100%", HTML(codeButtons)))
  return(tags$div(HTML(res)))
}

campo.codigo <- function(runid, refid, fieldid, ...) {
  tags$div(class = "box box-solid bg-black",
           tags$div(style = "text-align:right;padding-right: 10px;",
                    tags$button(id = runid, type = "button",
                                class = "run-button action-button",
                                icon("play"),
                                tags$a("Ejecutar", style = "color:white"))),
           tags$div(class = "box-body",
                    aceEditor(fieldid, mode = "r", theme = "monokai",
                              value = "", ...)))
}

infoBoxPROMiDAT <- function(titulo, valor, icono) {
  tags$div(class = "info-box bg-promidat",
           tags$span(class = "info-box-icon", icono),
           tags$div(class="info-box-content",
                    tags$span(class = "info-box-text", titulo),
                    tags$span(class = "info-box-number", valor)
           )
  )
}

###################### Carga de Datos #########################################
#' Funciones Carga de Datos
#' @author Diego
#' @return functions
#' @export
#'
colnames.empty <- function(res) {
  res <- colnames(res)
  if (is.null(res)) {
    return("")
  }
  return(res)
}

var.numericas <- function(data){
  if(is.null(data)) {
    return(NULL)
  }
  res <- base::subset(data, select =
                        sapply(data, class) %in% c('numeric', 'integer'))
  return(res)
}

var.categoricas <- function(data){
  if(is.null(data)) {
    return(NULL)
  }
  res <- base::subset(data, select =
                        !sapply(data, class) %in% c('numeric', 'integer'))
  return(res)
}

datos.disyuntivos <- function(data, vars){
  if(is.null(data)) {
    return(NULL)
  }
  cualitativas <- base::subset(data, select = colnames(data) %in% c(vars))
  data <- data[, !colnames(data) %in% vars]
  for (variable in colnames(cualitativas)) {
    for (categoria in unique(cualitativas[, variable])) {
      nueva.var <- as.numeric(cualitativas[, variable] == categoria)
      data <- cbind(data, nueva.var)
      colnames(data)[length(colnames(data))] <-
        paste0(variable, '.', categoria)
    }
  }
  return(data)
}

code.carga <- function(nombre.filas = T, ruta = NULL, separador = ";",
                       sep.decimal = ",", encabezado = T){
  if(!is.null(ruta)){
    ruta <- gsub("\\", "/", ruta, fixed = T)
  }
  res <- paste0("datos.originales <<- read.table('", ruta, "', header=",
                encabezado, ", sep='", separador, "', dec = '", sep.decimal,
                "'", ifelse(nombre.filas, ", row.names = 1", ""),
                ")\ndatos <<- datos.originales")
  return(res)
}

code.NA <- function(deleteNA = T) {
  res <- ifelse(deleteNA, "datos.originales <<- na.omit(datos.originales)\n",
                paste0("Mode <- function(x) {\n  x[which.max(summary(x))]\n}\n",
                       "for (variable in colnames(datos.originales)) {\n",
                       "  if(any(is.na(datos.originales[, variable]))){\n",
                       "    ifelse(class(datos.originales[, variable]) %in% c('numeric', 'integer'),\n",
                       "           datos.originales[, variable][is.na(datos.originales[, variable])] <<- \n",
                       "                                              mean(datos.originales[, variable], na.rm = T),\n",
                       "           datos.originales[, variable][is.na(datos.originales[, variable])] <<- \n",
                       "                                     Mode(datos.originales[, variable]))",
                       "\n   }\n}"))
  return(res)
}

code.trans <- function(variable, nuevo.tipo) {
  if(nuevo.tipo == "categorico"){
    return(paste0("datos[, '", variable, "'] <<- as.factor(datos[, '",
                  variable, "'])"))
  } else if(nuevo.tipo == "numerico") {
    return(paste0("datos[, '", variable, "'] <<- as.numeric(sub(',', '.', datos[, '",
                  variable, "'], fixed = TRUE))"))
  } else {
    es.factor <- ifelse(class(datos.originales[, variable]) %in%
                          c('numeric', 'integer'),
                        paste0("datos[, '", variable, "'] <<- as.factor(datos[, '",
                               variable, "']) \n"), "")
    return(paste0(es.factor, "datos <<- datos.disyuntivos(datos, '",
                  variable,"')"))
  }
}

code.desactivar <- function(variables){
  return(paste0("datos <<- subset(datos, select = -c(",
                paste(variables, collapse = ","), "))"))
}

###################### Estadisticas Basicas ###################################
#' Funciones Resumen Numérico
#' @author Diego
#' @return functions
#' @export
#'
resumen.numerico <- function(data, variable) {
  salida <- ""
  datos.numericos <- list(
    Q1 = list(id = "q1", Label = "Primer Cuartil", Value = format(round(quantile(
      data[, variable], .25), 3), scientific = F), color = "green"),
    Mediana = list(id = "mediana", Label = "Mediana", Value = format(round(median(
      data[, variable]), 3), scientific = F), color = "orange"),
    Q3 = list(id = "q3", Label = "Tercer Cuartil", Value = format(round(quantile(
      data[, variable], .75), 3), scientific = F), color = "maroon"),
    Minimo = list(id = "minimo", Label = "Mínimo", Value = format(round(min(
      data[, variable]), 3), scientific = F), color = "red"),
    Promedio = list(id = "promedio", Label = "Promedio", Value = format(round(mean(
      data[, variable]), 3), scientific = F), color = "blue"),
    Maximo = list(id = "maximo", Label = "Máximo", Value = format(round(max(
      data[, variable]), 3), scientific = F), color = "purple"),
    DS <- list(id = "ds", Label = "Desviación Estandar", Value = format(round(sd(
      data[, variable]), 3), scientific = FALSE, nsmall = 3), color = "yellow"))

  for (calculo in datos.numericos) {
    salida <- paste0(
      salida, "<div class='shiny-html-output col-sm-6 shiny-bound-output' id='",
      calculo$id, "'> <div class='small-box bg-", calculo$color,
      "'> <div class='inner'>", "<h3>", calculo$Value, "</h3> <p>", calculo$Label,
      "</p></div> <div class='icon-large'> <i class='", calculo$icon,
      "'></i></div></div></div>")
  }
  return(salida)
}

resumen.categorico <- function(data, variable){
  salida <- ""
  color <- c("red","yellow","aqua","navy","teal","olive","purple","maroon",
             "black","blue","lime","orange","light-blue","green","fuchsia")
  datos.categoricos <- summary(data[, variable])
  for (i in 1:length(datos.categoricos)) {
    salida <- paste0(
      salida, "<div class='shiny-html-output col-sm-6 shiny-bound-output' id='",
      variable, i, "'> <div class='small-box bg-", sample(color, 1),
      "'> <div class='inner'>", "<h3>", datos.categoricos[i], "</h3> <p>",
      levels(data[, variable])[i],
      "</p></div> <div class='icon-large'> <i class=''></i></div></div></div>")
  }
  return(salida)
}

#' Funciones Normal
#' @author Diego
#' @return functions
#' @export
#'
default.normal <- function(data = "datos", vars = NULL, color = "#00FF22AA"){
  if(is.null(vars)){
    return(NULL)
  } else {
    return(paste0(
      "promedio <- mean(", data, "[, '", vars, "']) \n",
      "desviacion <- sd(", data, "[, '", vars, "']) \n",
      "values <- dnorm(", data, "[, '", vars, "'], mean = promedio, sd = desviacion) \n",
      "values <- c(values, hist(", data, "[, '", vars, "'],  plot = F)$density) \n",
      "hist(", data, "[, '", vars, "'], col = '", color, "', border=F, axes=F,\n",
      "  freq = F, ylim = range(0, max(values)), \n",
      "  main = paste0('Test de normalidad de la variable ','", vars, "')) \n",
      "axis(1, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025) \n",
      "axis(2, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025) \n",
      "curve(dnorm(x, mean = promedio, sd = desviacion), add=T, col='blue', lwd=2)\n",
      "legend('bottom', legend = 'Curva Normal', col = 'blue', lty=1, cex=1.5)"))
  }
}

fisher.calc <- function (x, na.rm = FALSE, ...) {
  if (!is.numeric(x)) {
    stop("argument 'x' is must be numeric")
  }
  if (na.rm)
    x <- x[!is.na(x)]
  nx <- length(x)

  if (nx < 3)
    sk <- NA
  else sk <- ((sqrt(nx * (nx - 1))/(nx - 2)) * (sum(x^3)/nx))/((sum(x^2)/nx)^(3/2))

  return(sk)
}

default.calc.normal <- function(data = "datos"){
  return(paste0(
    "calc <- lapply(var.numericas(datos), function(i) fisher.calc(i)[1]) \n",
    "calc <- as.data.frame(calc) \n",
    "calc <- rbind(calc, lapply(calc, function(i) ifelse(i > 0, 'Positiva', \n",
    "                                                ifelse(i < 0, 'Negativa', 'Sin Asimetría')))) \n",
    "calc <- t(calc)\ncolnames(calc) <- c('Cálculo de Fisher', 'Asimetría')\ncalc"))
}

#' Funciones Dispersión
#' @author Diego
#' @return functions
#' @export
#'
default.disp <- function(data = "datos", vars = NULL, color = "#FF0000AA"){
  if(length(vars) < 2) {
    return("NULL")
  } else if(length(vars) == 2) {
    return(paste0("ggplot(data = ", data, ", aes(x = ", vars[1], ", y = ", vars[2],
                  ", label = rownames(", data, "))) +\n", "geom_point(color = '",
                  color, "', size = 3) + geom_text(vjust = -0.7)"))
  } else{
    return(paste0("scatterplot3d(", data, "[, '", vars[1], "'], ", data, "[, '",
                  vars[2], "'], ", data, "[, '", vars[3], "'], pch = 16, color = '",
                  color, "')"))
  }
}

#' Funciones Distribuciones
#' @author Diego
#' @return functions
#' @export
#'
distribucion.numerico <- function(var, nombre.var, color){
  nf <- graphics::layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE),  height = c(3,1))
  par(mar=c(3.1, 3.1, 1.1, 2.1))
  hist(var, col = color, border=F, main =
         paste0('Distribución y atipicidad de la variable ', nombre.var), axes=F)
  axis(1, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025)
  axis(2, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025)
  boxplot(var, col = color, boxcol = color, boxlty = 1, boxlwd = 3,
          boxwex = 1.5, edcol = color, medlty = 1, medlwd = 8, axes=F,
          medcol = color, whiskcol = color, whisklty = 3, staplecol = color,
          staplelty = 1, staplelwd = 3, horizontal=TRUE, outline=TRUE,
          frame=F, whisklwd = 2.5, outpch = 20, outcex = 1.5, outcol = 'red')
}

distribucion.categorico <- function(var) {
  colores <- sapply(levels(var),
                    function(i) rgb(runif(1), runif(1), runif(1), 0.8))
  data <- data.frame(label = levels(var), value = summary(var))
  plot(ggplot(data, aes(label, value)) +
         geom_bar(stat = 'identity', fill = colores) +
         geom_text(aes(label = value, y = value), vjust = -0.5, size = 4) +
         theme_minimal() +
         labs(title = 'Distribución', y = 'Cantidad de casos', x = 'Categorias'))
}

def.code.num <- function(data = "datos", variable, color) {
  return(paste0("distribucion.numerico(", data, "[, ", variable, "], ",
                variable, ", color = ", color,")"))
}

def.code.cat <- function(data = "datos", variable) {
  return(paste0("distribucion.categorico(", data, "[, ", variable,"])"))
}

#' Funciones Correlaciones
#' @author Diego
#' @return functions
#' @export
#'
modelo.cor <- function(data = "datos") {
  return(paste0("correlacion <<- cor(var.numericas(", data, "))"))
}

correlaciones <- function(metodo = 'circle', tipo = "lower") {
  return(paste0(
    "corrplot(correlacion, method='", metodo,"', shade.col=NA, tl.col='black',\n",
    "         tl.srt=20, addCoef.col='black', order='AOE', type = '", tipo, "')"))
}

###################### PCA ####################################################
#' Modelo PCA
#' @author Diego
#' @return functions
#' @export
#'
def.pca.model <- function(data = "datos", scale.unit = T, npc = 5) {
  return(paste0("pca.modelo <- PCA(var.numericas(", data, "), scale.unit = ",
                scale.unit, ", ncp = ", npc, ", graph = FALSE)"))
}

#' PCA Individuos
#' @author Diego
#' @return functions
#' @export
#'
pca.individuos <- function(ind.cos = 0, color = '#696969', ejes = c(1, 2)) {
  return(paste0(
    "fviz_pca_ind(pca.modelo, pointsize = 2, pointshape = 16, axes = c(",
    paste(ejes, collapse = ","), "),\n",
    "     col.ind = '", color, "', select.ind = list(cos2 = ", ind.cos, "))"))
}

#' PCA Variables
#' @author Diego
#' @return functions
#' @export
#'
pca.variables <- function(var.cos = 0, color = 'steelblue', ejes = c(1, 2)) {
  return(paste0("fviz_pca_var(pca.modelo, col.var= '", color, "',\n",
                "     select.var = list(cos2 = ", var.cos, "),",
                "axes = c(", paste(ejes, collapse = ","), "))"))
}

#' PCA Sobreposición
#' @author Diego
#' @return functions
#' @export
#'
pca.sobreposicion <- function(ind.cos = 0, var.cos = 0, col.ind = '#696969',
                              col.var = 'steelblue', ejes = c(1, 2)) {
  return(paste0(
    "fviz_pca_biplot(pca.modelo, pointsize = 2, pointshape = 16, col.var = '",
    col.var, "', \n", "     col.ind = '", col.ind,
    "', select.ind = list(cos2 = ", ind.cos, "), ", "select.var = list(cos2 = ",
    var.cos, "), axes = c(", paste(ejes, collapse = ","), "))"))
}

#' PCA Varianza Explicada por cada eje
#' @author Diego
#' @return functions
#' @export
#'
code.pca.vee <- function() {
  return(paste0(
    "fviz_eig(pca.modelo, addlabels = TRUE, \n",
    "     ylab = 'Porcentaje de Varianzas Explicadas',\n",
    "     xlab = 'Dimensiones', main = 'Varianza Explicada por cada eje')"))
}

#' PCA Cosenos cuadrados de los individuos
#' @author Diego
#' @return functions
#' @export
#'
code.pca.cci <- function() {
  return(paste0(
    "fviz_cos2(pca.modelo, choice = 'ind', axes = 1:2, top = 20) +\n",
    "  labs(y = 'Cos2 - Calidad de la Representación',\n",
    "       title = 'Cosenos cuadrados de los individuos')"))
}

#' PCA Cosenos cuadrados de las variables
#' @author Diego
#' @return functions
#' @export
#'
code.pca.ccv <- function() {
  return(paste0(
    "fviz_cos2(pca.modelo, choice = 'var', axes = 1:2) +\n",
    "  labs(y = 'Cos2 - Calidad de la Representación',\n",
    "       title = 'Cosenos cuadrados de los variables')"))
}

#' PCA Correlación de las variables con las componentes principales
#' @author Diego
#' @return functions
#' @export
#'
code.pca.cvp <- function(metodo = "circle") {
  return(paste0(
    "corrplot(pca.modelo$var$cor, is.corr=FALSE, mar=c(0,0,1,0), shade.col=NA,\n",
    "         tl.col='black', addCoef.col='black', method='", metodo,"',\n",
    "         title = 'Correlación de las variables con las componentes principales')"))
}

#' PCA Contributions of variables to PC1
#' @author Diego
#' @return functions
#' @export
#'
code.pca.pc1 <- function() {
  return(paste0(
    "fviz_contrib(pca.modelo, choice = 'var', axes = 1, top = 20) +\n",
    "  labs(y = 'Contribuciones (%)',\n",
    "       title = 'Contribuciones de las variables para Dim-1')"))
}

#' PCA Contributions of variables to PC2
#' @author Diego
#' @return functions
#' @export
#'
code.pca.pc2 <- function() {
  return(paste0(
    "fviz_contrib(pca.modelo, choice = 'var', axes = 2, top = 20) +\n",
    "  labs(y = 'Contribuciones (%)',\n",
    "       title = 'Contribuciones de las variables para Dim-2')"))
}

###################### Clustering Jerarquico ##################################
#' Modelo
#' @author Diego
#' @return functions
#' @export
#'
def.model <- function(data = "datos", cant = "as.numeric(input$cant.cluster)",
                      dist.method = "euclidean", hc.method = "complete") {
  return(paste0(
    "modelo <- hclust(dist(var.numericas(", data, "), method = '",
    dist.method, "'), method = '", hc.method, "')\n",
    "clusters <- as.factor(cutree(modelo, k = ", cant, "))\n",
    "centros <- calc.centros(var.numericas(", data, "), clusters)\n",
    "hc.modelo <- list(modelo = modelo, clusters = clusters, centros = centros)"))
}

calc.centros <- function(data, clusteres) {
  if(is.null(clusteres)) return(NULL)
  real <- lapply(unique(clusteres), function(i)
    colMeans(data[clusteres == i, ]))
  real <- as.data.frame(do.call('rbind', real))
  porcentual <- apply(real, 2, function(i) scales::rescale(i, to = c(0, 100)))
  porcentual <- as.data.frame(porcentual)
  return(list(real = real, porcentual = porcentual))
}

#' Inercia
#' @author Diego
#' @return functions
#' @export
#'
centros.total <- function(DF){
  apply(DF, 2, mean)
}

calc.inercia <- function(total, individuo){
  return(inercia(0, 1, total, individuo))
}

inercia <- function(suma, i, total, individuo){
  if(i > length(total)){
    return(as.double(suma))
  }
  inercia(suma + ((total[i] - individuo[i])^2), i+1, total, individuo)
}

inercia.total <- function(DF){
  inercia.total2(0, 1, DF, centros.total(DF))
}
inercia.total2 <- function(suma, i, DF, c.total){
  if(i > length(DF[, 1])){
    return(as.double(suma))
  }
  inercia.total2(suma + (calc.inercia(c.total, DF[i, ])), i+1, DF, c.total)
}

BP <- function(DF, modelo, cant){
  BP2(0, 1, DF, centros.total(DF), cant, cutree(modelo, k = cant))
}
BP2 <- function(suma, i, DF, c.total, cant, clusters){
  if(i > cant){
    return(suma)
  }
  BP2(suma + (length(clusters[clusters == i]) *
                calc.inercia(c.total, centros.total(DF[clusters == i, ]))),
      i+1, DF, c.total, cant, clusters)
}

WP <- function(DF, modelo, cant){
  clusters <- cutree(modelo, k = cant)
  centros.cluster <- lapply(1:cant, function(i) centros.total(DF[clusters == i, ]))
  WP2(0, 1, DF, clusters, centros.cluster)
}
WP2 <- function(suma, i, DF, clusters, centros.cluster){
  if(i > nrow(DF)){
    return(as.double(suma))
  }
  WP2(suma + calc.inercia(DF[i, ], centros.cluster[[clusters[i]]]),
      i+1, DF, clusters, centros.cluster)
}

#' Dendograma
#' @author Diego
#' @return functions
#' @export
#'
diagrama <- function(cant = "as.numeric(input$cant.cluster)",
                     colores = "'steelblue'") {
  return(paste0(
    "dendograma <- dendro_data(hc.modelo$modelo, type='rectangle')\n",
    "order.labels <- data.frame(label=names(hc.modelo$clusters), clusters = hc.modelo$clusters)\n",
    "dendograma[['labels']] <- merge(dendograma[['labels']], order.labels, by='label')\n",
    "ggplot() + geom_segment(data=segment(dendograma), aes(x=x, y=y, xend=xend, yend=yend)) +\n",
    "  geom_text(data=label(dendograma), aes(x, y, label = label, hjust=1.1, color = clusters), \n",
    "            size = 4, angle = 90) +\n",
    "  scale_color_manual(values = c(", paste(colores, collapse = ","),
    ")) + expand_limits(y=-2)"))
}


###################### K-medias ###############################################
#' Modelo
#' @author Diego
#' @return functions
#' @export
#'
def.k.model <- function(data = "datos", cant = "as.numeric(input$cant.kmeans.cluster)",
                        iter.max = 200, nstart = 300, algorithm = "Hartigan-Wong") {
  return(paste0("k.modelo <- kmeans(var.numericas(", data, "), centers = ",
                cant, ",\n    ", "iter.max = ", iter.max,", nstart = ",
                nstart,", algorithm = '", algorithm ,"')"))
}

#' Jambu
#' @author Diego
#' @return functions
#' @export
#'
lead <- function(x){
  out <- c(x[-seq_len(1)], rep(NA, 1))
  return(out)
}

codo.jambu <- function(data. = NULL, k. = NA_integer_,
                       nstart. = 200, iter.max. = 5000, h. = 1.5){
  params <- list(k = k., data = list(data.))
  params <- purrr::cross(params)
  models <- purrr::map(params, ~future::future(
    kmeans(x = .$data, centers = .$k, iter.max = iter.max., nstart = nstart.)))
  models <- future::values(models)
  tot_withinss <- purrr::map_dbl(models, 'tot.withinss')
  model_index <- head(which(!tot_withinss/lead(tot_withinss) > h.), 1)
  if(length(model_index) == 0)
    model_index <- which.min(tot_withinss)
  best_model <- models[[model_index]]
  res.plot <- ggplot() + geom_point(aes(x = k., y = tot_withinss), size = 2) +
    geom_line(aes(x = k., y = tot_withinss), size = 1) +
    theme_minimal() + labs(x = 'k', y = 'Inercia Intra-Clase') +
    scale_x_continuous(breaks = seq(1, length(k.), 1)) +
    scale_y_continuous(labels = scales::comma)
  return(plot(res.plot))
}

calc.maxK <- function(data) {
  ifelse(nrow(datos) < 40, return(as.integer(nrow(datos)/2)), return(20))
}

def.code.jambu <- function(data = "datos", k = 20) {
  return(paste0("codo.jambu(data. = var.numericas(", data, "), k. = 2:", k, ")"))
}


###################### Clustering Jerarquico & K-medias #######################
#' Panel Inercia
#' @author Diego
#' @return functions
#' @export
#'
panel.inercia <- function(esHC = T, modelo, cant.clusters) {
  salida <- ""
  if (esHC) {
    intra.clase <- WP(var.numericas(datos), modelo, cant.clusters)
    inter.clase <- BP(var.numericas(datos), modelo, cant.clusters)
    total.clase <- intra.clase + inter.clase
  } else {
    intra.clase <- modelo$tot.withinss
    inter.clase <- modelo$betweenss
    total.clase <- modelo$totss
  }

  datos.numericos <-
    list(WP = list(id = "WP", Label = "Inercia Intra-Clases",
                   Value = format(intra.clase, scientific = FALSE),
                   color = "red"),
         BP = list(id = "BP", Label = "Inercia Inter-Clases",
                   Value = format(inter.clase, scientific = FALSE),
                   color = "green"),
         total = list(id = "total", Label = "Inercia Total",
                      Value = format(total.clase, scientific = FALSE),
                      color = "blue"))

  for (calculo in datos.numericos) {
    salida <- paste0(
      salida, "<div class='shiny-html-output col-sm-12 shiny-bound-output' id='",
      calculo$id, "'> <div class='small-box bg-", calculo$color,
      "'> <div class='inner'>", "<h3>", calculo$Value, "</h3> <p>",
      calculo$Label, "</p></div> <div class='icon-large'> <i class='",
      calculo$icon, "'></i></div></div></div>")
  }
  return(salida)
}

cluster.mapa <- function(esHC = T, colores = "'steelblue'") {
  code.clusters <-
    ifelse(esHC, "hc.modelo$clusters", "as.factor(k.modelo$cluster)")
  return(paste0(
    "fviz_pca_biplot(pca.modelo, col.ind = ", code.clusters, ",\n",
    "                palette = c(", paste(colores, collapse = ","), "),\n",
    "                col.var = 'steelblue', legend.title = 'Clúster')"))
}

#' Interpretación Horizontal
#' @author Diego
#' @return functions
#' @export
#'
centros.horizontal.todos <- function(centros){
  colnames(centros) <- sapply(c(1:ncol(centros)), function(i)
    paste0('Cluster ', i))
  var <- row.names(centros)
  centros <- cbind(centros, var)
  centros <- melt(centros, id.vars = 'var')
  ggplot(centros, aes(x=var, y=value)) +
    geom_bar(stat='identity', position='dodge', show.legend = F) +
    labs(x = '', y = '') + facet_wrap(~variable) + coord_flip() +
    theme(text = element_text(size = 20)) + aes(fill = variable)
}

cluster.horiz <- function(esHC = T, sel = "1", colores = "'steelblue'",
                          color = "red") {
  code.centros <-
    ifelse(esHC, "centros <- as.data.frame(t(hc.modelo$centros$real))",
           "centros <- as.data.frame(t(k.modelo$centers))")
  if(sel == "Todos") {
    return(paste0(
      code.centros, "\ncentros.horizontal.todos(centros) +\n",
      "  scale_fill_manual(values = c(", paste(colores, collapse = ","), "))"))
  } else {
    return(paste0(
      code.centros, "\nggplot(data = centros, aes(x = row.names(centros), ",
      "y = centros[, ", sel, "])) +\n",
      "  geom_bar(stat = 'identity', fill = ", color, ") +\n",
      "  scale_y_continuous(expand = c(.01,0,0,0)) +\n",
      "  labs(x = '', y = '') + coord_flip() + theme_minimal()"))
  }
}

#' Interpretación Vertical
#' @author Diego
#' @return functions
#' @export
#'
centros.vertical.todos <- function(centros){
  cluster <- c(1:nrow(centros))
  centros <- cbind(centros, cluster)
  centros <- melt(centros, id.vars = 'cluster')
  ggplot(centros, aes(x=variable, y=value, fill=factor(cluster))) +
    geom_bar(stat='identity', position='dodge') + labs(x = '', y = '')
}

cluster.vert <- function(esHC = T, sel = "1", colores = "'steelblue'") {
  code.centros <-
    ifelse(esHC, "centros <- hc.modelo$centros$real",
           "centros <- as.data.frame(k.modelo$centers)")
  if(sel == "Todos") {
    return(paste0(
      code.centros, "\ncentros.vertical.todos(centros) +\n",
      "  scale_fill_manual('Clúster', values = c(",
      paste(colores, collapse = ","), "))"))
  } else {
    return(paste0(
      code.centros, "\nggplot(data = centros, aes(x = row.names(centros), ",
      "y = centros[, '", sel, "'], fill = row.names(centros))) +\n",
      "  geom_bar(stat = 'identity') + labs(x = '', y = '') +\n",
      "  scale_fill_manual('Clúster', values = c(",
      paste(colores, collapse = ","), "))"))
  }
}

#' Interpretación Radar
#' @author Diego
#' @return functions
#' @export
#'
coord_radar <- function (theta = 'x', start = 0, direction = 1) {
  theta <- match.arg(theta, c('x', 'y'))
  r <- if (theta == 'x') 'y' else 'x'
  ggproto('CordRadar', CoordPolar, theta = theta, r = r, start = start,
          direction = sign(direction), is_linear = function(coord) TRUE)
}

centros.radar <- function(centros){
  res <- melt(t(centros), varnames = c('variables', 'clusteres'))
  res <- res[order(res$variables, decreasing = F), ]
  res$clusteres <- as.character(res$clusteres)
  ggplot(res, aes(x = variables, y = value)) +
    geom_polygon(aes(group = clusteres, color = clusteres, fill = clusteres),
                 alpha=0.3, size = 1, show.legend = FALSE) +
    geom_point(aes(group = clusteres, color = clusteres), size = 3) +
    theme(panel.background = element_rect(fill = 'transparent'),
          plot.background = element_rect(fill = 'transparent'),
          panel.grid.major = element_line(size = 0.5, linetype = 'solid',
                                          colour = '#dddddd'),
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_blank(), axis.ticks = element_blank()) +
    scale_y_continuous(limits=c(-10, 100), breaks=c(0, 25, 50, 75, 100)) +
    ggtitle('Comparación de Clustéres') + xlab('') + ylab('') +
    geom_text(aes(x = 0.5, y = 0, label = '0%'), size = 3.5,
              colour = '#dddddd', family = 'Arial') +
    geom_text(aes(x = 0.5, y = 25, label = '25%'), size = 3.5,
              colour = '#dddddd', family = 'Arial') +
    geom_text(aes(x = 0.5, y = 50, label = '50%'), size = 3.5,
              colour = '#dddddd', family = 'Arial') +
    geom_text(aes(x = 0.5, y = 75, label = '75%'), size = 3.5,
              colour = '#dddddd', family = 'Arial') +
    geom_text(aes(x = 0.5, y = 100, label = '100%'), size = 3.5,
              colour = '#dddddd', family = 'Arial') +
    coord_radar()
}

cluster.radar <- function(esHC = T, colores = "'steelblue'"){
  code.centros <-
    ifelse(esHC, "centros <- hc.modelo$centros$porcentual",
           paste0("centros <- as.data.frame(apply(k.modelo$centers, 2, function(i)\n",
                  "  scales::rescale(i, to = c(0, 100))))"))
  return(paste0(
    code.centros, "\ncentros.radar(centros) + \n",
    "scale_color_manual('Clústeres', values = c(", paste(colores, collapse = ","),
    ")) +\nscale_fill_manual('Clústeres', values = c(",
    paste(colores, collapse = ","),"))"))
}

#' Interpretación Categóricos
#' @author Diego
#' @return functions
#' @export
#'
cluster.cat <- function(esHC = T, var, colores = "'steelblue'") {
  code.clusters <-
    ifelse(esHC, "hc.modelo$clusters", "as.factor(k.modelo$cluster)")
  return(paste0(
    "NDatos <- cbind(datos, Cluster = ", code.clusters, ")\n",
    "plot(ggplot(NDatos, aes(x = ", var, ")) + geom_bar(aes(fill = Cluster)) +",
    "\n  scale_fill_manual('Cluster', values = c(", paste(colores, collapse = ","),
    ")) +\n  facet_wrap(~Cluster, labeller = label_both) +\n  ",
    "theme(text = element_text(size = 15)) +\n  labs(x = '', y = '') + guides(fill = F))"))
}


###################### Reporte ################################################
#' Reporte
#' @author Diego
#' @return functions
#' @export
#'
def.reporte <- function(titulo = "Sin Titulo", nombre = "PROMiDAT", entradas) {
  codigo.usuario <- ""
  for (ejercicio in names(env.report$codigo.reporte)) {
    codigo.usuario <- paste0(codigo.usuario, "\n\n## DATOS - ", ejercicio, "\n\n")
    codigo.usuario <- paste0(codigo.usuario, "```{r echo=FALSE}\n",
                             "datos.originales <<- datos.reporte[['", ejercicio, "']]\n",
                             "datos <<- datos.originales\n",
                             "```\n", env.report$codigo.reporte[[ejercicio]])
  }
  return(paste0("---\n",
                "title: '", titulo, "'\n",
                "author: '", nombre, "'\n",
                "date: ", Sys.Date(), "\n",
                "output:\n",
                "  word_document:\n",
                "    df_print: paged\n",
                "---\n\n",
                "```{r setup, include=FALSE}\n",
                "knitr::opts_chunk$set(echo = FALSE,  fig.height = 10, fig.width = 15)\n",
                "```\n\n",
                "```{r message=FALSE, warning=FALSE}\n",
                "library(promises)\nlibrary(ggplot2)\nlibrary(FactoMineR)\n",
                "library(FactoMineR)\nlibrary(factoextra)\nlibrary(reshape)\n",
                "library(corrplot)\nlibrary(dendextend)\nlibrary(scatterplot3d)\n",
                "library(stringr)\nlibrary(ggdendro)\n",
                "```\n\n",
                "```{r}\n", extract.code("var.numericas"),
                "\n\n", extract.code("var.categoricas"),
                "\n\n", extract.code("datos.disyuntivos"),
                "\n\n", extract.code("distribucion.numerico"),
                "\n\n", extract.code("distribucion.categorico"),
                "\n\n", extract.code("codo.jambu"),
                "\n\n", extract.code("calc.centros"),
                "\n\n", extract.code("centros.horizontal.todos"),
                "\n\n", extract.code("centros.vertical.todos"),
                "\n\n", extract.code("centros.radar"),
                "\n```\n\n", codigo.usuario, "\n\n"))
}

gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}

datos <<- NULL
datos.originales <<- NULL
centros <<- NULL
hc.modelo <<- NULL
pca.modelo <<- NULL
k.modelo <<- NULL
correlacion <<- NULL
def.colores <<- gg_color_hue(10)

colnames.empty <- function(res){
  res <- colnames(res)
  if(is.null(res))
    return("")
  return(res)
}

var.categoricas <- function(data){
  if(is.null(data)) return(NULL)
  res <- base::subset(data, select = !sapply(data, class) %in% c('numeric', 'integer'))
  return(res)
}

datos.disyuntivos <- function(data, vars){
  if(is.null(data)) return(NULL)
  cualitativas <- base::subset(data, select = colnames(data) %in% c(vars))
  data <- data[, !colnames(data) %in% vars]
  for (variable in colnames(cualitativas)) {
    for (categoria in unique(cualitativas[, variable])) {
      nueva.var <- as.numeric(cualitativas[, variable] == categoria)
      data <- cbind(data, nueva.var)
      colnames(data)[length(colnames(data))] <- paste0(variable, '.', categoria)
    }
  }
  return(data)
}

code.carga <- function(nombre.filas = T, ruta = NULL, separador = ";", sep.decimal = ",", encabezado = T){
  if(nombre.filas){
    return(paste0("datos.originales <<- read.table('", ruta, "', header=", encabezado, ", sep='",
                  separador, "', dec = '", sep.decimal, "', row.names = 1) \ndatos <<- datos.originales"))
  } else {
    return(paste0("datos.originales <<- read.table('", ruta, "', header=", encabezado, ", sep='",
                  separador, "', dec = '", sep.decimal, "') \ndatos <<- datos.originales"))
  }
}

code.trans <- function(variable, nuevo.tipo){
  if(nuevo.tipo == "categorico"){
      return(paste0("datos[, '", variable, "'] <<- as.factor(datos[, '", variable, "'])"))
  } else if(nuevo.tipo == "numerico") {
    return(paste0("datos[, '", variable, "'] <<- as.numeric(datos[, '", variable, "'])"))
  } else {
    es.factor <- ifelse(class(datos.originales[, variable]) %in% c('numeric', 'integer'),
           paste0("datos[, '", variable, "'] <<- as.factor(datos[, '", variable, "']) \n"), "")
    return(paste0(es.factor, "datos <<- datos.disyuntivos(datos, '", variable,"')"))
  }
}

code.desactivar <- function(variables){
  return(paste0("datos <<- subset(datos, select = -c(",
                paste(variables, collapse = ","), "))"))
}

resumen.numerico <- function(data, variable){
  salida <- ""
  datos.numericos <- list(Q1 = list(id = "q1", Label = "Primer Cuartil",
                                    Value = format(quantile(data[, variable], .25), scientific = FALSE), color = "green"),
                          Mediana = list(id = "mediana", Label = "Mediana",
                                         Value = format(median(data[, variable]), scientific = FALSE), color = "orange"),
                          Q3 = list(id = "q3", Label = "Tercer Cuartil",
                                    Value = format(quantile(data[, variable], .75), scientific = FALSE), color = "maroon"),
                          Minimo = list(id = "minimo", Label = "Mínimo",
                                        Value = format(min(data[, variable]), scientific = FALSE), color = "red"),
                          Promedio = list(id = "promedio", Label = "Promedio",
                                          Value = format(mean(data[, variable]), scientific = FALSE), color = "blue"),
                          Maximo = list(id = "maximo", Label = "Máximo",
                                        Value = format(max(data[, variable]), scientific = FALSE), color = "purple"),
                          DS <- list(id = "ds", Label = "Desviación Estandar",
                                     Value = format(max(data[, variable]), scientific = FALSE), color = "yellow"))

  for (calculo in datos.numericos) {
    salida <- paste0(salida, "<div class='shiny-html-output col-sm-6 shiny-bound-output' id='", calculo$id,
                     "'> <div class='small-box bg-", calculo$color,"'> <div class='inner'>",
                     "<h3>", calculo$Value, "</h3> <p>", calculo$Label, "</p></div> <div class='icon-large'> <i class='",
                     calculo$icon, "'></i></div></div></div>")
  }
  return(salida)
}

resumen.categorico <- function(data, variable){
  salida <- ""
  color <- c("red","yellow","aqua","navy","teal","olive","purple","maroon","black","blue","lime","orange","light-blue","green","fuchsia")
  datos.categoricos <- summary(data[, variable])
  for (i in 1:length(datos.categoricos)) {
    salida <- paste0(salida, "<div class='shiny-html-output col-sm-6 shiny-bound-output' id='", variable, i,
                     "'> <div class='small-box bg-", sample(color, 1), "'> <div class='inner'>",
                     "<h3>", datos.categoricos[i], "</h3> <p>", levels(data[, variable])[i],
                     "</p></div> <div class='icon-large'> <i class=''></i></div></div></div>")
  }
  return(salida)
}

resumen.kmeans <- function(kmedias){
  salida <- ""
  datos.numericos <- list(WP = list(id = "WP", Label = "Inercia Intra-Clases",
                                    Value = format(kmedias$tot.withinss, scientific = FALSE), color = "red"),
                          BP = list(id = "BP", Label = "Inercia Inter-Clases",
                                    Value = format(kmedias$betweenss, scientific = FALSE), color = "green"),
                          total = list(id = "total", Label = "Inercia Total",
                                       Value = format(kmedias$totss, scientific = FALSE), color = "blue"))

  for (calculo in datos.numericos) {
    salida <- paste0(salida, "<div class='shiny-html-output col-sm-4 shiny-bound-output' id='", calculo$id,
                     "'> <div class='small-box bg-", calculo$color,"'> <div class='inner'>",
                     "<h3>", calculo$Value, "</h3> <p>", calculo$Label, "</p></div> <div class='icon-large'> <i class='",
                     calculo$icon, "'></i></div></div></div>")
  }
  return(salida)
}

default.normal <- function(data = "datos", vars = NULL, color = "#00FF22AA"){
  if(is.null(vars)){
    return(NULL)
  } else {
    return(paste0("hist(", data, "[, '", vars, "'], col = '", color,
"', border=F, main = paste0('Test de normalidad de la variable ','", vars,"'), axes=F, freq = F)
axis(1, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025)
axis(2, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025)
curve(dnorm(x, mean = mean(", data, "[, '", vars, "']), sd = sd(", data, "[, '", vars, "'])), add=T, col='blue', lwd=2)
legend('bottom', legend = 'Curva Normal', col = 'blue', lty=1, cex=1.5)"))
  }
}

default.disp <- function(data = "datos", vars = NULL, color = "#FF0000AA"){
  if(length(vars) < 2) {
    return(NULL)
  } else if(length(vars) == 2) {
return(paste0("ggplot(data = ", data, ", aes(x = ", vars[1], ", y = ", vars[2], ", label = rownames(", data, "))) +
       geom_point(color = '", color, "', size = 3) + geom_text(vjust = -0.7)"))
  } else{
return(paste0("scatterplot3d(", data, "[, '", vars[1], "'], ", data, "[, '",
                  vars[2], "'], ", data, "[, '", vars[3], "'], pch = 16, color = '", color, "')"))
  }
}

def.pca.model <- function(data = "datos", scale.unit = T, npc = 5){
  return(paste0("pca.modelo <<- PCA(var.numericas(", data, "), scale.unit = ", scale.unit, ", ncp = ", npc, ", graph = FALSE)"))
}

def.model <- function(data = "datos", cant = "as.numeric(input$cant.cluster)", dist.method = "euclidean", hc.method = "complete"){
  return(paste0("hc.modelo <<- hclust(dist(var.numericas(", data, "), method = '", dist.method, "'), method = '", hc.method, "')
centros <<- calc.centros(var.numericas(", data, "), hc.modelo, ", cant, ")"))
}

def.k.model <- function(data = "datos", cant = "as.numeric(input$cant.kmeans.cluster)", iter.max = 200, nstart = 300){
  return(paste0("k.modelo <<- kmeans(var.numericas(", data, "), centers = ", cant,", iter.max = ", iter.max,", nstart = ", nstart,")"))
}

pca.individuos <- function(ind.cos = 0, color = '#696969'){
  return(paste0("fviz_pca_ind(pca.modelo, pointsize = 2, pointshape = 16,
             col.ind = '", color, "', select.ind = list(cos2 = ", ind.cos, "))"))
}

pca.variables <- function(var.cos = 0, color = 'steelblue'){
  return(paste0("fviz_pca_var(pca.modelo, col.var= '", color, "', select.var = list(cos2 = ", var.cos, "))"))
}

pca.sobreposicion <- function(ind.cos = 0, var.cos = 0, col.ind = '#696969', col.var = 'steelblue'){
  return(paste0("fviz_pca_biplot(pca.modelo, pointsize = 2, pointshape = 16, col.var = '", col.var, "',
                 col.ind = '", col.ind, "', select.ind = list(cos2 = ", ind.cos, ") , select.var = list(cos2 = ", var.cos, "))"))
}

code.pca.vee <- function(){
  # Varianza Explicada por cada eje
  return("fviz_eig(pca.modelo, addlabels = TRUE, ylab = 'Porcentaje de Varianzas Explicadas',
           xlab = 'Dimensiones', main = 'Varianza Explicada por cada eje')")
}

code.pca.cci <- function(){
  # Cosenos cuadrados de los individuos
  return("fviz_cos2(pca.modelo, choice = 'ind', axes = 1:2) +
         labs(y = 'Cos2 - Calidad de la Representación',
         title = 'Cosenos cuadrados de los individuos')")
}

code.pca.ccv <- function(){
  # Cosenos cuadrados de las variables
  return("fviz_cos2(pca.modelo, choice = 'var', axes = 1:2) +
         labs(y = 'Cos2 - Calidad de la Representación',
         title = 'Cosenos cuadrados de los variables')")
}

code.pca.cvp <- function(){
  # Correlación de las variables con las componentes principales
  return("corrplot(pca.modelo$var$cos2, is.corr=FALSE, mar=c(0,0,1,0),
         title = 'Correlación de las variables con las componentes principales')")
}

code.pca.pc1 <- function(){
  # Contributions of variables to PC1
  return("fviz_contrib(pca.modelo, choice = 'var', axes = 1, top = 10) +
         labs(y = 'Contribuciones (%)',
              title = 'Contribuciones de las variables para Dim-1')")
}

code.pca.pc2 <- function(){
  # Contributions of variables to PC2
  return("fviz_contrib(pca.modelo, choice = 'var', axes = 2, top = 10) +
         labs(y = 'Contribuciones (%)',
              title = 'Contribuciones de las variables para Dim-2')")
}

modelo.cor <- function(data = "datos"){
  return(paste0("correlacion <<- cor(var.numericas(", data, "))"))
}

correlaciones <- function(metodo = 'circle', tipo = "lower"){
  return(paste0("corrplot(correlacion, method='", metodo,"', shade.col=NA, tl.col='black',
         tl.srt=20, addCoef.col='black', order='AOE', type = '", tipo, "')"))
}

def.code.num <- function(data = "datos", variable = "input$sel.distribucion", color = 'input$col.dist'){
  return(paste0("distribucion.numerico(", data, "[, ", variable, "], ", variable, ", color = ", color,")"))
}

def.code.cat <- function(data = "datos", variable = "input$sel.distribucion", color = 'input$col.dist'){
  return(paste0("distribucion.categorico(", data, "[, ", variable,"], color = ", color, ")"))
}

default.func.num <- function(){
  return(paste0("distribucion.numerico <<- function(var, nombre.var, color){
  nf <- graphics::layout(mat = matrix(c(1, 2), 2, 1, byrow=TRUE),  height = c(3,1))
  par(mar=c(3.1, 3.1, 1.1, 2.1))
  hist(var, col = color, border=F, main = paste0('Distribución y atipicidad de la variable ', nombre.var), axes=F)
  axis(1, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025)
  axis(2, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025)
  boxplot(var, col = color, boxcol = color, boxlty = 1, boxlwd = 3, boxwex = 1.5,
          edcol = color, medlty = 1, medlwd = 8, medcol = color, whiskcol = color, whisklty = 3,
          staplecol = color, staplelty = 1, staplelwd = 3, horizontal=TRUE, outline=TRUE,
          frame=F, whisklwd = 2.5, outpch = 20, outcex = 1.5, outcol = 'red', axes=F)
}"))
}

default.func.cat <- function(){
  return(paste0("distribucion.categorico <<- function(var, color = 'input$col.dist'){
  colores <- sapply(c(1:length(levels(var))), function(i) rgb(sample(0:255, 1), sample(0:255, 1), sample(0:255, 1), 180, maxColorValue = 255))
  data <- data.frame(label = levels(var), value = summary(var))
  ggplot(data, aes(label, value)) +
  geom_bar(stat = 'identity', fill = colores) +
      geom_text(aes(label = value, y = value), vjust = -0.5, size = 4) +
      theme_minimal() +
      labs(title = 'Distribución', y = 'Cantidad de casos', x = 'Categorias')
}"))
}

diagrama <- function(cant = "as.numeric(input$cant.cluster)", colores = "'steelblue'"){
  return(paste0("modelo <- color_branches(hc.modelo, k = ", cant, ", col = c(", paste(colores, collapse = ","), "))\n",
                "modelo <- color_labels(modelo, k = ", cant, ", col = c(", paste(colores, collapse = ","), "))\n",
                "plot(modelo)"))
}

def.code.jambu <- function(data = "datos"){
  return(paste0("codo.jambu(data. = var.numericas(", data, "), k. = 2:10)"))
}

def.func.jambu <- function(){
  return(paste0("lead <- function(x){
  out <- c(x[-seq_len(1)], rep(NA, 1))
  return(out)
}
codo.jambu <<- function(data. = NULL, k. = NA_integer_, nstart. = 200, iter.max. = 5000, h. = 1.5){
  params <- list(k = k., data = list(data.))
  params <- purrr::cross(params)
  models <- purrr::map(params, ~future::future(kmeans(x = .$data, centers = .$k, iter.max = iter.max., nstart = nstart.)))
  models <- future::values(models)
  tot_withinss <- purrr::map_dbl(models, 'tot.withinss')
  model_index <- head(which(!tot_withinss/lead(tot_withinss) > h.), 1)
  if(length(model_index) == 0)
     model_index <- which.min(tot_withinss)

  best_model <- models[[model_index]]
  res.plot <- ggplot() + geom_point(aes(x = k., y = tot_withinss), size = 2) +
  geom_line(aes(x = k., y = tot_withinss), size = 1) +
  geom_vline(xintercept = k.[model_index], linetype='dashed', color = 'blue', size=0.8) +
  theme_minimal() + labs(x = 'k', y = 'Inercia Intra-Clase') +
  scale_x_continuous(breaks = seq(1, length(k.), 1)) + scale_y_continuous(labels = scales::comma)
  return(res.plot)
}"))
}

cluster.mapa <- function(cant = "as.numeric(input$cant.cluster)", colores = "'steelblue'"){
  return(paste0("res.hcpc <- HCPC(pca.modelo, nb.clust = -1, consol = TRUE, min = ", cant, ", max = ", cant, ", graph = FALSE)
fviz_pca_biplot(pca.modelo, col.ind = res.hcpc$data.clust$clust, palette = c(", paste(colores, collapse = ","), "), addEllipses = T,
                label = 'var', col.var = 'steelblue', repel = TRUE, legend.title = 'Clúster')"))
}

cluster.kmapa <- function(colores = "'steelblue'"){
  return(paste0("fviz_pca_biplot(pca.modelo, col.ind = as.factor(k.modelo$cluster), palette = c(", paste(colores, collapse = ","), "),
                 addEllipses = T, label = 'var', col.var = 'steelblue', repel = TRUE, legend.title = 'Clúster')"))
}

default.centros <- function(){
  return(paste0("calc.centros <<- function(data, modelo = NULL, cant.cluster, metodo = 'complete') {
  if(is.null(modelo)) return(NULL)
  clusters <- cutree(modelo, k=cant.cluster)
  real <- lapply(unique(clusters), function(i) colMeans(data[clusters == i, ]))
  real <- as.data.frame(do.call('rbind', real))

  porcentual <- apply(real, 2, function(i) scales::rescale(i, to = c(0, 100)))
  porcentual <- as.data.frame(porcentual)
  return(list(real = real, porcentual = porcentual))
}"))
}

default.horiz <- function(){
  return(paste0("centros.horizontal.todos <<- function(centros){
  colnames(centros) <- sapply(c(1:ncol(centros)), function(i) paste0('Cluster ', i))
  var <- row.names(centros)
  centros <- cbind(centros, var)
  centros <- melt(centros, id.vars = 'var')
  ggplot(centros, aes(x=var, y=value)) + geom_bar(stat='identity', position='dodge', show.legend = F) +
     labs(x = '', y = '') + facet_wrap(~variable) + coord_flip() +
     theme(text = element_text(size = 20)) + aes(fill = variable)
}"))
}

default.vert <- function(){
  return(paste0("centros.vertical.todos <<- function(centros){
  cluster <- c(1:nrow(centros))
  centros <- cbind(centros, cluster)
  centros <- melt(centros, id.vars = 'cluster')
  ggplot(centros, aes(x=variable, y=value, fill=factor(cluster))) +
    geom_bar(stat='identity', position='dodge') + labs(x = '', y = '')
}"))
}

cluster.horiz <- function(sel = "1", colores = "'steelblue'", color = "red"){
  return(paste0("t.centros <- as.data.frame(t(centros$real))
if(", sel, " == 'Todos'){
    centros.horizontal.todos(t.centros) + scale_fill_manual(values = c(", paste(colores, collapse = ","), "))
} else {
    ggplot(data = t.centros, aes(x = row.names(t.centros), y = t.centros[, as.numeric(", sel, ")])) +
       geom_bar(stat = 'identity', fill = ", color, ") +
       scale_y_continuous(expand = c(.01,0,0,0)) + labs(x = '', y = '') +
       coord_flip() + theme_minimal()
}"))
}

cluster.khoriz <- function(sel = "1", colores = "'steelblue'", color = "red"){
  return(paste0("centros <- as.data.frame(t(k.modelo$centers))
if(", sel, " == 'Todos'){
   centros.horizontal.todos(centros) + scale_fill_manual(values = c(", paste(colores, collapse = ","), "))
} else{
   ggplot(data = centros, aes(x = row.names(centros), y = centros[, as.numeric(", sel, ")])) +
       geom_bar(stat = 'identity', fill = ", color, ") + scale_y_continuous(expand = c(.01,0,0,0)) + labs(x = '', y = '') +
       coord_flip() + theme_minimal()
}"))
}

cluster.vert <- function(sel = "input$sel.verticales", colores = "'steelblue'"){
  return(paste0("real <- centros$real
if(", sel, " == 'Todos'){
  centros.vertical.todos(real) + scale_fill_manual('Clúster', values = c(", paste(colores, collapse = ","), "))
} else{
  ggplot(data = real, aes(x = row.names(real), y = real[, ", sel, "], fill = row.names(real))) +
         geom_bar(stat = 'identity') + labs(x = '', y = '') +
         scale_fill_manual('Clúster', values = c(", paste(colores, collapse = ","), "))
}"))
}

cluster.kvert <- function(sel = "input$sel.kmeans.verticales", colores = "'steelblue'"){
  return(paste0("centros <- as.data.frame(k.modelo$centers)
if(", sel, " == 'Todos'){
    centros.vertical.todos(centros) + scale_fill_manual('Clúster', values = c(", paste(colores, collapse = ","), "))
} else{
    ggplot(data = centros, aes(x = row.names(centros), y = centros[, ", sel, "], fill = row.names(centros))) +
         geom_bar(stat = 'identity') + labs(x = '', y = '') +
         scale_fill_manual('Clúster', values = c(", paste(colores, collapse = ","), "))
}"))
}

cluster.cat <- function(var = "input$sel.kcat.var", cant = "input$cant.cluster"){
  return(paste0("hc.clusters <- cutree(hc.modelo, k=", cant, ")
NDatos <- cbind(datos, grupo = hc.clusters)
                ggplot(NDatos, aes(", var, ")) + geom_bar(aes(fill = ", var, ")) +
                facet_wrap(~grupo) + theme(text = element_text(size = 10), axis.text.x = element_blank()) +
                scale_fill_discrete(name='Variable') + labs(x = '', y = '')"))
}

cluster.kcat <- function(var = "input$sel.kcat.var"){
  return(paste0("NDatos <- cbind(datos, grupo = k.modelo$cluster)
ggplot(NDatos, aes(", var, ")) + geom_bar(aes(fill = ", var, ")) +
  facet_wrap(~grupo) + theme(text = element_text(size = 10), axis.text.x = element_blank()) +
  scale_fill_discrete(name='Variable') + labs(x = '', y = '')"))
}

cluster.radar <- function(){
  return(paste0("coord_radar <<- function (theta = 'x', start = 0, direction = 1) {
  theta <- match.arg(theta, c('x', 'y'))
  r <- if (theta == 'x') 'y' else 'x'
  ggproto('CordRadar', CoordPolar, theta = theta, r = r, start = start, direction = sign(direction), is_linear = function(coord) TRUE)
}

centros.radar <<- function(centros){
  res <- melt(t(centros), varnames = c('variables', 'clusteres'))
  res <- res[order(res$variables, decreasing = F), ]
  res$clusteres <- as.character(res$clusteres)
  ggplot(res, aes(x = variables, y = value)) +
    geom_polygon(aes(group = clusteres, color = clusteres, fill = clusteres), alpha=0.3, size = 1, show.legend = FALSE) +
    geom_point(aes(group = clusteres, color = clusteres), size = 3) +
    theme( panel.background = element_rect(fill = 'transparent'),
           plot.background = element_rect(fill = 'transparent'),
           panel.grid.major = element_line(size = 0.5, linetype = 'solid', colour = '#dddddd'),
          axis.text.x = element_text(size = rel(1.2)),
          axis.text.y = element_blank(),
          axis.ticks = element_blank()) +
    scale_y_continuous(limits=c(-10, 100), breaks=c(0, 25, 50, 75, 100)) +
    ggtitle('Comparación de Clustéres') + xlab('') + ylab('') +
    geom_text(aes(x = 0.5, y = 0, label = '0%'), size = 3.5, colour = '#dddddd', family = 'Arial') +
    geom_text(aes(x = 0.5, y = 25, label = '25%'), size = 3.5, colour = '#dddddd', family = 'Arial') +
    geom_text(aes(x = 0.5, y = 50, label = '50%'), size = 3.5, colour = '#dddddd', family = 'Arial') +
    geom_text(aes(x = 0.5, y = 75, label = '75%'), size = 3.5, colour = '#dddddd', family = 'Arial') +
    geom_text(aes(x = 0.5, y = 100, label = '100%'), size = 3.5, colour = '#dddddd', family = 'Arial') +
    coord_radar()
}"))
}

def.radar <- function(colores = "'steelblue'"){
  return(paste0("centros.radar(centros$porcentual) + \n",
                "  scale_color_manual('Clústeres', values = c(", paste(colores, collapse = ","), ")) + \n",
                "  scale_fill_manual('Clústeres', values = c(", paste(colores, collapse = ","), "))"))
}

def.kradar <- function(colores = "'steelblue'"){
  return(paste0("centros <- as.data.frame(apply(k.modelo$centers, 2, function(i) scales::rescale(i, to = c(0, 100))))\n",
                "centros.radar(centros) + \n",
                "  scale_color_manual('Clústeres', values = c(", paste(colores, collapse = ","), ")) + \n",
                "  scale_fill_manual('Clústeres', values = c(", paste(colores, collapse = ","), "))"))
}

def.reporte <- function(entradas){
  return(paste0("---
title: 'Untitled'
author: 'PROMIDAT'
date: ", Sys.Date(), "
output:
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = FALSE,  fig.height = 10, fig.width = 15)
```

```{r message=FALSE, warning=FALSE}
library(promises)
library(ggplot2)
library(FactoMineR)
library(factoextra)
library(reshape)
library(corrplot)
library(dendextend)
library(scatterplot3d)
library(stringr)
```

```{r}
var.numericas <- function(data){
  if(is.null(data)) return(NULL)
  res <- base::subset(data, select = sapply(data, class) %in% c('numeric', 'integer'))
  return(res)
}

var.categoricas <- function(data){
  if(is.null(data)) return(NULL)
  res <- base::subset(data, select = !sapply(data, class) %in% c('numeric', 'integer'))
  return(res)
}

datos.disyuntivos <- function(data, vars){
  if(is.null(data)) return(NULL)
     cualitativas <- base::subset(data, select = colnames(data) %in% c(vars))
     data <- data[, !colnames(data) %in% vars]
     for (variable in colnames(cualitativas)) {
       for (categoria in unique(cualitativas[, variable])) {
          nueva.var <- as.numeric(cualitativas[, variable] == categoria)
          data <- cbind(data, nueva.var)
          colnames(data)[length(colnames(data))] <- paste0(variable, '.', categoria)
       }
     }
  return(data)
}

", default.func.num(), "

", default.func.cat(), "

", def.func.jambu(), "

", default.centros(), "

", default.horiz(), "

", default.vert(), "

", cluster.radar(), "
```

# Carga de Datos
```{r}
head(datos)
```

# Estadísticas Básicas

## Resumen Numérico
```{r}
summary(datos)
```
##### Interpretación

## Test de Normalidad
```{r}
```

## Dispersión
```{r}
```

## Distribuciones
```{r}
```

## Correlación
```{r}
", entradas$fieldCodeCor, "
```
##### Interpretación

# ACP

## Individuos
```{r}
", entradas$fieldCodeInd, "
```
##### Interpretación

## Variables
```{r}
", entradas$fieldCodeVar, "
```
##### Interpretación

## Sobreposición
```{r}
", entradas$fieldCodeBi, "
```
##### Interpretación

## Inercia y Valores Propios

### Varianza Explicada por cada eje
```{r}
", code.pca.vee(), "
```
##### Interpretación

### Cosenos cuadrados de los individuos
```{r}
", code.pca.cci(), "
```
##### Interpretación

### Cosenos cuadrados de los variables
```{r}
", code.pca.ccv(), "
```
##### Interpretación

### Correlación variables-componentes
```{r}
", code.pca.cvp(), "
```
##### Interpretación

### Contribución de las Variables Dim-1
```{r}
", code.pca.pc1(), "
```
##### Interpretación

### Contribución de las Variables Dim-2
```{r}
", code.pca.pc2(), "
```

##### Interpretación

# Cluster Jerárquico
```{r}
", entradas$fieldCodeModelo, "
```

##### Interpretación

## Diagrama
```{r}
", entradas$fieldCodeDiag, "
```
##### Interpretación

## Mapa
```{r}
", entradas$fieldCodeMapa, "
```

##### Interpretación

## Interpretación Horizontal
```{r}
", entradas$fieldCodeHoriz, "
```

##### Interpretación

## Interpretación Vertical
```{r}
", entradas$fieldCodeVert, "
```

##### Interpretación

## Gráfico Radar
```{r}
", entradas$fieldCodeRadar, "
```

##### Interpretación

## Interpretación Categórico
```{r}
", entradas$fieldCodeBarras, "
```
##### Interpretación

# K-Medias

## Inercia

## Codo de Jambu
```{r}
", entradas$fieldCodeJambu, "
```

##### Interpretación

## Mapa
```{r}
", entradas$fieldCodeKmapa, "
```

##### Interpretación

## Interpretación Horizontal
```{r}
", entradas$fieldCodeKhoriz, "
```

##### Interpretación

## Interpretación Vertical
```{r}
", entradas$fieldCodeKvert, "
```

##### Interpretación

## Gráfico Radar
```{r}
", entradas$fieldCodeKradar, "
```

##### Interpretación

## Interpretación Categórico
```{r}
", entradas$fieldCodeKbarras, "
```

##### Interpretación

"))
}

cod.resum <- function(data = "datos") {return(paste0("summary(", data, ")"))}
cod.disp <- default.disp()
cod.pca <- list("variables" = pca.variables(), "individuos" = pca.individuos(), "sobreposicion" = pca.sobreposicion())

cod.cor <- correlaciones()

cod.dya.cat <- def.code.cat()
cod.dya.num <- def.code.num()
func.dya.num <- default.func.num()
func.dya.cat <- default.func.cat()

func.centros <- default.centros()
func.horiz <- default.horiz()
func.vert <- default.vert()
code.diagrama <- diagrama()
code.mapa <- cluster.mapa()
code.horiz <- cluster.horiz()
code.vert <- cluster.vert()
func.radar <- cluster.radar()
code.radar <- def.radar()
code.cat <- cluster.cat()

#code.jambu <- def.func.jambu()
code.kmapa <- cluster.kmapa()
code.khoriz <- cluster.khoriz()
code.kvert <- cluster.kvert()
func.khoriz <- default.horiz()
func.kvert <- default.vert()
func.kradar <- cluster.radar()
code.kradar <- def.kradar()
code.kcat <- cluster.kcat()


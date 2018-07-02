datos.originales <<- NULL
datos.modificados <<- NULL
centros <<- NULL

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

code.carga <- function(nombre.columnas = T, ruta = NULL, separador = ";", sep.decimal = ",", encabezado = "T"){
  if(nombre.columnas){
    return(paste0("datos.originales <<- read.csv('", ruta, "', header=", 
                  encabezado, ", sep='", separador, "', dec = '", sep.decimal, "', row.names = 1)"))
  } else {
    return(paste0("datos.originales <<- read.csv('", ruta, "', header=", encabezado, ", sep='", separador, "', dec = '", sep.decimal, "')"))
  }
}

code.trans <- function(variables, nuevo.tipo){
  res <- ""
  if(nuevo.tipo == "Categorico"){
    for (variable in variables) {
      res <- paste0(res, "datos.originales[, '", variable, "'] <<- as.factor(datos.originales[, '", variable, "']) \n")
    }
  } else {
    res <- paste0("datos.modificados <<- datos.disyuntivos(datos.originales, '", variables,"')")
  }
  return(res)
}

resumen.numerico <- function(data, variable){
  salida <- ""
  datos.numericos <- list(Q1 = list(id = "q1", Label = "Primer Cuartil", Value = formatC(quantile(data[, variable], .25)), color = "green"),
                          Mediana = list(id = "mediana", Label = "Mediana", Value = formatC(median(data[, variable])), color = "orange"),
                          Q3 = list(id = "q3", Label = "Tercer Cuartil", Value = formatC(quantile(data[, variable], .75)), color = "maroon"),
                          Minimo = list(id = "minimo", Label = "Mínimo", Value = formatC(min(data[, variable])), color = "red"),
                          Promedio = list(id = "promedio", Label = "Promedio", Value = formatC(mean(data[, variable])), color = "blue"),
                          Maximo = list(id = "maximo", Label = "Máximo", Value = formatC(max(data[, variable])), color = "purple"),
                          DS <- list(id = "ds", Label = "Desviación Estandar", Value = formatC(max(data[, variable])), color = "yellow"))
  
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
  datos.numericos <- list(WP = list(id = "WP", Label = "Inercia Intra-Clases", Value = formatC(kmedias$tot.withinss), color = "red"),
                          BP = list(id = "BP", Label = "Inercia Inter-Clases", Value = formatC(kmedias$betweenss), color = "green"),
                          total = list(id = "total", Label = "Inercia Total", Value = formatC(kmedias$totss), color = "blue"))
  
  for (calculo in datos.numericos) {
    salida <- paste0(salida, "<div class='shiny-html-output col-sm-4 shiny-bound-output' id='", calculo$id, 
                     "'> <div class='small-box bg-", calculo$color,"'> <div class='inner'>",
                     "<h3>", calculo$Value, "</h3> <p>", calculo$Label, "</p></div> <div class='icon-large'> <i class='",
                     calculo$icon, "'></i></div></div></div>")
  }
  return(salida)
}

default.disp <- function(vars = NULL){
  if(is.null(vars)){
    return(NULL)
  } else if(length(vars) == 1){
    return(paste0("color <- rgb(sample(0:255, 1), sample(0:255, 1), sample(0:255, 1), 140, maxColorValue = 255)
hist(datos.originales[, '", vars, "'], col = color, border=F, main = paste0('Test de normalidad de la variable ','", vars,"'), axes=F, freq = F)
axis(1, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025)
axis(2, col=par('bg'), col.ticks='grey81', lwd.ticks=1, tck=-0.025)
curve(dnorm(x, mean = mean(datos.originales[, '", vars, "']), sd = sd(datos.originales[, '", vars, "'])), add=T, col='blue', lwd=2)
legend('bottom', legend = 'Curva Normal', col = 'blue', lty=1, cex=1.5)"))
  } else if(length(vars) == 2){
    return(paste0("color <- rgb(sample(0:255, 1), sample(0:255, 1), sample(0:255, 1), 255, maxColorValue = 255) 
ggplot(data = datos.originales, aes(x = ", vars[1], ", y = ", vars[2], ", label = rownames(datos.originales))) +
       geom_point(color = color, size = 3) + geom_text(vjust = -0.7)"))
  } else{
    return(paste0("colores <- rgb(sample(0:255, 1), sample(0:255, 1), sample(0:255, 1), 255, maxColorValue = 255)
scatterplot3d(datos.originales[, '", vars[1], "'], datos.originales[, '", 
                  vars[2], "'], datos.originales[, '", vars[3], "'], pch = 16, color = colores)"))
  }
}

def.pca.model <- function(data = "datos.originales", scale.unit = T, npc = 5){
  return(paste0("pca.modelo <<- PCA(var.numericas(", data, "), scale.unit = ", scale.unit, ", ncp = ", npc, ", graph = FALSE)"))
}

def.model <- function(data = "datos.originales", cant = "as.numeric(input$cant.cluster)", dist.method = "euclidean", hc.method = "complete"){
  return(paste0("hc.modelo <<- hclust(dist(var.numericas(", data, "), method = '", dist.method, "'), method = '", hc.method, "')
centros <<- calc.centros(var.numericas(", data, "), hc.modelo, ", cant, ")"))
}

def.k.model <- function(data = "datos.originales", cant = "as.numeric(input$cant.kmeans.cluster)", iter.max = 200, nstart = 300){
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

modelo.cor <- function(data = "datos.originales"){
  return(paste0("correlacion <<- cor(var.numericas(", data, "))"))
}

correlaciones <- function(metodo = 'circle', tipo = "lower"){
  return(paste0("corrplot(correlacion, method='", metodo,"', shade.col=NA, tl.col='black', 
         tl.srt=20, addCoef.col='black', order='AOE', type = '", tipo, "')"))
}

def.code.num <- function(variable = "input$sel.distribucion", color = 'input$col.dist'){
  return(paste0("distribucion.numerico(datos.originales[, ", variable, "], ", variable, ", color = ", color,")"))
}

def.code.cat <- function(variable = "input$sel.distribucion", color = 'input$col.dist'){
  return(paste0("distribucion.categorico(datos.originales[, ", variable,"], color = ", color, ")"))
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

diagrama <- function(cant = "as.numeric(input$cant.cluster)"){
  return(paste0("modelo <- color_branches(hc.modelo, k = ", cant, ", col = )
modelo <- color_labels(hc.modelo, k = ", cant, ", col = )
plot(modelo)"))
} 

def.code.jambu <- function(data = "datos.originales"){
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

cluster.mapa <- function(cant = "as.numeric(input$cant.cluster)"){
  return(paste0("res.hcpc <- HCPC(pca.modelo, nb.clust = -1, consol = TRUE, min = ", cant, ", max = ", cant, ", graph = FALSE)
fviz_pca_biplot(pca.modelo, col.ind = res.hcpc$data.clust$clust, palette = 'jco', addEllipses = T,
                label = 'var', col.var = 'steelblue', repel = TRUE, legend.title = 'Clúster')"))
}

cluster.kmapa <- function(){
  return(paste0("fviz_pca_biplot(pca.modelo, col.ind = as.factor(k.modelo$cluster), palette = 'jco', addEllipses = T,
                label = 'var', col.var = 'steelblue', repel = TRUE, legend.title = 'Clúster')"))
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
  ggplot(centros, aes(x=var, y=value)) + geom_bar(stat='identity', position='dodge') +
     scale_fill_discrete(name='Variable') + labs(x = '', y = '') + facet_wrap(~variable) + coord_flip() +
     theme_minimal() + theme(text = element_text(size = 20))
}"))
}

default.vert <- function(){
  return(paste0("centros.vertical.todos <<- function(centros){
  cluster <- c(1:nrow(centros))
  centros <- cbind(centros, cluster)
  centros <- melt(centros, id.vars = 'cluster')
  ggplot(centros, aes(x=variable, y=value, fill=factor(cluster))) + geom_bar(stat='identity', position='dodge') +
                scale_fill_discrete(name='Clúster') + labs(x = '', y = '')
}"))
}

cluster.horiz <- function(sel = "input$sel.cluster"){
  return(paste0("t.centros <- as.data.frame(t(centros$real))
if(", sel, " == 'Todos'){
    centros.horizontal.todos(t.centros)
} else {
    ggplot(data = t.centros, aes(x = row.names(t.centros), y = t.centros[, as.numeric(", sel, ")])) +
       geom_bar(stat = 'identity', fill = 'steelblue') + scale_y_continuous(expand = c(.01,0,0,0)) + labs(x = '', y = '') + 
       coord_flip() + theme_minimal()
}"))
}

cluster.khoriz <- function(sel = "input$sel.kmeans.cluster"){
  return(paste0("centros <- as.data.frame(t(k.modelo$centers))
if(", sel, " == 'Todos'){
   centros.horizontal.todos(centros)
} else{
   ggplot(data = centros, aes(x = row.names(centros), y = centros[, as.numeric(", sel, ")])) +
       geom_bar(stat = 'identity', fill = 'steelblue') + scale_y_continuous(expand = c(.01,0,0,0)) + labs(x = '', y = '') + 
       coord_flip() + theme_minimal()
}"))
}

cluster.vert <- function(sel = "input$sel.verticales"){
  return(paste0("real <- centros$real
if(", sel, " == 'Todos'){
  centros.vertical.todos(real)
} else{
  ggplot(data = real, aes(x = row.names(real), y = real[, ", sel, "], fill = row.names(real))) +
         geom_bar(stat = 'identity') + scale_fill_discrete(name = 'Clúster') + labs(x = '', y = '')
}"))
}

cluster.kvert <- function(sel = "input$sel.kmeans.verticales"){
  return(paste0("centros <- as.data.frame(k.modelo$centers)
if(", sel, " == 'Todos'){
    centros.vertical.todos(centros)
} else{
    ggplot(data = centros, aes(x = row.names(centros), y = centros[, ", sel, "], fill = row.names(centros))) +
         geom_bar(stat = 'identity') + scale_fill_discrete(name = 'Clúster') + labs(x = '', y = '')
}"))
}

cluster.cat <- function(var = "input$sel.kcat.var", cant = "input$cant.cluster"){
  return(paste0("hc.clusters <- cutree(hc.modelo, k=", cant, ")
NDatos <- cbind(datos.originales, grupo = hc.clusters)
                ggplot(NDatos, aes(", var, ")) + geom_bar(aes(fill = ", var, ")) + 
                facet_wrap(~grupo) + theme(text = element_text(size = 10), axis.text.x = element_blank()) +
                scale_fill_discrete(name='Variable') + labs(x = '', y = '')"))
}

cluster.kcat <- function(var = "input$sel.kcat.var"){
  return(paste0("NDatos <- cbind(datos.originales, grupo = k.modelo$cluster)
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

def.radar <- function(){
  return(paste0("centros.radar(centros$porcentual)"))
}

def.kradar <- function(){
  return(paste0("centros <- as.data.frame(apply(k.modelo$centers, 2, function(i) scales::rescale(i, to = c(0, 100))))
centros.radar(centros)"))
}

def.reporte <- function(){
  return(paste0("---
title: 'Untitled'
author: 'PROMIDAT'
date: Sys.Date()
output:
  html_document:
    df_print: paged
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

# Carga de librerias Necesarias
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

# Funciones

```{r}
var.numericas <- function(data){
  if(is.null(data)) return(NULL)
  res <- subset(data, select = sapply(data, class) %in% c('numeric', 'integer'))
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
```"))
}

cod.resum <- "summary(datos.originales)" 
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

# aux <- datos.disyuntivos(iris, "Species")
# k.modelo <<- kmeans(var.numericas(aux), 3, iter.max = 200, nstart = 300)
# 
# NDatos <- cbind(aux, grupo = k.modelo$cluster)
# NDatos <- cbind(NDatos, Species = iris$Species) 
# ggplot(NDatos, aes(Species)) + geom_bar(aes(fill = Species)) + 
#   facet_wrap(~grupo) + theme(text = element_text(size = 10), axis.text.x = element_blank()) +
#   scale_fill_discrete(name='Variable') + labs(x = '', y = '')


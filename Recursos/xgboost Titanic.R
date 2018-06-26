library(xgboost)

setwd("C:/Users/wendy/Desktop/Métodos predictivos")
datos <- read.table(file = "titanicV2.csv", sep = ",", dec = ".", row.names = 1, header = T)
datos <- na.omit(datos)
#datos$Survived <- as.numeric(ifelse(datos$Survived == "Survived", "1", "0"))

muestra <- sample(1:dim(iris)[1], dim(datos)[1]*0.3)
ttesting <- iris[muestra, ]
taprendizaje <- iris[-muestra, ]

modelo.xgb <- function(datos, variable.predecir, categoria.1){
  datos[, variable.predecir] <- as.numeric(ifelse(datos[, variable.predecir] == categoria.1, "1", "0"))
  datos <- xgb.DMatrix(data = data.matrix(datos[, colnames(datos) != variable.predecir]), label = data.matrix(datos[, variable.predecir]))
  
  params <- list(booster = "gbtree", objective = "binary:logistic", eta=0.3, 
                 gamma=0, max_depth=6, min_child_weight=1, subsample=1, colsample_bytree=1)
  
  xgbcv <- xgb.cv( params = params, data = datos, nrounds = 500, nfold = 10, showsd = T, 
                   stratified = T, print_every_n = 10, early_stop_round = 20, maximize = F)
  
  minError <- which.min(xgbcv[4]$evaluation_log$train_error_mean)
  xgb <- xgb.train (params = params, data = datos, nrounds = minError,verbose = 0,
                    watchlist = list(train=datos), print_every_n = 10, 
                    early_stop_round = 10, maximize = F , eval_metric = "error")
  return(xgb)
}

predict.xgb <- function(modelo, testing, corte = 0.5){
  testing <- xgb.DMatrix(data = data.matrix(testing))
  predict (modelo, testing)
  prediccion <- ifelse (prediccion > corte, 1, 0)
  return(prediccion)
}

modelo <- modelo.xgb(taprendizaje, "Survived", "Survived")
pred <- predict.xgb(modelo, ttesting, corte = 0.6)

variables.importantes <- xgb.importance (feature_names = colnames(taprendizaje), model = modelo)
xgb.plot.importance (importance_matrix = variables.importantes) 


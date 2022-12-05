#####################################################################################################
#                           CreaciÃ³n del conjunto de datos de modelado
#####################################################################################################
#
# Objetivo: Crear un conjunto de datos global a partir del atp_player, uniendo ambos conjunto y qui-
#           tando las variables que no son necesarias.
#
# Input: atp_player.RData
#
# Output: entrenamiento_probabilidad.RData.
# 
#####################################################################################################
rm(list=ls())


Sys.time()


datos_modelado<-readRDS(paste0("C:/Users/juana/OneDrive/Escritorio/TFM/scripts/code","/datos/", "model_data_sel.RDS"))

# Seleccion de variables


######

# Librerías

if (!require("data.table")){install.packages("data.table")}else{require("data.table")}
if (!require("MASS")){install.packages("MASS")}else{require("MASS")}
if (!require("tidyverse")){install.packages("tidyverse")}else{require("tidyverse")}
if (!require("dplyr")){install.packages("dplyr")}else{require("dplyr")}
if (!require("randomForest")){install.packages("randomForest")}else{require("randomForest")}
if (!require("gbm")){install.packages("gbm")}else{require("gbm")}
if (!require("doParallel")){install.packages("doParallel")}else{require("doParallel")}
if (!require("foreach")){install.packages("foreach")}else{require("foreach")}
source(paste0("C:/Users/juana/OneDrive/Escritorio/TFM/scripts/code/","aux_functions.R"))


# 5 modelos:

# General

# Para cada uno divisiÃ³n entre 1990-2017 para entrenamiento

# Test 2018 - 2021

# En caso de modelo de superficie, filtrar por ella


#datos_modelado <- datos_modelado  %>%  select(year,surface, tourney_level,t_num_juegos,hand_p1:ht_p1,age_p1,rank_points_p1, win_temp_p1:sd_atp_point_l_10_s_p1,hand_p2:ht_p2,age_p2,rank_points_p2, win_temp_p2:Win)

datos_modelado$time <- as.Date(datos_modelado$time, format = "%Y-%m-%d")
# DivisiÃ³n conjunto de entrenamiento y test
datos_modelado <- as.data.table(datos_modelado)
datos_modelado$edad_p1 <- as.numeric(datos_modelado$edad_p1)
datos_modelado$edad_p2 <- as.numeric(datos_modelado$edad_p2)

gen_ent <- datos_modelado[datos_modelado$time<as.Date("2020-01-01",format="%Y-%m-%d"),]
#summary(gen_ent)
gen_test <- datos_modelado[datos_modelado$time>=as.Date("2020-01-01",format="%Y-%m-%d") ,]

# 
# # Clay
# gen_ent_clay <- gen_ent[gen_ent$surface == "Clay"]
# gen_test_clay <- gen_test[gen_test$surface == "Clay"]
# gen_ent_clay$surface <- NULL
# gen_test_clay$surface <- NULL
# 
# # Carpet
# gen_ent_carpet <- gen_ent[gen_ent$surface == "Carpet"]
# gen_test_carpet <- gen_test[gen_test$surface == "Carpet"]
# gen_ent_carpet$surface <- NULL
# gen_test_carpet$surface <- NULL
# 
# # Hard
# gen_ent_hard <- gen_ent[gen_ent$surface == "Hard"]
# gen_test_hard <- gen_test[gen_test$surface == "Hard"]
# gen_ent_hard$surface <- NULL
# gen_test_hard$surface <- NULL
# 
# # Grass
# gen_ent_grass <- gen_ent[gen_ent$surface == "Grass"]
# gen_test_grass <- gen_test[gen_test$surface == "Grass"]
# gen_ent_grass$surface <- NULL
# gen_test_grass$surface<- NULL


############################
##          Boosting      ##
############################
require(gbm)
require(MASS)#package with the boston housing dataset

#Modify training and test data (Win tiene que ser numerico!!!!!!!!!!!)

########## GENERAL #############
print("Boosting General")



cv_error  <- vector("numeric")
n_arboles <- vector("numeric")
shrinkage <- vector("numeric")

print("........Optimizacion shrinkage")
for (i in c(0.001, 0.005, 0.01, 0.05, 0.1)) {
  set.seed(123)
  arbol_boosting <- gbm(Win ~ .-name_p1-name_p2-player_id_p1-player_id_p2-time-match_id, data = gen_ent,
                        #distribution = "bernoulli", #bernoulli
                        #n.trees = 5000,
                        n.trees = 50,
                        interaction.depth = 1,
                        shrinkage = i,
                        n.minobsinnode = 10,
                        bag.fraction = 0.5,
                        cv.folds = 5)
  cv_error  <- c(cv_error, arbol_boosting$cv.error)
  n_arboles <- c(n_arboles, seq_along(arbol_boosting$cv.error))
  shrinkage <- c(shrinkage, rep(i, length(arbol_boosting$cv.error)))
  print(i)
}
error <- data.frame(cv_error, n_arboles, shrinkage)

ggplot(data = error, aes(x = n_arboles, y = cv_error,
                         color = as.factor(shrinkage))) +
  geom_smooth() +
  labs(title = "Evolución del cv-error", color = "shrinkage") + 
  theme_bw() +
  theme(legend.position = "bottom")

hiperparametro_shrinkage <- error %>% arrange(cv_error)

best_shrinkage_boost <- (hiperparametro_shrinkage$shrinkage[1]) #0.1

#interaction.depth
print("........Optimizacion depth")
cv_error  <- vector("numeric")
n_arboles <- vector("numeric")
interaction.depth <- vector("numeric")
for (i in c(1, 2, 3, 5)) {
  set.seed(123)
  arbol_boosting <- gbm(Win ~ .-name_p1-name_p2-player_id_p1-player_id_p2-time-match_id, data = gen_ent,
                        #distribution = "adaboost",
                        n.trees = 50,
                        interaction.depth = i,
                        shrinkage = best_shrinkage_boost,
                        n.minobsinnode = 10,
                        bag.fraction = 0.5,
                        cv.folds = 5)
  cv_error  <- c(cv_error, arbol_boosting$cv.error)
  n_arboles <- c(n_arboles, seq_along(arbol_boosting$cv.error))
  interaction.depth <- c(interaction.depth,
                         rep(i, length(arbol_boosting$cv.error)))
  print(i)
}
error <- data.frame(cv_error, n_arboles, interaction.depth)

ggplot(data = error, aes(x = n_arboles, y = cv_error,
                         color = as.factor(interaction.depth))) +
  geom_smooth() +
  labs(title = "Evolución del cv-error", color = "interaction.depth") + 
  theme_bw() +
  theme(legend.position = "bottom")

hiperparametro_depth <- error %>% arrange(cv_error)

best_depth_boost <- as.integer(hiperparametro_depth$interaction.depth[1])


#n.minobsinnode (Ver si tiene sentido hacerlo)
print("........Optimizacion min obs")
cv_error  <- vector("numeric")
n_arboles <- vector("numeric")
n.minobsinnode <- vector("numeric")
for (i in c(5, 10, 20)) {
  arbol_boosting <- gbm(Win ~ .-name_p1-name_p2-player_id_p1-player_id_p2-time-match_id, data = gen_ent,
                        #distribution = "adaboost",
                        n.trees = 50,
                        interaction.depth = best_depth_boost,
                        shrinkage = best_shrinkage_boost,
                        n.minobsinnode = i,
                        bag.fraction = 0.5,
                        cv.folds = 5)
  cv_error  <- c(cv_error, arbol_boosting$cv.error)
  n_arboles <- c(n_arboles, seq_along(arbol_boosting$cv.error))
  n.minobsinnode <- c(n.minobsinnode,
                      rep(i, length(arbol_boosting$cv.error)))
  print(i)
}


error <- data.frame(cv_error, n_arboles, n.minobsinnode)

ggplot(data = error, aes(x = n_arboles, y = cv_error,
                         color = as.factor(n.minobsinnode))) +
  geom_smooth() +
  labs(title = "Evolución del cv-error", color = "n.minobsinnode") + 
  theme_bw() +
  theme(legend.position = "bottom")

hiperparametro_obs <- error %>% arrange(cv_error)

best_obs_boost <- as.integer(hiperparametro_obs$n.minobsinnode[1])
#best_obs_boost <-10

#Numero arboles
print("........Optimizacion n arboles")
modelo_boost <-gbm(Win ~ .-name_p1-name_p2-player_id_p1-player_id_p2-time-match_id, data = gen_ent,
                   #distribution = "adaboost",
                   n.trees = 1000,
                   interaction.depth = best_depth_boost,
                   shrinkage = best_shrinkage_boost,
                   n.minobsinnode = best_obs_boost,
                   bag.fraction = 0.5,
                   cv.folds = 5)

error <- data.frame(cv_error = modelo_boost$cv.error,
                    n_arboles = seq_along(modelo_boost$cv.error))
ggplot(data = error, aes(x = n_arboles, y = cv_error)) +
  geom_line(color = "black") +
  geom_point(data = error[which.min(error$cv_error),], color = "red") +
  labs(title = "Evolución del cv-error") + 
  theme_bw() 

hiperparametro_arb <- error %>% arrange(cv_error)

best_arb_boost <- as.integer(hiperparametro_arb$n_arboles[1])


#Predicciones
predicciones <- predict(object = modelo_boost, newdata = gen_test,
                        n.trees = best_arb_boost,type = "response")
hist(predicciones)
predicted.classes_boost<- ifelse(predicciones > 0.5, 1, 0)
# Prediction accuracy
observed.classes_boost <- gen_test$Win
acierto_general <- mean(predicted.classes_boost == observed.classes_boost)
print(paste0("Acierto: ", acierto_general*100, "%"))

importancia_pred <- summary(modelo_boost, plotit = FALSE)
importancia_pred <- importancia_pred[1:10,]
ggplot(data = importancia_pred, aes(x = reorder(var, rel.inf), y = rel.inf,
                                    fill = rel.inf)) +
  labs(x = "variable", title = "ReducciÃ³n de MSE") +
  geom_col() +
  coord_flip() +
  theme_bw() +
  theme(legend.position = "bottom")

hist(predicciones[predicted.classes_boost != observed.classes_boost])



save(list = ls(),
     file = paste0("C:/Users/juana/OneDrive/Escritorio/TFM/scripts/code","/datos/", "model_data_boost_sel.RData"))
######################################################################################################




#####################################################################################################


fun_aux <- function(tabla) {
  tabla_aux <- tabla[, c("p1","p2","surface_m", "tourney_level_m", "round_m"), with=F]
  
  prob_tabla <- data.frame(t(apply(tabla_aux, 1, pred_prob)))
  colnames(prob_tabla) <- c("jugador", "flag", "prob", "desv_prob")
  
  tabla_cuotas_aux <- tabla[,c("cuota_p1", "cuota_p2"), with=F]
  
  tabla_cuotas <- cbind(prob_tabla$flag, tabla_cuotas_aux)
  
  cuota <- apply(tabla_cuotas, 1, sel_cuota)
  
  results <- simplex_optimization(data.frame(prob_tabla$jugador), data.frame(cuota),
                                  data.frame(as.numeric(prob_tabla$prob)),
                                  as.numeric(input$N), as.numeric(input$lambda),
                                  as.numeric(input$risk_level))
  
  return(results)
  
}

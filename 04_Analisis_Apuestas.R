###################################################################################################
#                                   MAXIMIZAR DINERO APOSTADO
###################################################################################################
#
# Objetivo: El objetivo es maximizar la esperanza del dinero ganado.
#
# Input: - Jugador: Vector con los nombre de los jugadores a los que se va a apostar.
#        - Prob: Probabilidad de victoria que se ha predicho para el jugador y el partido en
#                cuestión.
#        - Cuota: Vector con las cuotas correspondientes a las victorias de los jugadores.
#        - N: Dinero que se dispone para apostar.
#        - nivel_riesgo: A partir de que probabilidad apostarias sin límite; es decir, si tu
#                        nivel de riesgo es del 0.4, sólo se apuesta sin penalización a aquellos
#                        jugadores cuya probabilidad es superior al 40%. En caso de que un jugador
#                        tenga una probabilidad se le penaliza por riesgo.
#        -Lambda: Nivel de penalización para las probabilidades inferiores al nivel de riesgo. A 
#                 mayor lambda, más penalización y por tanto, apuestas más conservadoras.
#
# Output: - Dinero esperado a ganar.
#         - Tabla con el jugador, probabilidad de victoria, cuota, dinero a apostar y dinero 
#           que se obtiene si el jugador gana.
#
###################################################################################################

##############################
# Carga funciones auxiliares #
##############################
rm(list=ls())

load(file = paste0(sub("/scripts", "", getwd()), "/datos/", "model_data_boost.RData"))
load(file = paste0(sub("/scripts", "", getwd()), "/datos/", "model_data.RData"))



if (!require("data.table")){install.packages("data.table")}else{require("data.table")}
if (!require("MASS")){install.packages("MASS")}else{require("MASS")}
if (!require("tidyverse")){install.packages("tidyverse")}else{require("tidyverse")}
if (!require("dplyr")){install.packages("dplyr")}else{require("dplyr")}
if (!require("randomForest")){install.packages("randomForest")}else{require("randomForest")}
if (!require("gbm")){install.packages("gbm")}else{require("gbm")}
if (!require("doParallel")){install.packages("doParallel")}else{require("doParallel")}
if (!require("foreach")){install.packages("foreach")}else{require("foreach")}

source(paste0(sub("/scripts","",getwd()),"/scripts/","aux_functions.R"))
#########################
# Parámetros de entrada #
#########################

jugador <- data.frame(c("John Millman", "Borna Coric", "Diego Swartzman", "Andrey Rublev", "Marin Cilic", "Andy Murray"))
colnames(jugador)<-c("jugador")
cuota <- data.frame(c(1.8, 1.66, 1.44, 1.40, 2, 1.72))
colnames(cuota)<-c("cuota")
prob <- data.frame(c(0.9951, 0.8252, 0.91, 0.7, 0.99, 0.999))
colnames(prob)<-"prob"
N <- 100
lambda<- 3 # Nivel de conservadurismo, a más alto, menos riesgo. A más bajo, más riesgo y más
# posibilidad de perder todo el dinero
nivel_riesgo <- 0.8

###########################
# Optimización del dinero #
###########################

simplex_optimization(jugador, cuota, prob, N,lambda,nivel_riesgo)


##########
# Prueba #
##########

data <- read.csv("prueba_apuestas1.csv",header = T,sep = ";")
colnames(data)[1] <- "p1"
data$tourney_level_m[data$tourney_level_m == "FALSE"] <- "F"
data$round_m[data$round_m == "FALSE"] <- "F"
data
l_names <- unique(atp_players$name)

data$p1 %in% l_names
data$p1[!data$p1 %in% l_names]

data$p2 %in% l_names
data$p2[!data$p2 %in% l_names]

data <- data[data$p1 %in% l_names & data$p2 %in% l_names, ]

#data <- data[-c(3,6,14,15),]

data_cuotas_aux <- data.frame(cuota_p1 = data$cuota_p1,
                              cuota_p2 = data$cuota_p2)

prob_tabla <- data.table(t(apply(data, 1, pred_prob)))
colnames(prob_tabla) <- c("winner", "flag", "prob", "desv_prob", "loser", "aplica")
prob_tabla

# prob_tabla <- prob_tabla[-c(4,9),]
# data <- data[-c(4,9),]
# data_cuotas_aux <- data_cuotas_aux[-c(4,9),]

data_cuotas_aux_2 <- cbind(data[,c("p1","p2")], prob_tabla[,"flag"], data_cuotas_aux,prob_tabla[,"aplica"])
vector <- data_cuotas_aux_2$aplica == "SI"
#data_cuotas_aux_2 <- data_cuotas_aux_2[data_cuotas_aux_2$aplica == "SI",]

data_cuotas_aux_3 <- data.table(t(apply(data_cuotas_aux_2, 1, sel_cuota)))
colnames(data_cuotas_aux_3) <- c("nombre_a", "nombre_b", "cuota_a", "cuota_b")
data_cuotas_aux_3

data_cuotas <- cbind(data_cuotas_aux_3, prob_tabla$prob, prob_tabla$flag)
colnames(data_cuotas) <- c(colnames(data_cuotas_aux_3), "prob", "flag")
data_cuotas$cuota_a <- as.numeric(data_cuotas$cuota_a)
data_cuotas$cuota_b <- as.numeric(data_cuotas$cuota_b)
data_cuotas$prob <- as.numeric(data_cuotas$prob)
data_cuotas$flag <- as.integer(data_cuotas$flag)

data_cuotas <- data_cuotas[vector,]


data_Alg_Kelly <- data.table(t(apply(data_cuotas, 1, AlgoritmoKelly)))
colnames(data_Alg_Kelly) <- c("nombre", "cuota", "prob", "flag")
data_Alg_Kelly

data_Alg_Kelly <- data_Alg_Kelly[complete.cases(data_Alg_Kelly),]
#data_Alg_Kelly <- data_Alg_Kelly[-c(1,4),]
data_Alg_Kelly

N <- 10
lambda <- 10
risk_level <- 0.1

simplex_optimization(data.frame(data_Alg_Kelly$nombre),
                     data.frame(as.numeric(data_Alg_Kelly$cuota)), 
                     data.frame(as.numeric(data_Alg_Kelly$prob)),
                     N, lambda, risk_level)

a <- simplex_optimization(data.frame(data_Alg_Kelly$nombre),
                     data.frame(as.numeric(data_Alg_Kelly$cuota)), 
                     data.frame(as.numeric(data_Alg_Kelly$prob)),
                     N, lambda, risk_level)
arrange(a,-Probabilidad)

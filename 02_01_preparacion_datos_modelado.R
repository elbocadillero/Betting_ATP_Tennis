################################################################################
#                        TRATAMIENTO DE LOS DATOS                              #
################################################################################
#                                                                              #
# Objetivo: Obtención del conjunto de datos formateado y creación de variables #
#           necesarias para empezar a modelar.                                 #
#                                                                              #
# Input: Fichero atp_matches.Rdata.                                            #
#                                                                              #
# Output:                                                                      #
#                                                                              #
# M. Perez, E. Sevillano y J Velázquez                                         #
################################################################################
rm(list=ls())

if (!require("hclust")){install.packages("hclust")
}else{require("hclust")}
if (!require("reshape2")){install.packages("reshape2")
}else{require("reshape2")}
if (!require("tidyverse")){install.packages("tidyverse")
}else{require("tidyverse")}
library(tidyverse)
source(paste0("C:/Users/juana/OneDrive/Escritorio/TFM/scripts/code/","aux_functions.R"))

load(file = paste0("C:/Users/juana/OneDrive/Escritorio/TFM/scripts/code","/datos/", "atp_data_sel.RData"))
data_variables$tourney_round_name <- as.character(data_variables$tourney_round_name)
data_variables$tourney_surface <- as.character(data_variables$tourney_surface)
data_variables <- subset(data_variables, select =-c(match_duration,serve_rating,aces,return_rating,sets_won,games_won,tiebreaks_won,walkout,Ranking,perc_first_serves_in,perc_first_points_won,perc_second_points_won,perc_break_points_saved,
                                  perc_first_serve_return_won,perc_second_serve_return_won,perc_break_points_converted,perc_service_points_won,
                                  perc_return_points_won,perc_total_points_won))


data_variables$time <- as.character(data_variables$time)
data_variables_sin_nan<-data_variables %>% mutate_all(~replace(., is.nan(.), 0))
data_variables$time <- as.Date(data_variables$time, format = "%Y-%m-%d") 

data_variables_completos <- data_variables_sin_nan[complete.cases(data_variables_sin_nan),]




# Eliminación de variables por correlación
target <- data_variables_completos$Win


datos_cor <- subset(data_variables_completos, select = -c(player_id,time,match_id,tourney_round_name,name,tourney_type,tourney_conditions,tourney_surface,Hand,
                                                          Backhand,Win) )

datos_cor<-sapply(datos_cor, as.numeric)
target<-sapply(target, as.numeric)

matriz_correlacion_target <- data.frame(cor=abs(cor(datos_cor,target)))
matriz_correlacion_target$var <- rownames(matriz_correlacion_target)
rownames(matriz_correlacion_target)<-NULL

matriz_correlacion <- abs(cor(datos_cor))
diag(matriz_correlacion) <- 0
matriz_correlacion[upper.tri(matriz_correlacion, diag = FALSE)] <- 0

matriz_correlacion[is.na(matriz_correlacion)]<-1
matriz_correlacion_target[is.na(matriz_correlacion_target)]<-0

datos_cor_melt<-data.frame(setNames(melt(matriz_correlacion), c('var1', 'var2', 'cor')))

datos_prueba_correlacion <- elimina_variables_correladas(datos_cor_melt, matriz_correlacion_target, data_variables_completos, alfa=0.95)

# Transformar a factor las variables que entrarán en el modelo
datos_prueba_correlacion$tourney_round_name <- ordered(datos_prueba_correlacion$tourney_round_name,
                                   levels = c("1st Round Qualifying", "2nd Round Qualifying", "3rd Round Qualifying", "Round Robin", "Round of 128" ,
                                              "Round of 64", "Round of 32", "Round of 16", "Quarter-Finals", "Semi-Finals","Finals"),
                                   labels = c("1st Round Qualifying", "2nd Round Qualifying", "3rd Round Qualifying", "Round Robin", "Round of 128" ,
                                              "Round of 64", "Round of 32", "Round of 16", "Quarter-Finals", "Semi-Finals","Finals"))

datos_prueba_correlacion$tourney_type <- factor(datos_prueba_correlacion$tourney_type)
datos_prueba_correlacion$tourney_conditions <- factor(datos_prueba_correlacion$tourney_conditions)
datos_prueba_correlacion$tourney_surface <- factor(datos_prueba_correlacion$tourney_surface)
datos_prueba_correlacion$Hand <- factor(datos_prueba_correlacion$Hand)
datos_prueba_correlacion$Backhand <- factor(datos_prueba_correlacion$Backhand)

variables_partido <- c("time","tourney_round_name","tourney_type", "tourney_conditions","tourney_surface")
  
comunes_partido <- datos_prueba_correlacion[,c("match_id",variables_partido)]

copy_data <- subset(datos_prueba_correlacion, select = -c(Win,tourney_round_name,tourney_type, tourney_conditions,tourney_surface,time) )

datos_modelado_act <- merge(datos_prueba_correlacion, copy_data, by="match_id") %>% filter(player_id.x!=player_id.y)


colnames(datos_modelado_act) <- gsub(colnames(datos_modelado_act), pattern = ".x", replace = "_p1",fixed=T)
colnames(datos_modelado_act) <- gsub(colnames(datos_modelado_act), pattern = ".y", replace = "_p2",fixed=T)


saveRDS(datos_modelado_act,paste0("C:/Users/juana/OneDrive/Escritorio/TFM/scripts/code","/datos/", "model_data_sel.RDS")) 
# Primera prueba de modelado








# Solcuión que no coja el día del partido (soluicionado se prueba al final)
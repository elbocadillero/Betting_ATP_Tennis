################################################################################
#                        TRATAMIENTO DE LOS DATOS                              #
################################################################################
#                                                                              #
# Objetivo: Obtención del conjunto de datos formateado y creación de variables #
#           necesarias para empezar a modelar.                                 #
#                                                                              #
# Input: Fichero atp_matches.Rdata.                                            #
#                                                                              #
# Output:
#                                                                              #
# M. Perez, E. Sevillano y J Velázquez                                         #
################################################################################

# Limpieza del entorno:
rm(list=ls())

Sys.time() # Hora ejecución (para que se imprima en el log)

#######################
# Paquetes necesarios #
#######################

if (!require("tidyverse")){install.packages("tidyverse")
}else{require("tidyverse")}
if (!require("data.table")){install.packages("data.table")
}else{require("data.table")}
if (!require("lubridate")){install.packages("lubridate")
}else{require("lubridate")}


##############################
# Carga funciones auxiliares #
##############################

source(paste0("C:/Users/juana/OneDrive/Escritorio/TFM/scripts/code/","aux_functions.R"))

###############################
# Carga del conjunto de datos #
###############################

data <- read.csv(file = paste0("C:/Users/juana/OneDrive/Escritorio/TFM/scripts/data/","dataset_completo.csv"),sep=",")

###############
# Tratamiento #
###############

data$tourney_round_name <- ordered(data$tourney_round_name,
                         levels = c("1st Round Qualifying", "2nd Round Qualifying", "3rd Round Qualifying", "Round Robin", "Round of 128" ,
                                    "Round of 64", "Round of 32", "Round of 16", "Quarter-Finals", "Semi-Finals","Finals"),
                         labels = c("1st Round Qualifying", "2nd Round Qualifying", "3rd Round Qualifying", "Round Robin", "Round of 128" ,
                                    "Round of 64", "Round of 32", "Round of 16", "Quarter-Finals", "Semi-Finals","Finals"))

data$tourney_surface <- factor(data$tourney_surface)

# Completar con la mano

# atp_act <- atp_act %>%
#   filter(!is.na(winner_hand), !is.na(loser_hand),
#          winner_hand != "U", loser_hand != "U",
#          winner_hand != "", loser_hand != "")
# atp_act$winner_hand <- factor(atp_act$winner_hand)
# atp_act$loser_hand <- factor(atp_act$loser_hand)


data <- as.data.table(data)

# Se separan los distintos sets

data$walkout <- ifelse(grepl("(RET)",data$match_score_tiebreaks),1,0)

data$match_score_tiebreaks<-str_trim(gsub("(RET)","",data$match_score_tiebreaks, fixed=TRUE))

data <- data %>%
  separate(match_score_tiebreaks,
           into = c("set1", "set2", "set3", "set4", "set5"),
           sep = " ", convert = TRUE)


data$best_of <- ifelse(data$tourney_type=="Grand Slam",5,3)

sub_data <- data[,c("set1","set2","set3","set4","set5","best_of"), with=F]
sub_data <- as.data.table(t(apply(sub_data,1,function(x){
  gsub("(\\?|\\¿)","",x)
}
)))####NUEVOOOOOOO!!!!!!!!!

results <- as.data.table(t(apply(sub_data,1,Walkout_function)))
colnames(results) <- c("sets_played",
                       "t_num_juegos", "winner_num_juegos", "loser_num_juegos")


data <- cbind(data, results)

max_round_data <-as.data.table(data %>%
                    group_by(tourney_id) %>%
                    summarise(max_round=max(round_order)))
data <- merge(data, max_round_data,by = "tourney_id")

data[,c("start_date"):={
  time = as.Date(as.character(start_date), format = "%Y.%m.%d")
  .(time = time)}]
data[,c("end_date"):={
  time = as.Date(as.character(end_date), format = "%Y.%m.%d")
  .(time = time)}]

data<-data[!is.na(data$end_date) & !is.na(data$start_date),]
date <- c()
for (i in 1:nrow(data)) {
  date[i] = calcula_fecha(data[i,c("end_date","start_date","max_round","round_order")])
}
date  

data$time <- as.Date(as.character(date), format = "%Y-%m-%d") 


#data <- data %>%
#  separate(time, into=c("year","month","day"), sep=c(4,7), convert = T)

data$year <- format(data$time, format="%Y")
data$month <- format(data$time, format="%m")
data$day <- format(data$time, format="%d")

data <- data[data$year>=1995,] %>%
  drop_na(sets_played, t_num_juegos, winner_num_juegos, loser_num_juegos)

variables_comunes <- colnames(data)[(!grepl("winner",colnames(data)) & !grepl("loser",colnames(data)))]
                                    
nombres_loser <- colnames(data)[grepl("loser",colnames(data))]                               


nombres_winner <- colnames(data)[grepl("winner",colnames(data))]                               

data_winner <- data[,colnames(data) %in% c(variables_comunes,nombres_winner),with=FALSE]
data_winner[, c("singles_winner_name","singles_winner_player_slug","singles_winner_player_id"):=NULL]


data_loser <- data[,colnames(data) %in% c(variables_comunes,nombres_loser),with=FALSE]


players_data <- read.csv(file = paste0("C:/Users/juana/OneDrive/Escritorio/TFM/scripts/data/","players_completo.csv"),sep=",")

data_winner <- merge(data_winner, players_data,by.x = "winner_player_id",by.y="Id")
data_loser <- merge(data_loser, players_data,by.x = "loser_player_id",by.y="Id")

data_winner$Win <- 1
data_loser$Win <- 0

colnames(data_winner) <- gsub(colnames(data_winner), pattern = "winner_", replace = "")
colnames(data_loser) <- gsub(colnames(data_loser), pattern = "loser_", replace = "")

atp_players_act <- rbind(data_winner, data_loser) %>%
  arrange(desc(year), desc(month), desc(day), desc(round_order))

atp_players_act <- as.data.table(atp_players_act)

atp_players_act$Birthday <- as.Date(as.character(atp_players_act$Birthday), format = "%Y-%m-%d") 
atp_players_act$edad = floor(difftime(atp_players_act$time, atp_players_act$Birthday, units = "days")/365)

# Cálculo de porcentajes

atp_players_act$perc_first_serves_in <- round(atp_players_act$first_serves_in/atp_players_act$first_serves_total*100,2)
atp_players_act$perc_first_points_won <- round(atp_players_act$first_serve_points_won/atp_players_act$first_serve_points_total*100,2)
atp_players_act$perc_second_points_won <- round(atp_players_act$second_serve_points_won/atp_players_act$second_serve_points_total*100,2)
atp_players_act$perc_break_points_saved <- round(atp_players_act$break_points_saved/atp_players_act$break_points_serve_total*100,2)
atp_players_act$perc_first_serve_return_won <- round(atp_players_act$first_serve_return_won/atp_players_act$first_serve_return_total*100,2)
atp_players_act$perc_second_serve_return_won <- round(atp_players_act$second_serve_return_won/atp_players_act$second_serve_return_total*100,2)
atp_players_act$perc_break_points_converted <- round(atp_players_act$break_points_converted/atp_players_act$break_points_return_total*100,2)
atp_players_act$perc_service_points_won<-round(atp_players_act$service_points_won/atp_players_act$service_points_total*100,2)
atp_players_act$perc_return_points_won<-round(atp_players_act$return_points_won/atp_players_act$return_points_total*100,2)
atp_players_act$perc_total_points_won<-round(atp_players_act$total_points_won/atp_players_act$total_points_total*100,2)

atp_players_act<-atp_players_act[,-c("tourney_id","X.x","Unnamed..0",
"tourney_slug","match_time",
"slug","first_serves_in",
"first_serves_total","first_serve_points_won","first_serve_points_total","second_serve_points_won",
"second_serve_points_total","break_points_saved","break_points_serve_total","service_games_played",
"first_serve_return_won","first_serve_return_total","second_serve_return_won",
"second_serve_return_total","break_points_converted","break_points_return_total","return_games_played",
"service_points_won","service_points_total","return_points_won","return_points_total",
"total_points_won","total_points_total","start_date","start_month",
"start_day","end_date","end_month","end_day",
"currency","prize_money","match_index",
"round_order","match_order","set1",
"set2","set3","set4","set5","Unnamed..0.1",
"tourney_order","tourney_name","tourney_location",
"tourney_date","year","tourney_month","tourney_day",
"tourney_singles_draw","tourney_fin_commit_raw",
"tourney_fin_commit","best_of","sets_played",
"t_num_juegos","num_juegos","max_round",
"month","day","X.y","Name",
"Slug","Nationality","Birthday","isActive")]



variables <- c("serve_rating","return_rating","sets_won","games_won","aces","perc_first_points_won","perc_second_points_won","perc_break_points_saved","perc_first_serve_return_won","perc_second_serve_return_won","perc_break_points_converted","perc_service_points_won","perc_return_points_won","perc_total_points_won","match_duration","Win")
surfaces = c("Carpet","Clay","Grass","Hard")
data_variables <- calcula_variables(variables, datos=atp_players_act,num_partidos=10, surface=surfaces, num_dias=7)

save(list = c("atp_players_act", "data_variables"), file = paste0("C:/Users/juana/OneDrive/Escritorio/TFM/scripts/code","/datos/", "atp_data_sel.RData"))

#################################################


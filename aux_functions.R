################################################################################
#                         FUNCIONES AUXILIARES                                 #
################################################################################
#                                                                              #
# Objetivo: Recopilación de las funciones auxiliares necesarias durante los    #
#           diversos procesos de la obtención, depurado y modelados de los     #
#           datos.                                                             #
#                                                                              #
################################################################################


################################################################################
#          OBTENCIÓN VARIABLES Nº SETS Y JUEGOS JUGADOS Y RETIRADA             #
################################################################################
#                                                                              #
# Objetivo: Obtención de variables número de sets jugadores, si hay retirada   #
#           del partido, número de juegos jugados y número de juegos ganados   #
#           por cada jugador.                                                  #
#                                                                              #
# Input:  Las variables de las puntuaciones de cada set tabla "atp" y          #
#         la variable "best of" de la misma tabla.                             #
#                                                                              #
# Output: Variables de los partidos:                                           #
#                                                                              #
#           - Número de sets jugados.                                          #
#           - Retirada en el partido.                                          #
#           - Número de juegos jugados.                                        #
#           - Número de juegos ganados por el ganador.                         #
#           - Número de juegos ganados por el perdedor.                        #
#                                                                              #
################################################################################

elimina_variables_correladas <- function(cor_total, cor_target, data, alfa){
  variables_a_eliminar <- c()
  variables <- unique(cor_target$var)
  for (var_i in variables){
    datos <- cor_total[cor_total$var1==var_i & cor_total$cor>alfa,]
    variables_correladas <- unique(datos$var2)
    for (var_j in variables_correladas){
      cor_var_i <- cor_target[cor_target$var==var_i,]$cor
      cor_var_j <- cor_target[cor_target$var==var_j,]$cor

      if (cor_var_i>cor_var_j){variables_a_eliminar<-c(variables_a_eliminar,var_j)}
      else{variables_a_eliminar<-c(variables_a_eliminar,var_i)}
    }
    
  }
  variables_a_eliminar<- unique(variables_a_eliminar)
  data = data.frame(data)
  data_new = data[,names(data)[!(names(data) %in% variables_a_eliminar)]]
  print(names(data)[!(names(data) %in% variables_a_eliminar)])
  return(data_new)
  
}


calcula_variables <- function(input_variables, datos,num_partidos, surface, num_dias){
  datos_nuevas_variables <- data.frame()
  setorder(datos, -time)
  jugadores <- unique(datos$player_id)
  pb = txtProgressBar(min = 0, max = length(jugadores), initial = 0) 
  n = length(jugadores)
  contador = 1
  pb <- txtProgressBar(min = 0,      # Minimum value of the progress bar
                       max = n, # Maximum value of the progress bar
                       style = 3,    # Progress bar style (also available style = 1 and style = 2)
                       width = 50,   # Progress bar width. Defaults to getOption("width")
                       char = "=")
  for (jug_i in jugadores){
    tiempo = round(contador/n*100,2)
    
    setTxtProgressBar(pb,jug_i)
    datos_jugador <- datos[player_id ==  jug_i] 
    setorder(datos_jugador, -time)
    for (i in 1:nrow(datos_jugador)) {
      # Datos 10 ?ltimos partidos
      input <- datos_jugador[i,]
      n_jugador <- nrow(datos_jugador)
      if(i!=n_jugador){
        # No se hace nada para el ?ltimo partido
        datos_10 <- datos_jugador[(i+1):(min(i+1+num_partidos,nrow(datos_jugador))),]
        datos_s <- datos_jugador[datos_jugador$tourney_surface == input$tourney_surface]
        datos_10_s <- datos_s[(i+1):min(i+1+num_partidos,nrow(datos_s)),]
        
        
        
        #Imputaci?n variables
        
        
        for (var_i in input_variables) {
          eval(parse(text=paste0("if (is.na(input$",var_i,") | is.nan(input$",var_i,")){input$",var_i," <- mean(datos_s$",var_i,", na.rm = T)}")))
          
        }
        
        # C?lculo de variables 
        datos_sel <- datos_jugador[(datos_jugador$time >= (input$time - num_dias)) & (datos_jugador$time < (input$time))]
        
        # N?mero de partidos y duraci?n de los ?ltimos num_dias
        
        eval(parse(text=paste0("input$n_played_matches_",num_dias," = nrow(datos_sel)")))
        eval(parse(text=paste0("input$t_played_matches_",num_dias," = sum(datos_sel$match_duration)")))
        
        for (var_i in input_variables) {
          # Ultimos num_dias
          eval(parse(text=paste0("input$m_",var_i,"<-mean(datos_sel$",var_i,",na.rm=T)")))
          #eval(parse(text=paste0("if (nrow(datos_sel)>0) {input$sd_",var_i," <- sd(datos_sel$",var_i,", na.rm = T)} else {input$sd_",var_i," <- 0}")))
          
          #eval(parse(text=paste0("if (nrow(datos_sel[datos_sel$Win == 1])>0) {input$m_w_",var_i," <- mean(datos_sel[datos_sel$Win == 1]$",var_i,", na.rm = T)} else {input$m_w_",var_i," <- 0}")))
          #eval(parse(text=paste0("if (nrow(datos_sel[datos_sel$Win == 0])>0) {input$m_l_",var_i," <- mean(datos_sel[datos_sel$Win == 0]$",var_i,", na.rm = T)} else {input$m_l_",var_i," <- 0}")))
          
          #eval(parse(text=paste0("if (nrow(datos_sel[datos_sel$Win == 1])>0) {input$sd_w_",var_i," <- sd(datos_sel[datos_sel$Win == 1]$",var_i,", na.rm = T)} else {input$sd_w_",var_i," <- 0}")))
          #eval(parse(text=paste0("if (nrow(datos_sel[datos_sel$Win == 0])>0) {input$sd_l_",var_i," <- sd(datos_sel[datos_sel$Win == 0]$",var_i,", na.rm = T)} else {input$sd_l_",var_i," <- 0}")))
          
          # Ultimos partidos
          
          eval(parse(text=paste0("input$m_",num_partidos,var_i,"<-mean(datos_10$",var_i,",na.rm=T)")))
          #eval(parse(text=paste0("if (nrow(datos_10)>0) {input$sd_",num_partidos,var_i," <- sd(datos_10$",var_i,", na.rm = T)} else {input$sd_",num_partidos,var_i," <- 0}")))
          
          #eval(parse(text=paste0("if (nrow(datos_10[datos_10$Win == 1])>0) {input$m_w_",num_partidos,var_i," <- mean(datos_10[datos_10$Win == 1]$",var_i,", na.rm = T)} else {input$m_w_",num_partidos,var_i," <- 0}")))
          #eval(parse(text=paste0("if (nrow(datos_10[datos_10$Win == 0])>0) {input$m_l_",num_partidos,var_i," <- mean(datos_10[datos_10$Win == 0]$",var_i,", na.rm = T)} else {input$m_l_",num_partidos,var_i," <- 0}")))
          
          #eval(parse(text=paste0("if (nrow(datos_10[datos_10$Win == 1])>0) {input$sd_w_",num_partidos,var_i," <- sd(datos_10[datos_10$Win == 1]$",var_i,", na.rm = T)} else {input$sd_w_",num_partidos,var_i," <- 0}")))
          #eval(parse(text=paste0("if (nrow(datos_10[datos_10$Win == 0])>0) {input$sd_l_",num_partidos,var_i," <- sd(datos_10[datos_10$Win == 0]$",var_i,", na.rm = T)} else {input$sd_l_",num_partidos,var_i," <- 0}")))
          
          
          for (sup in surface){
            datos_s <- datos_jugador[datos_jugador$tourney_surface == sup]
            datos_10_s <- datos_s[(i):min(i+num_partidos,nrow(datos_s)),]
            # Superficie
            
            eval(parse(text=paste0("input$m_",sup,"_",var_i,"<-mean(datos_s$",var_i,",na.rm=T)")))
            #eval(parse(text=paste0("if (nrow(datos_s)>0) {input$sd_",sup,"_",var_i," <- sd(datos_s$",var_i,", na.rm = T)} else {input$sd_",sup,"_",var_i," <- 0}")))
            
            #eval(parse(text=paste0("if (nrow(datos_s[datos_s$Win == 1])>0) {input$m_w_",sup,"_",var_i," <- mean(datos_s[datos_s$Win == 1]$",var_i,", na.rm = T)} else {input$m_w_",sup,"_",var_i," <- 0}")))
            #eval(parse(text=paste0("if (nrow(datos_s[datos_s$Win == 0])>0) {input$m_l_",sup,"_",var_i," <- mean(datos_s[datos_s$Win == 0]$",var_i,", na.rm = T)} else {input$m_l_",sup,"_",var_i," <- 0}")))
            
            #eval(parse(text=paste0("if (nrow(datos_s[datos_s$Win == 1])>0) {input$sd_w_",sup,"_",var_i," <- sd(datos_s[datos_s$Win == 1]$",var_i,", na.rm = T)} else {input$sd_w_",sup,"_",var_i," <- 0}")))
            #eval(parse(text=paste0("if (nrow(datos_s[datos_s$Win == 0])>0) {input$sd_l_",sup,"_",var_i," <- sd(datos_s[datos_s$Win == 0]$",var_i,", na.rm = T)} else {input$sd_l_",sup,"_",var_i," <- 0}")))
            
            # Superficie ?ltimos partidos
            
            eval(parse(text=paste0("input$m_",num_partidos,"_",sup,"_",var_i,"<-mean(datos_10_s$",var_i,",na.rm=T)")))
            #eval(parse(text=paste0("if (nrow(datos_10_s)>0) {input$sd_",num_partidos,"_",sup,"_",var_i," <- sd(datos_10_s$",var_i,", na.rm = T)} else {input$sd_",num_partidos,"_",sup,"_",var_i," <- 0}")))
            
            #eval(parse(text=paste0("if (nrow(datos_10_s[datos_10_s$Win == 1])>0) {input$m_w_",num_partidos,"_",sup,"_",var_i," <- mean(datos_10_s[datos_10_s$Win == 1]$",var_i,", na.rm = T)} else {input$m_w_",num_partidos,"_",sup,"_",var_i," <- 0}")))
            #eval(parse(text=paste0("if (nrow(datos_10_s[datos_10_s$Win == 0])>0) {input$m_l_",num_partidos,"_",sup,"_",var_i," <- mean(datos_10_s[datos_10_s$Win == 0]$",var_i,", na.rm = T)} else {input$m_l_",num_partidos,"_",sup,"_",var_i," <- 0}")))
            
            #eval(parse(text=paste0("if (nrow(datos_10_s[datos_10_s$Win == 1])>0) {input$sd_w_",num_partidos,"_",sup,"_",var_i," <- sd(datos_10_s[datos_10_s$Win == 1]$",var_i,", na.rm = T)} else {input$sd_w_",num_partidos,"_",sup,"_",var_i," <- 0}")))
            #eval(parse(text=paste0("if (nrow(datos_10_s[datos_10_s$Win == 0])>0) {input$sd_l_",num_partidos,"_",sup,"_",var_i," <- sd(datos_10_s[datos_10_s$Win == 0]$",var_i,", na.rm = T)} else {input$sd_l_",num_partidos,"_",sup,"_",var_i," <- 0}")))
            
          }
          
          
        }
        datos_nuevas_variables <- rbind(datos_nuevas_variables,input)
      }

      }
    contador <- contador +1
    setTxtProgressBar(pb, contador)
  }
  return(datos_nuevas_variables)
  }


calcula_fecha <- function(input){
  
  date <- seq(input[[1]],input[[2]],length.out=input[[3]])[input[[4]]]
  return(as.character(as.Date(as.character(date), format = "%Y-%m-%d")))
}


Walkout_function <- function(input){
  # Definimos por defecto que no hay retirada.

  # Definimos a priori el número de sets jugados como el número de sets que no
  # sean NA.
  sets_played <- sum(!is.na(input[1:5]))
  # Miramos si ese valor es al menos 2.
    # En cualquier caso marcamos el número de sets jugados como NA.
  
    
  # Inicializamos las variables de juegos jugados y ganados por jugador.
  w_num_juegos <- 0
  l_num_juegos <- 0
  t_num_juegos <- 0
  # Comprobamos si el número de sets jugados es NA o no.
  if (!is.na(sets_played)) {
    # Si no es NA definimos al mínimo entre ese número y el número máximo
    # posible.
    maximum <- min(sets_played, as.numeric(input[6]))
    # Formamos una lista desde ese número a 1.
    seq <- seq(maximum,1,-1)
    # Hacemos un bucle respecto a eso:
    for(i in seq) {
      # Obtenemos la puntuación del set sin corchetes de tenerlos.
      input_wo_cor <- gsub(gsub(input[i], pattern = "[", replace = "",
                                fixed = TRUE), pattern = "]",
                           replace = "", fixed = TRUE)
      # Comprobamos si el valor del sets es una puntuación legal.
      if ((grepl("[[:digit:]]-[[:digit:]]",input[i])) |
          (grepl("[[:digit:]]-[[:digit:]]",input_wo_cor))) {
        # De ser así, comprobamos si realmente está entre corchete.
        if (input[i] == gsub(input[i], pattern = "[", replace = "",
                             fixed = TRUE)) {
          # De ser así, obtenemos las puntuaciones de cada jugador.
          num <- c()
          num[1] <- as.numeric(strsplit(input[i],
                                        split = "-")[[1]][1])
          num[2] <- as.numeric(strsplit(strsplit(input[i],
                                                 split = "-")[[1]][2],
                                        split = "(", fixed = TRUE)[[1]][1])
          # Inicializamos las variables índice de jugador ganador o perdedor.
          if(exists("win")){win <- win}else{win <- 0}
          if(exists("lose")){lose <- lose}else{lose <- 0}
          
          # En caso de ser el último set jugado entramos en esta condición.
          if (i == maximum) {
            # Comprobamos cuál de los dos números es el mayor para definir
            # cuáles son los puntos del ganador y cuáles son del perdedor.
            if (num[1] >= num[2]) {
              win <- 1
              lose <- 2
              } else {
                win <- 2
                lose <- 1
                }
          }
          # Comprobamos si los índices se han rellenado.
          if (win == 0) {
             # En caso de no estarlo (porque el set no tenía una puntuación
             # válida) bajamos en 1 el valor del máximo para que lo compruebe
             # en la siguiente iteración.
             maximum <- maximum - 1
          } else {
             # Caso de estarlo sumamos esos valores al número de juegos ganados
             #  por cada jugador.
             w_num_juegos <- w_num_juegos + num[win]
             l_num_juegos <- l_num_juegos + num[lose]
          }
          } else {
             # En caso de ser una puntuación entre corchetes (debido a que se
             # trata de un partido de )
             # le sumamos 6 al ganador y 3 al perdedor como valores medios.
             w_num_juegos <- w_num_juegos + 6
             l_num_juegos <- l_num_juegos + 3
             }
        } else {
          # Si el valor no es una puntuación legal salimos del bucle.
          break
          }
      }
    } else {
       # En caso de ser NA al menos comprobamos si el primer set está relleno
       # con una puntuación real o no.
       if (!is.na(input[1]) & (input[1] != "RET") &
           (input[1] != "W/O") & (input[1] != "ABN") &
           (input[1] != "DEF") & (input[1] != "Walkover")) {
          # En caso de estarlo añadimos esos valores al número de juegos
          # jugados.
          num <- c()
          num[1] <- as.numeric(strsplit(input[1], split = "-")[[1]][1])
          num[2] <- as.numeric(strsplit(strsplit(input[1], split = "-")[[1]][2],
                                        split = "(", fixed = TRUE)[[1]][1])
          w_num_juegos <- max(num)
          l_num_juegos <- min(num)
       }
    }
  # Obtenemos el número de juegos jugados sumando ambos.
  t_num_juegos <- w_num_juegos + l_num_juegos
  # Reunimos todas las variables en la variable salida.
  output <- c(sets_played, t_num_juegos, w_num_juegos, l_num_juegos)
  # Devolvemos la salida.
  return(output)
}


################################################################################
#                 OBTENCIÓN VARIABLES ATEMPORALES JUGADORES                    #
################################################################################
#                                                                              #
# Objetivo: Obtención de variables atemporales de los jugadores a partir de    #
#           las iniciales filtrando por jugador, año (pero sólo registros      #
#           anteriores al dado) y por superficies en algunas variables.        #
#                                                                              #
# Input:  Las variables necesarias de la tabla "atp_players" para obtener      #
#         las variables atemporales que queremos.                              #
#                                                                              #
# Output: Variables atemporales los jugadores:                                 #
#                                                                              #
#           - Número de partidos jugados en los últimos 15 días (dentro del    #
#           mismo año).                                                        #
#         Las siguientes se obtienen tanto en general como por superficie,     #
#         como de los últimos 10 partidos del año en cuestión, como de los     #
#         últimos 10 partidos del año en cuestión por superficie:              #
#           - Porcentaje de victoria.                                          #
#           - Porcentaje de break points salvados.                             #
#           - Medias de aces.                                                  #
#           - Porcentaje de primeros servicios ganados.                        #
#           - Media de dobles faltas.                                          #
#           - Media de puntos de ranking del rival en los partidos ganados.    #
#           - Media de puntos de ranking del rival en los partidos perdidos.   #
#           - Desviación típica de puntos de ranking del rival en los partidos #
#           ganados.                                                           #
#           - Desviación típica de puntos de ranking del rival en los partidos #
#           perdidos.                                                          #
#                                                                              #
################################################################################

Players_function <- function(input) {
  
  ace <- input[8]
  df <- input[9]
  svpt <- input[10]
  `1stIn` <- input[11]
  `1stWon` <- input[12]
  `2ndWon` <- input[13]
  SvGms <- input[14]
  bpSaved <- input[15]
  bpFaced <- input[16]
  minutes <- input[17]
  ht <- input[18]
  n_played_matches_15 <- 0
  win_temp <- 0
  p_temp_bp <- 0
  m_aces <- 0
  p_1won <- 0
  m_df <- 0
  m_atp_point_w <- 0
  m_atp_point_l <- 0
  sd_atp_point_w <- 0
  sd_atp_point_l <- 0
  win_temp_s <- 0
  p_temp_bp_s <- 0
  m_aces_s <- 0
  p_1won_s <- 0
  m_df_s <- 0
  m_atp_point_w_s <- 0
  m_atp_point_l_s <- 0
  sd_atp_point_w_s <- 0
  sd_atp_point_l_s <- 0
  win_10 <- 0
  p_bp_10 <- 0
  m_aces_10 <- 0
  p_1won_10 <- 0
  m_df_10 <- 0
  m_atp_point_w_10 <- 0
  m_atp_point_l_10 <- 0
  sd_atp_point_w_10 <- 0
  sd_atp_point_l_10 <- 0
  win_10_s <- 0
  p_bp_10_s <- 0
  m_aces_10_s <- 0
  p_1won_10_s <- 0
  m_df_10_s <- 0
  m_atp_point_w_10_s <- 0
  m_atp_point_l_10_s <- 0
  sd_atp_point_w_10_s <- 0
  sd_atp_point_l_10_s <- 0
  
  datos <- atp_players[atp_players$id == input[1] &
                         atp_players$year == input[2] &
                         ((atp_players$month < input[3]) |
                            ((atp_players$month == input[3]) &
                               (atp_players$day < input[4])) |
                            ((atp_players$month == input[3]) &
                               (atp_players$day == input[4]) &
                               (atp_players$round <= input[5])))]
  
  setorder(datos, -time, -round)
  
  datos_10 <- datos[1:min(10,nrow(datos)),]
  
  datos_s <- datos[datos$surface == input[7]]
  
  datos_10_s <- datos_s[1:min(10,nrow(datos_s)),]
  
  # Imputación datos faltantes:
  
  if (is.na(input[8]) | is.nan(input[8])) {
    ace <- mean(datos_s$ace, na.rm = T)
    if (is.na(ace) | is.nan(ace)) {
      ace <- mean(datos$ace, na.rm = T)
    }
  }
  if (is.na(input[9]) | is.nan(input[9])) {
    df <- mean(datos_s$df, na.rm = T)
    if (is.na(df) | is.nan(df)) {
      df <- mean(datos$df, na.rm = T)
    }
  }
  if (is.na(input[10]) | is.nan(input[10])) {
    svpt <- mean(datos_s$svpt, na.rm = T)
    if (is.na(svpt) | is.nan(svpt)) {
      svpt <- mean(datos$svpt, na.rm = T)
    }
  }
  if (is.na(input[11]) | is.nan(input[11])) {
    `1stIn` <- mean(datos_s$`1stIn`, na.rm = T)
    if (is.na(`1stIn`) | is.nan(`1stIn`)) {
      `1stIn` <- mean(datos$`1stIn`, na.rm = T)
    }
  }
  if (is.na(input[12]) | is.nan(input[12])) {
    `1stWon` <- mean(datos_s$`1stWon`, na.rm = T)
    if (is.na(`1stWon`) | is.nan(`1stWon`)) {
      `1stWon` <- mean(datos$`1stWon`, na.rm = T)
    }
  }
  if (is.na(input[13]) | is.nan(input[13])) {
    `2ndWon` <- mean(datos_s$`2ndWon`, na.rm = T)
    if (is.na(`2ndWon`) | is.nan(`2ndWon`)) {
      `2ndWon` <- mean(datos$`2ndWon`, na.rm = T)
    }
  }
  if (is.na(input[14]) | is.na(input[14])) {
    SvGms <- mean(datos_s$SvGms, na.rm = T)
    if (is.na(SvGms) | is.nan(SvGms)) {
      SvGms <- mean(datos$SvGms, na.rm = T)
    }
  }
  if (is.na(input[15]) | is.na(input[15])) {
    bpSaved <- mean(datos_s$bpSaved, na.rm = T)
    if (is.na(bpSaved) | is.nan(bpSaved)) {
      bpSaved <- mean(datos$bpSaved, na.rm = T)
    }
  }
  if (is.na(input[16]) | is.nan(input[16])) {
    bpFaced <- mean(datos_s$bpFaced, na.rm = T)
    if (is.na(bpFaced) | is.nan(bpFaced)) {
      bpFaced <- mean(datos$bpFaced, na.rm = T)
    }
  }
  if (is.na(input[17]) | is.nan(input[17])) {
    minutes <- mean(datos_s$minutes, na.rm = T)
    if (is.na(minutes) | is.nan(minutes)) {
      minutes <- mean(datos$minutes, na.rm = T)
    }
  }
  if (is.na(input[18]) | is.nan(input[18])) {
    ht <- mean(datos_s$ht, na.rm = T)
    if (is.na(ht) | is.nan(ht)) {
      ht <- mean(datos$ht, na.rm = T)
    }
  }
  
  # Partidos jugados últimos 15 días
  
  datos_sel <- datos[as.Date(datos$time) >= (as.Date(input[6]) - 15)]
  n_played_matches_15 <- nrow(datos_sel)
  
  # temporada
  
  win_temp <- mean(datos$Win,na.rm = T)
  p_temp_bp <- mean(datos$bpSaved[datos$bpFaced!=0] /
                     datos$bpFaced[datos$bpFaced!=0], na.rm = T)
  m_aces <- mean(datos$ace, na.rm = T)
  p_1won <- mean(datos$`1stWon`[datos$`1stIn`!=0] /
                  datos$`1stIn`[datos$`1stIn`!=0], na.rm = T)
  m_df <- mean(datos$df, na.rm=T)
  m_atp_point_w <- mean(datos[datos$Win == 1]$rank_points, na.rm = T)
  if (is.nan(m_atp_point_w)){
    m_atp_point_w <- quantile(datos$rank_points, probs = c(0.1), na.rm = TRUE)
  }
  m_atp_point_l <- mean(datos[datos$Win == 0]$rank_points, na.rm = T)
  if (is.nan(m_atp_point_l)){
    m_atp_point_l <- quantile(datos$rank_points, probs = c(0.9), na.rm = TRUE)
  }
  if (nrow(datos[datos$Win == 1]) > 1) {
    sd_atp_point_w <- sd(datos[datos$Win == 1]$rank_points, na.rm = T)
  } else {
    sd_atp_point_w <- 0
  }
  if (nrow(datos[datos$Win == 0]) > 1) {
    sd_atp_point_l = sd(datos[datos$Win == 0]$rank_points, na.rm = T)
  } else {
    sd_atp_point_l = 0
  }
  
  
  # superficie
  
  win_temp_s <- mean(datos_s$Win,na.rm = T)
  p_temp_bp_s <- mean(datos_s$bpSaved[datos_s$bpFaced!=0] /
                       datos_s$bpFaced[datos_s$bpFaced!=0],na.rm = T)
  m_aces_s <- mean(datos_s$ace, na.rm = T)
  p_1won_s <- mean(datos_s$`1stWon`[datos_s$`1stIn`!=0] /
                    datos_s$`1stIn`[datos_s$`1stIn`!=0], na.rm = T)
  m_df_s <- mean(datos_s$df, na.rm=T)
  m_atp_point_w_s <- mean(datos_s[datos_s$Win == 1]$rank_points, na.rm = T)
  if (is.nan(m_atp_point_w_s)){
    m_atp_point_w_s <- quantile(datos_s$rank_points, probs = c(0.1), na.rm = TRUE)
  }
  m_atp_point_l_s <- mean(datos_s[datos_s$Win == 0]$rank_points, na.rm = T)
  if (is.nan(m_atp_point_l_s)) {
    m_atp_point_l_s <- quantile(datos_s$rank_points, probs = c(0.9), na.rm = TRUE)
  }
  if (nrow(datos_s[datos_s$Win == 1]) > 1) {
    sd_atp_point_w_s <- sd(datos_s[datos_s$Win == 1]$rank_points, na.rm = T)
  } else {
    sd_atp_point_w_s <- 0
  }
  if (nrow(datos_s[datos_s$Win == 0]) > 1) {
    sd_atp_point_l_s <- sd(datos_s[datos_s$Win == 0]$rank_points, na.rm = T)
  } else {
    sd_atp_point_l_s <- 0
  }
  
  
  # 10 últimos partidos
  
  win_10 <- mean(datos_10$Win,na.rm = T)
  p_bp_10 <- mean(datos_10$bpSaved[datos_10$bpFaced!=0] /
                   datos_10$bpFaced[datos_10$bpFaced!=0], na.rm = T)
  m_aces_10 <- mean(datos_10$ace, na.rm = T)
  p_1won_10 <- mean(datos_10$`1stWon`[datos_10$`1stIn`!=0] /
                     datos_10$`1stIn`[datos_10$`1stIn`!=0], na.rm = T)
  m_df_10 <- mean(datos_10$df, na.rm=T)
  m_atp_point_w_10 <- mean(datos_10[datos_10$Win == 1]$rank_points, na.rm = T)
  if (is.nan(m_atp_point_w_10)){
    m_atp_point_w_10 <- quantile(datos_10$rank_points, probs = c(0.1),
                                 na.rm = TRUE)
  }
  m_atp_point_l_10 <- mean(datos_10[datos_10$Win == 0]$rank_points, na.rm = T)
  if (is.nan(m_atp_point_l_10)) {
    m_atp_point_l_10 <- quantile(datos_10$rank_points, probs = c(0.9),
                                 na.rm = TRUE)
  }
  if (nrow(datos_10[datos_10$Win == 1]) > 1) {
    sd_atp_point_w_10 <- sd(datos_10[datos_10$Win == 1]$rank_points, na.rm = T)
  } else {
    sd_atp_point_w_10 <- 0
  }
  if (nrow(datos_10[datos_10$Win == 0]) > 1) {
    sd_atp_point_l_10 <- sd(datos_10[datos_10$Win == 0]$rank_points, na.rm = T)
  } else {
    sd_atp_point_l_10 <- 0
  }
  
  # 10 últimos partidos por superficie
  
  win_10_s <- mean(datos_10_s$Win,na.rm = T)
  p_bp_10_s <- mean(datos_10_s$bpSaved[datos_10_s$bpFaced!=0] /
                     datos_10_s$bpFaced[datos_10_s$bpFaced!=0], na.rm = T)
  m_aces_10_s <- mean(datos_10_s$ace, na.rm = T)
  p_1won_10_s <- mean(datos_10_s$`1stWon`[datos_10_s$`1stIn`!=0] /
                       datos_10_s$`1stIn`[datos_10_s$`1stIn`!=0], na.rm = T)
  m_df_10_s <- mean(datos_10_s$df, na.rm=T)
  m_atp_point_w_10_s <- mean(datos_10_s[datos_10_s$Win == 1]$rank_points,
                            na.rm = T)
  if (is.nan(m_atp_point_w_10_s)){
    m_atp_point_w_10_s <- quantile(datos_10_s$rank_points, probs = c(0.1),
                                   na.rm = TRUE)
  }
  m_atp_point_l_10_s <- mean(datos_10_s[datos_10_s$Win == 0]$rank_points,
                            na.rm = T)
  if (is.nan(m_atp_point_l_10_s)){
    m_atp_point_l_10_s <- quantile(datos_10_s$rank_points, probs = c(0.9),
                                   na.rm = TRUE)
  }
  if (nrow(datos_10_s[datos_10_s$Win == 1]) > 1) {
    sd_atp_point_w_10_s <- sd(datos_10_s[datos_10_s$Win == 1]$rank_points,
                             na.rm = T)
  } else {
    sd_atp_point_w_10_s <- 0
  }
  if (nrow(datos_10_s[datos_10_s$Win == 0]) > 1) {
    sd_atp_point_l_10_s <- sd(datos_10_s[datos_10_s$Win == 0]$rank_points,
                             na.rm = T)
  } else {
    sd_atp_point_l_10_s <- 0
  }
  
    output <- c(ace, df, svpt, `1stIn`, `1stWon`, `2ndWon`,
                SvGms, bpSaved, bpFaced, minutes, ht,
                n_played_matches_15,
                win_temp, p_temp_bp, m_aces, p_1won, m_df,
                m_atp_point_w, m_atp_point_l,
                sd_atp_point_w, sd_atp_point_l,
                win_temp_s, p_temp_bp_s, m_aces_s, p_1won_s, m_df_s,
                m_atp_point_w_s, m_atp_point_l_s,
                sd_atp_point_w_s, sd_atp_point_l_s,
                win_10, p_bp_10, m_aces_10, p_1won_10, m_df_10,
                m_atp_point_w_10, m_atp_point_l_10,
                sd_atp_point_w_10, sd_atp_point_l_10,
                win_10_s, p_bp_10_s, m_aces_10_s, p_1won_10_s, m_df_10_s,
                m_atp_point_w_10_s, m_atp_point_l_10_s,
                sd_atp_point_w_10_s, sd_atp_point_l_10_s)
                
  return(output)
}


################################################################################
#                      VALIDACIÓN DEL MODELO GENERADO                          #
################################################################################
#                                                                              #
# Objetivo: Validación los distintos modelos generados.                        #
#                                                                              #
# Input:  - Vector con los valores reales del test.                            #
#         - Vector con las predicciones realizadas en el conjunto test.        #
#                                                                              #
# Salida: Tabla con los siguientes apartados:                                  #
#                                                                              #
#           - % Acierto global.                                                #
#           - % acierto marginal (Sensitividad y especificidad).               #
#           - Curva ROC y AUC.                                                 #
#                                                                              #
################################################################################

# Librerías

if (!require("ROCR")){install.packages("ROCR")}else{require("ROCR")}

# Validation 

validacion <- function(test, modelo){
   library(ROCR)
   
   probabilities_full <- modelo %>% predict(test, type = "response",
                                            na.action = na.omit)
   predicted.classes_full <- ifelse(probabilities_full > 0.5, 1, 0)
   # Prediction accuracy
   observed.classes_full <- test$Win
   
   valid <- table(pred=predicted.classes_full, real = observed.classes_full)
   
   Perc_acierto <- sum(diag(valid))/sum(valid)
   
   Perc_acierto_marginal<-diag(prop.table(valid, 1))
   
   probabi <- predict(modelo, test, type="response")
   prediobj <- prediction(probabi, test$Win)
   plot(performance(prediobj, "tpr","fpr"), main="CURVA COR TEST")
   abline(a=0,b=1,col="blue",lty=2)
   auc <- as.numeric(performance(prediobj,"auc")@y.values)
   legend("bottomright",legend=paste("AUC = ",round(auc,3)))
   
   sensitividad <- valid[2,2]/sum(valid[,2])
   especificidad <- (valid[1,1]/sum(valid[,1]))
   resultados <- as.data.table(cbind(Fecha= as.character(Sys.time()),
                                     P_Acierto = round(Perc_acierto,4),
                                     Sensitividad=round(sensitividad,4),
                                     Especificidad=round(especificidad,4),
                                     AUC = round(auc,2)))
   return(resultados)
}


################################################################################
#                             ALGORITMO DE KELLY                               #
################################################################################
#                                                                              #
# Objetivo: Creación del algoritmo de Kelly.                                   #
#                                                                              #
# Input:  - Cuota jugador A.                                                   #
#         - Cuota jugador B.                                                   #                                  
#         - Probabilidad de victoria modelo R.                                 #
#         - Cantidad de dinero disponible.                                     #
#                                                                              #
# Salida: - Jugador al que apostar.                                            #   
#         - Dinero a apostar.                                                  #
#                                                                              #
################################################################################

cuotaa <- 1.16
cuotab <- 4.50
proba <- 0.8
probb <- 0.2


AlgoritmoKelly <- function(input){
  
  nombrea <- as.character(input[1])
  nombreb <- as.character(input[2])
  cuotaa <- as.numeric(input[3])
  cuotab <- as.numeric(input[4])
  proba <- as.numeric(input[5])
  flag <- as.integer(input[6])
   
   
   probb<- 1-proba
  
   prob_ca<- cuotab/(cuotaa+cuotab)
   
   prob_cb <- cuotaa/(cuotaa+cuotab)
   
   
   
   #prob_ca_new <- prob_ca/(prob_ca+prob_cb)
   
   #prob_cb_new <- prob_cb/(prob_ca+prob_cb)
   
   
   # cuota_verdadera_a <- 1/prob_ca_new
   # 
   # cuota_verdadera_b <- 1/prob_cb_new
   
   # Diferencia entre probabilidades
   
   diferencia_a <- proba - prob_ca
   
   # Caso 1 
   
   if( diferencia_a > 0.15){
      
      #porc_dinero <- max(0,((cuotaa-1)*proba-probb)/(cuotaa-1)*100)
      #cat("Apostar ", porc_dinero,"% al Jugador A")
      nombre <- nombrea
      cuota <- cuotaa
      prob <- proba
      flag_aux <- 1
   }else if(diferencia_a < -0.15){
      #porc_dinero <- max(0,((cuotab-1)*probb-proba)/(cuotab-1)*100)
      #cat("Apostar ", porc_dinero,"% al Jugador B")
      nombre <- nombreb
      cuota <- cuotab
      prob <- probb
      flag_aux <- 2
   }else if(min(proba,prob_ca) > 0.7){
     #porc_dinero <- max(0,((cuotaa-1)*proba-probb)/(cuotaa-1)*100)
     #cat("Apostar ", porc_dinero,"% al Jugador A")
     nombre <- nombrea
     cuota <- cuotaa
     prob <- proba
     flag_aux <- 1
   }else if (max(proba,prob_ca) < 0.3){
     #porc_dinero <- max(0,((cuotab-1)*probb-proba)/(cuotab-1)*100)
     #cat("Apostar ", porc_dinero,"% al Jugador B")
     nombre <- nombreb
     cuota <- cuotab
     prob <- probb
     flag_aux <- 2
   }else{
     #cat("No apostar")
     nombre <- NA
     cuota <- NA
     prob <- NA
     flag_aux <- NA
   }
   
   if (!is.na(flag_aux)) {
     if (flag_aux == 1) {
       output_flag <- flag
     } else {
       if (flag == 1) {
         output_flag <- 2
       } else {
         output_flag <- 1
       }
     }
   } else {
     output_flag <- NA
   }
   
   
   output <- c(nombre, cuota, prob, output_flag)
   
  return(output)
   
   
}

####################################################################################
#                                   SIMPLEX ALGORITHM                              #               
####################################################################################


simplex_optimization <- function(jugador, cuota, prob, N, lambda, nivel_riesgo){
  
  # browser()

  #Paquetes necesarios
  
  if (!require("boot")){install.packages("boot")}else{require("boot")}
  
  # Condiciones que terminan el cálculo
  
  if(nrow(jugador)!=nrow(cuota) || nrow(jugador)!=nrow(prob)){
   stop("Los parámetros de entrada no tienen la misma longitud") 
  }
  if(N<=0){stop("Introduce una cantidad de dinero mayor que cero")}
  
  # Penalización por riesgo
  prob_aux <- c()
  for (i in 1:nrow(prob)){
    if(prob[i,1]<=nivel_riesgo){
      prob_aux[i]<-prob[i,1]/lambda
    }else{prob_aux[i]<- prob[i,1]}
  }
  
  
  # Construcción de los parámetros de entrada  
  
    # Función a maximizar
  
    a <-  as.vector(unlist(cuota*prob_aux))
  
    # Restricciones menor o igual
    
    A1 <- diag(rep(1, nrow(jugador)))
  
    b1 <-as.vector(unlist(prob_aux*N))
  
    # Restricciones mayor o igual
    
    A2 <- diag(rep(1, nrow(jugador)))
    
    b2 <- as.vector(prob_aux*(1/nrow(jugador))*N)
    
    # Restricciones iguales
  
    A3 <- as.vector(rep(1,nrow(jugador)))
    
    b3 <- N
    
  # Optimización
    
    res_apuesta <- simplex(a,A1,b1,A2,b2,A3,b3, maxi=T)
    
    x_i <- res_apuesta$soln
    
    dinero_esperado <- sum(prob*x_i*cuota)
    
    
    resultado_final <- as.data.frame(cbind(jugador, prob, cuota, round(x_i,2),round(cuota*x_i,2),round(cuota*x_i - x_i,2)))
    
    colnames(resultado_final) <- c("Jugador","Probabilidad", "Cuota", "Dinero_Apostar","Dinero_ganar","Ganancia")
    
    dinero_maximo <- round(sum(resultado_final$Ganancia), 2)
    
    cat("La ganancia esperada es de ", dinero_esperado - N,"\n","\n")
    
    cat("La ganancia maxima es de ", dinero_maximo,"\n","\n")
    
    cat("La rentabilidad media de la apuesta es: ", round((dinero_esperado-N)/N*100,2),"%","\n","\n")
    cat("La rentabilidad maxima de la apuesta es: ", round(dinero_maximo/N*100,2),"%","\n","\n")
    return(resultado_final)
}









pred_prob <- function(input) {
  
  p1 <- as.character(input[1])
  p2 <- as.character(input[2])
  surface_m <- as.character(input[3])
  tourney_level_m <- as.character(input[4])
  round_m <- as.character(input[5])
  cuota_p1 <- as.numeric(input[6])
  cuota_p2 <- as.numeric(input[7])
  
  a<-datos_modelado[datos_modelado$name_p1 %in% c(p1) |datos_modelado$name_p2 %in% c(p1),]
  a_prima <- datos_modelado[(datos_modelado$name_p1 %in% c(p1) |datos_modelado$name_p2 %in% c(p1)) & datos_modelado$surface == surface_m,]
  b<-datos_modelado[datos_modelado$name_p1 %in% c(p2) |datos_modelado$name_p2 %in% c(p2),]
  b_prima <- datos_modelado[(datos_modelado$name_p1 %in% c(p2) |datos_modelado$name_p2 %in% c(p2)) & datos_modelado$surface == surface_m,]
  setorder(a,-year,-month,-day,-round)
  setorder(b,-year,-month,-day,-round)
  setorder(a_prima,-year,-month,-day,-round)
  setorder(b_prima,-year,-month,-day,-round)
  
  cols_superficie <- names(a)[grepl("_s_",names(a))]
  (names(a) %in% cols_superficie)
  a <- data.frame(a)
  b <- data.frame(b)
  a_prima <- data.frame(a_prima)
  b_prima <- data.frame(b_prima)
  a[1,(names(a) %in% cols_superficie)] <- a_prima[1,(names(a) %in% cols_superficie)]
  b[1,(names(b) %in% cols_superficie)] <- b_prima[1,(names(b) %in% cols_superficie)]
  
  c_1 <- a[1,]
  c_2 <- a[1,]
  
  if (a$name_p1[1] == p1) {
    c_1[1,8:47] <- a[1,8:47]
    c_2[1,48:87] <- a[1,8:47]
  } else{
    c_1[1,8:47] <- a[1,48:87]
    c_2[1,48:87] <- a[1,48:87]
  }
  
  if (b$name_p1[1] == p2) {
    c_1[1,48:87] <- b[1,8:47]
    c_2[1,8:47] <- b[1,8:47]
  } else{
    c_1[1,48:87] <- b[1,48:87]
    c_2[1,8:47] <- b[1,48:87]
  }
  
  c_1$surface <- factor(surface_m, levels = levels(datos_modelado$surface))
  c_1$tourney_level <- factor(tourney_level_m, levels = levels(datos_modelado$tourney_level))
  c_1$round <- factor(round_m,levels = levels(datos_modelado$round))
  #c_1$t_num_juegos <- round(mean(a[1,]$t_num_juegos,b[1,]$t_num_juegos))
  c_1 <- c_1 %>% dplyr::select(-c(month,day,time,name_p1,name_p2))
  
  c_2$surface <- factor(surface_m, levels = levels(datos_modelado$surface))
  c_2$tourney_level <- factor(tourney_level_m, levels = levels(datos_modelado$tourney_level))
  c_2$round <- factor(round_m,levels = levels(datos_modelado$round))
  #c_2$t_num_juegos <- round(mean(a[1,]$t_num_juegos,b[1,]$t_num_juegos))
  c_2 <- c_2 %>% dplyr::select(-c(month,day,time,name_p1,name_p2))
  
  prediccion_1 <- predict(object = modelo_boost, newdata = c_1,
                          n.trees = best_arb_boost, type = "response")
  
  prediccion_2 <- predict(object = modelo_boost, newdata = c_2,
                          n.trees = best_arb_boost, type = "response")
  
  # if (prediccion_1*cuota_p1 >= prediccion_2*cuota_p2) {
  if (prediccion_1 >= prediccion_2) {
    nombre_winner <- p1
    nombre_loser <- p2
    pred_1 <- prediccion_1
    pred_2 <- 1 - prediccion_2
    flag <- 1
  } else {
    nombre_winner <- p2
    nombre_loser <- p1
    pred_1 <- 1 - prediccion_1
    pred_2 <- prediccion_2
    flag <- 2
  }
  
  prob_winner <- round(mean(c(pred_1, pred_2)),4)
  
  desv_probs <- round(1 - (prediccion_1 + prediccion_2),6)
  
  hoy <- as.Date(Sys.time(),"%Y-%m-%d")
  fecha_a <-as.Date(a$time[1],"%Y-%m-%d")
  fecha_b <-as.Date(b$time[1],"%Y-%m-%d")
  
  if((hoy - fecha_a) > 60 | (hoy - fecha_b) > 60){
    aplica <- "NO"
  }else{
    aplica <- "SI"
  }
  
  
  output <- c(nombre_winner, flag, prob_winner, desv_probs, nombre_loser,aplica)
  
  return(output)
  
}

sel_cuota <- function(input) {
  
  nombre_p1 <- as.character(input[1])
  nombre_p2 <- as.character(input[2])
  flag <- as.integer(input[3])
  cuota_p1 <- as.numeric(input[4])
  cuota_p2 <- as.numeric(input[5])
  
  if (flag == 1) {
    nombre_a <- nombre_p1
    nombre_b <- nombre_p2
    cuota_a <- cuota_p1
    cuota_b <- cuota_p2
  } else {
    nombre_a <- nombre_p2
    nombre_b <- nombre_p1
    cuota_a <- cuota_p2
    cuota_b <- cuota_p1
  }
  
  data_cuotas <- c(nombre_a, nombre_b, cuota_a, cuota_b)
  
  return(data_cuotas)
  
}
### kniznice
library(readr)
#
### nacitanie dat
dataset <- read_delim("D:/PRACA/DATABAZA_MRTVYCH/databaza_mrtvych_v_3_1/databases/mortality.csv",
                      ";", escape_double = FALSE, trim_ws = TRUE)
data <- as.data.frame(dataset)
cols <- c(1,2,3,5:14)
data[cols] <- lapply(data[cols], factor)
data$age_category <- ifelse(data$age == 0, 1,
                            ifelse(data$age > 0 & data$age < 5, 2,
                                   ifelse(data$age > 4 & data$age < 10, 3,
                                          ifelse(data$age > 9 & data$age < 15, 4,
                                                 ifelse(data$age > 14 & data$age < 20, 5,
                                                        ifelse(data$age > 19 & data$age < 25, 6,
                                                               ifelse(data$age > 24 & data$age < 30, 7,
                                                                      ifelse(data$age > 29 & data$age < 35, 8,
                                                                             ifelse(data$age > 34 & data$age < 40, 9,
                                                                                    ifelse(data$age > 39 & data$age < 45, 10,
                                                                                           ifelse(data$age > 44 & data$age < 50, 11,
                                                                                                  ifelse(data$age > 49 & data$age < 55, 12,
                                                                                                         ifelse(data$age > 54 & data$age < 60, 13,
                                                                                                                ifelse(data$age > 59 & data$age < 65, 14,
                                                                                                                       ifelse(data$age > 64 & data$age < 70, 15,
                                                                                                                              ifelse(data$age > 69 & data$age < 75, 16,
                                                                                                                                     ifelse(data$age > 74 & data$age < 80, 17,
                                                                                                                                            ifelse(data$age > 79 & data$age < 85, 18,
                                                                                                                                                   ifelse(data$age > 84 & data$age < 90, 19,
                                                                                                                                                          ifelse(data$age > 89 & data$age < 95, 20, 21))))))))))))))))))))
data[,17] <- as.factor(data[,17])
vsetky_roky <- as.integer(c("1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
                            "2009", "2010", "2011", "2012", "2013", "2014", "2015"))
vekove_skupiny <- c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                    "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                    "65 - 69" = "15", "70 - 74" = "16", "75 - 79" = "17", "80 - 84" = "18", "85 - 89" = "19", "90 - 94" = "20", "95+" = "21")
standardizovana_populacia <- c(1000, 4000, 5500, 5500, 5500, 6000, 6000, 6500, 7000, 7000, 7000, 7000, 6500, 6000, 5500, 5000, 4000,
                               2500, 1500, 800, 200)
#
function_pgle <- function(rok, kraj, okres, mech_diag, init_diag, init_diag_ch, vek, pohlavie) {
  #
  ### filter databazy mrtvych
  if(is.null(rok)) {
    data_subset1 <- data
  }else{
    data_subset1 <- as.data.frame(matrix(nc=dim(data)[2]))
    colnames(data_subset1) <- colnames(data)
    my_filter <- rok
    for(i in 1:length(my_filter)){
      data_subset1 <- rbind(data_subset1, subset(data, year == my_filter[i]))
    }
    data_subset1 <- data_subset1[-1,]
  }
  if(is.null(kraj)) {
    data_subset2 <- data_subset1
  }else{
    data_subset2 <- as.data.frame(matrix(nc=dim(data)[2]))
    colnames(data_subset2) <- colnames(data)
    my_filter <- kraj
    for(i in 1:length(my_filter)){
      data_subset2 <- rbind(data_subset2, subset(data_subset1, region == my_filter[i]))
    }
    data_subset2 <- data_subset2[-1,]
  }
  if(is.null(okres)) {
    data_subset3 <- data_subset2
  }else{
    data_subset3 <- as.data.frame(matrix(nc=dim(data)[2]))
    colnames(data_subset3) <- colnames(data)
    my_filter <- okres
    for(i in 1:length(my_filter)){
      data_subset3 <- rbind(data_subset3, subset(data_subset2, district == my_filter[i]))
    }
    data_subset3 <- data_subset3[-1,]
  }
  if(is.null(pohlavie)) {
    data_subset_lt <- data_subset3
  }else{
    data_subset_lt <- as.data.frame(matrix(nc=dim(data)[2]))
    colnames(data_subset_lt) <- colnames(data)
    my_filter <- pohlavie
    for(i in 1:length(my_filter)){
      data_subset_lt <- rbind(data_subset_lt, subset(data_subset3, sex == my_filter[i]))
    }
    data_subset_lt <- data_subset_lt[-1,]
  }
  if(is.null(mech_diag)) {
    data_subset4 <- data_subset_lt
  }else{
    data_subset4 <- as.data.frame(matrix(nc=dim(data)[2]))
    colnames(data_subset4) <- colnames(data)
    my_filter <- mech_diag
    for(i in 1:length(my_filter)){
      data_subset4 <- rbind(data_subset4, subset(data_subset_lt, mechanical_death_diagnosis == my_filter[i]))
    }
    data_subset4 <- data_subset4[-1,]
  }
  if(is.null(init_diag)) {
    data_subset5 <- data_subset4
  }else{
    data_subset5 <- as.data.frame(matrix(nc=dim(data)[2]))
    colnames(data_subset5) <- colnames(data)
    my_filter <- init_diag
    for(i in 1:length(my_filter)){
      data_subset5 <- rbind(data_subset5, subset(data_subset4, initial_death_diagnosis == my_filter[i]))
    }
    data_subset5 <- data_subset5[-1,]
  }
  if(is.null(init_diag_ch)) {
    data_subset6 <- data_subset5
  }else{
    data_subset6 <- as.data.frame(matrix(nc=dim(data)[2]))
    colnames(data_subset6) <- colnames(data)
    my_filter <- init_diag_ch
    for(i in 1:length(my_filter)){
      data_subset6 <- rbind(data_subset6, subset(data_subset5, initial_death_diagnosis_chapter == my_filter[i]))
    }
    data_subset6 <- data_subset6[-1,]
  }
  if(is.null(vek)) {
    data_subset <- data_subset6
  }else{
    data_subset <- as.data.frame(matrix(nc=dim(data)[2]))
    colnames(data_subset) <- colnames(data)
    my_filter <- vek
    for(i in 1:length(my_filter)){
      data_subset <- rbind(data_subset, subset(data_subset6, age_category == my_filter[i]))
    }
    data_subset <- data_subset[-1,]
  }
  #
  if(is.null(rok)) {
    roky <- as.integer(c("1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
                         "2009", "2010", "2011", "2012", "2013", "2014", "2015"))
  }else{roky <- as.integer(rok)}
  #
  if(is.null(vek)) {
    veky <- c(1:21)
  }else{veky <- as.integer(vek)}
  #
  n_col <- length(roky)
  n_row <- length(veky)
  #########################################################
  ### pomocna funkcia .... nerovna sa nule
  is.not.null <- function(x) ! is.null(x)
  #
  ### nacitanie stredneho stavu
  if(is.null(kraj) & is.null(okres)) {
    if(length(pohlavie) == 0 | length(pohlavie) == 2) {
      vekove_skupiny_muzi <- read_delim("databases/vekove_skupiny_muzi.csv",
                                        ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ","))
      vekove_skupiny_muzi <- as.data.frame(vekove_skupiny_muzi)
      
      vekove_skupiny_zeny <- read_delim("databases/vekove_skupiny_zeny.csv",
                                        ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ","))
      vekove_skupiny_zeny <- as.data.frame(vekove_skupiny_zeny)
      #
      stredny_stav <- vekove_skupiny_muzi + vekove_skupiny_zeny
    }else if(pohlavie == 2) {
      vekove_skupiny_zeny <- read_delim("databases/vekove_skupiny_zeny.csv",
                                        ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ","))
      #
      stredny_stav <- as.data.frame(vekove_skupiny_zeny)
    }else{
      vekove_skupiny_muzi <- read_delim("databases/vekove_skupiny_muzi.csv",
                                        ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ","))
      stredny_stav <- as.data.frame(vekove_skupiny_muzi)
    }
  }else if(is.not.null(okres)){
    okresy <- okres
    if(length(pohlavie) == 0 | length(pohlavie) == 2) {
      data_names_M <- paste0('M', okresy)
      data_names_F <- paste0('F', okresy)
      stredny_stav <- matrix(0, nc = 21, nr = 21)
      for (i in 1:length(okresy)) {
        filepath_M <- file.path(paste0('databases/stredny_stav_okresy/', data_names_M[i], '.csv'))
        filepath_F <- file.path(paste0('databases/stredny_stav_okresy/', data_names_F[i], '.csv'))
        assign(data_names_M[i], read_delim(filepath_M,
                                           ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ","))[1:21,])
        assign(data_names_F[i], read_delim(filepath_F,
                                           ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ","))[1:21,])
        stredny_stav <- stredny_stav + eval(parse(text = data_names_M[i])) + eval(parse(text = data_names_F[i]))
      }
    }else if(pohlavie == 2) {
      data_names_F <- paste0('F', okresy)
      stredny_stav <- matrix(0, nc = 21, nr = 21)
      for (i in 1:length(okresy)) {
        filepath_F <- file.path(paste0('databases/stredny_stav_okresy/', data_names_F[i], '.csv'))
        assign(data_names_F[i], read_delim(filepath_F,
                                           ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ","))[1:21,])
        stredny_stav <- stredny_stav + eval(parse(text = data_names_F[i]))
      }
    }else{
      data_names_M <- paste0('M', okresy)
      stredny_stav <- matrix(0, nc = 21, nr = 21)
      for (i in 1:length(okresy)) {
        filepath_M <- file.path(paste0('databases/stredny_stav_okresy/', data_names_M[i], '.csv'))
        assign(data_names_M[i], read_delim(filepath_M,
                                           ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ","))[1:21,])
        stredny_stav <- stredny_stav + eval(parse(text = data_names_M[i]))
      }
    }
  }
  #############################################################################################
  ### vypocet miery umrtnosti
  stredny_stav_sub <- as.matrix(stredny_stav[veky, match(roky, vsetky_roky)])
  #
  ### podrobna umrtnostna tabulka
  
  
  # tabulka pocet mrtvych
  tabulka_pocet_mrtvych <- matrix(NA, nc = n_col, nr = n_row)
  colnames(tabulka_pocet_mrtvych) <- roky
  rownames(tabulka_pocet_mrtvych) <- veky
  for(i in 1:n_row) {
    for(j in 1:n_col) {
      tempData <- subset(data_subset_lt, year == roky[j] & age_category == veky[i])
      tabulka_pocet_mrtvych[i,j] <- dim(tempData)[1]
    }
  }
  # specificka miera umrtnosti
  specificka_miera_umrtnosti <- tabulka_pocet_mrtvych/stredny_stav_sub
  # pravdepodobnost umrtia
  dlzka_intervalu <- c(1,4,rep(5,19))
  pravdepodobnost_umrtia <- (dlzka_intervalu * specificka_miera_umrtnosti) / (1 + dlzka_intervalu * (1 - 0.5) * specificka_miera_umrtnosti)
  # pravdepodobnost_umrtia <- 1 - exp(-specificka_miera_umrtnosti)
  # print(pravdepodobnost_umrtia[n_row,])
  pravdepodobnost_umrtia[n_row,] <- 1 - exp(-specificka_miera_umrtnosti[n_row,])
  # print(pravdepodobnost_umrtia[n_row,])
  #
  # pravdepodobnost prezitia
  pravdepodobnost_prezitia <- 1 - pravdepodobnost_umrtia
  #
  # hypoteticka populacia
  hypoteticka_populacia <- matrix(NA, nc = n_col, nr = n_row)
  hypoteticka_populacia[1,] <- 100000
  for(i in 1:n_col) {
    for(j in 2:n_row) {
      hypoteticka_populacia[j,i] <- hypoteticka_populacia[j-1,i] * pravdepodobnost_prezitia[j-1,i]
    }
  }
  # hypoteticka_populacia[n_row,] <- hypoteticka_populacia[n_row - 1,]
  #
  # hypoteticke pocty zomretych
  hYpoteticky_pocet_zomretych <- matrix(NA, nc = n_col, nr = n_row)
  for(i in 1:(n_row - 1)) {
    for(j in 1:n_col) {
      hYpoteticky_pocet_zomretych[i,j] <- hypoteticka_populacia[i,j] - hypoteticka_populacia[i+1,j]
    }
  }
  hYpoteticky_pocet_zomretych[n_row,] <- hypoteticka_populacia[n_row,] * pravdepodobnost_umrtia[n_row,]
  #
  # pocet prezitych rokov populaciou vo vekovom intervale
  pocet_prezitych_rokov <- matrix(NA, nc = n_col, nr = n_row)
  pocet_prezitych_rokov[1,] <- hypoteticka_populacia[1,] - 0.5 * hypoteticka_populacia[1,] * pravdepodobnost_umrtia[1,]
  for(i in 2:(n_row - 1)) {
    for(j in 1:n_col) {
      # pocet_prezitych_rokov[i,j] <- dlzka_intervalu[i] * (hypoteticka_populacia[i+1,j] + 0.5 * hYpoteticky_pocet_zomretych[i,j])
      pocet_prezitych_rokov[i,j] <- (hypoteticka_populacia[i + 1,j] + hypoteticka_populacia[i,j])  * dlzka_intervalu[i] / 2
    }
  }
  # pocet_prezitych_rokov[n_row,] <- hypoteticka_populacia[n_row,] / pravdepodobnost_umrtia[n_row,]
  pocet_prezitych_rokov[n_row,] <- hypoteticka_populacia[n_row-1,]  - (hypoteticka_populacia[n_row-1,] * pravdepodobnost_umrtia[n_row,]) / 2
  #
  # pocet clovekorokov
  pocet_clovekorokov <- matrix(NA, nc = n_col, nr = n_row)
  pocet_clovekorokov[n_row,] <- pocet_prezitych_rokov[n_row,]
  for(i in (n_row - 1):1) {
    for(j in 1:n_col) {
      pocet_clovekorokov[i,j] <- pocet_clovekorokov[i+1,j] + pocet_prezitych_rokov[i,j]
    }
  }
  #
  # ocakavana dlzka zivota
  ocakavana_dlzka_zivota <- pocet_clovekorokov / hypoteticka_populacia
  # ocakavana_dlzka_zivota[n_row,] <- 1/specificka_miera_umrtnosti[n_row,]
  #
  #######################################################################################################################################
  ########################################################### PGLE ######################################################################
  #######################################################################################################################################
  ### pocet umrti na diagnozu i
  i_tabulka_pocet_mrtvych <- matrix(NA, nc = n_col, nr = n_row)
  colnames(i_tabulka_pocet_mrtvych) <- roky
  rownames(i_tabulka_pocet_mrtvych) <- veky
  for(i in 1:n_row) {
    for(j in 1:n_col) {
      tempData <- subset(data_subset, year == roky[j] & age_category == veky[i])
      i_tabulka_pocet_mrtvych[i,j] <- dim(tempData)[1]
    }
  }
  ### pravdepodobnost umrtia na diagnozu i
  i_pravdepodobnost_umrtia <- 1 - pravdepodobnost_prezitia ^ ((tabulka_pocet_mrtvych - i_tabulka_pocet_mrtvych) / tabulka_pocet_mrtvych)
  #
  ### hypoteticka populacia po vyluceni diagnozy i
  i_hypoteticka_populacia <- matrix(NA, nc = n_col, nr = n_row)
  i_hypoteticka_populacia[1,] <- 100000
  for(i in 2:n_row) {
    for(j in 1: n_col) {
      i_hypoteticka_populacia[i, j] <- i_hypoteticka_populacia[i-1,j] * (1 - i_pravdepodobnost_umrtia[i-1,j])
    }
  }
  #
  ### hypoteticky pocet umrti
  i_hypoteticky_pocet_umrti <- matrix(NA, nc = n_col, nr = n_row)
  for(i in 1:(n_row - 1)) {
    for(j in 1: n_col) {
      i_hypoteticky_pocet_umrti[i,j] <- i_hypoteticka_populacia[i,j] - i_hypoteticka_populacia[i +1 ,j]
    }
  }
  i_hypoteticky_pocet_umrti[n_row,] <- i_hypoteticka_populacia[n_row,] * i_pravdepodobnost_umrtia[n_row,]
  #
  ### pocet prezitych rokov
  i_pocet_prezitych_rokov <- matrix(NA, nc = n_col, nr = n_row)
  # i_pocet_prezitych_rokov[1,] <- i_hypoteticka_populacia[1,] - 0.5 * i_hypoteticky_pocet_umrti[1,]
  for(i in 1:(n_row - 1)) {
    for(j in 1: n_col) {
      i_pocet_prezitych_rokov[i,j] <- (i_hypoteticka_populacia[i,j] + i_hypoteticka_populacia[i + 1,j]) * dlzka_intervalu[i] / 2
      # i_pocet_prezitych_rokov[i,j] <- dlzka_intervalu[i] * (i_hypoteticka_populacia[i+1,j] + 0.5 * i_hypoteticky_pocet_umrti[i,j])
    }
  }
  # i_pocet_prezitych_rokov[n_row,] <- i_hypoteticka_populacia[n_row,] / i_pravdepodobnost_umrtia[n_row,]
  i_pocet_prezitych_rokov[n_row,] <- i_hypoteticka_populacia[n_row-1,] - (i_hypoteticka_populacia[n_row-1,] * i_pravdepodobnost_umrtia[n_row,]) / 2
  #
  ### pocet clovekorokov
  i_pocet_clovekorokov <- matrix(NA, nc = n_col, nr = n_row)
  alfa <- (ocakavana_dlzka_zivota[n_row,] * hypoteticka_populacia[n_row,]) / pocet_prezitych_rokov[n_row,]
  i_pocet_clovekorokov[n_row,] <- alfa * i_pocet_prezitych_rokov[n_row,]
  # i_pocet_clovekorokov[n_row,] <- i_pocet_prezitych_rokov[n_row,]
  for(i in (n_row - 1):1) {
    for(j in 1:n_col) {
      i_pocet_clovekorokov[i,j] <- i_pocet_clovekorokov[i + 1,j] + i_pocet_prezitych_rokov[i,j]
    }
  }
  #
  ### ocakavana dlzka zivota
  i_ocakavana_dlzka_zivota <- i_pocet_clovekorokov / i_hypoteticka_populacia
  #
  ### pgle
  pgle <- i_ocakavana_dlzka_zivota - ocakavana_dlzka_zivota
  # # pocet umrti na vybranu pricinu
  # tabulka_pocet_mrtvych_i <- matrix(NA, nc = n_col, nr = n_row)
  # colnames(tabulka_pocet_mrtvych_i) <- roky
  # rownames(tabulka_pocet_mrtvych_i) <- veky
  # for(i in 1:n_row) {
  #   for(j in 1:n_col) {
  #     tempData <- subset(data_subset, year == roky[j] & age_category == veky[i])
  #     tabulka_pocet_mrtvych_i[i,j] <- dim(tempData)[1]
  #   }
  # }
  # #
  # # pravdepodobnost umrtia po eliminovani diagnozy
  # new_pravdepodobnost_umrtia <- 1 - pravdepodobnost_prezitia ^ ((tabulka_pocet_mrtvych - tabulka_pocet_mrtvych_i) / tabulka_pocet_mrtvych)
  # #
  # # 
  parameter <- matrix(rep(abs(pgle[n_row,]), each = 21), nr = 21, byrow = F)
  new_i_ocakavana_dlzka_zivota <- i_ocakavana_dlzka_zivota + parameter
  new_pgle <- new_i_ocakavana_dlzka_zivota - ocakavana_dlzka_zivota
  new_pgle[n_row,] <- abs(pgle[n_row,])
  
  
  ### output
  output <- list(ocakavana_dlzka_zivota, new_i_ocakavana_dlzka_zivota, new_pgle, hypoteticka_populacia, i_hypoteticka_populacia, 
                 pocet_prezitych_rokov, i_pocet_prezitych_rokov, pocet_clovekorokov, i_pocet_clovekorokov, 
                 pravdepodobnost_umrtia, i_pravdepodobnost_umrtia)
  # output1 <- list(tabulka_pocet_mrtvych_i, pravdepodobnost_umrtia, new_pravdepodobnost_umrtia)
  return(output)
}


function_pgle(c(2013,2014),NULL, NULL, NULL, c('E10', 'E11', 'E12', 'E13', 'E14'), NULL, NULL, NULL)
function_pgle(2014,NULL, NULL, NULL, c('I20', 'I21', 'I22', 'I23', 'I24', 'I25'), NULL, NULL, NULL)
function_pgle(2014,NULL, NULL, NULL, c('I60', 'I61', 'I62', 'I63', 'I64', 'I65', 'I66', 'I67', 'I68', 'I69'), NULL, NULL, NULL)
function_pgle(2015,NULL, NULL, NULL, c('G30'), NULL, NULL, NULL)

### standardizacia dat
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
standardDeath <- function(rok, kraj, okres, mech_diag, init_diag, init_diag_ch, vek, pohlavie) {
  
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
  if(is.null(mech_diag)) {
    data_subset4 <- data_subset3
  }else{
    data_subset4 <- as.data.frame(matrix(nc=dim(data)[2]))
    colnames(data_subset4) <- colnames(data)
    my_filter <- mech_diag
    for(i in 1:length(my_filter)){
      data_subset4 <- rbind(data_subset4, subset(data_subset3, mechanical_death_diagnosis == my_filter[i]))
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
    data_subset7 <- data_subset6
  }else{
    data_subset7 <- as.data.frame(matrix(nc=dim(data)[2]))
    colnames(data_subset7) <- colnames(data)
    my_filter <- vek
    for(i in 1:length(my_filter)){
      data_subset7 <- rbind(data_subset7, subset(data_subset6, age_category == my_filter[i]))
    }
    data_subset7 <- data_subset7[-1,]
  }
  if(is.null(pohlavie)) {
    data_subset <- data_subset7
  }else{
    data_subset <- as.data.frame(matrix(nc=dim(data)[2]))
    colnames(data_subset) <- colnames(data)
    my_filter <- pohlavie
    for(i in 1:length(my_filter)){
      data_subset <- rbind(data_subset, subset(data_subset7, sex == my_filter[i]))
    }
    data_subset <- data_subset[-1,]
  }
  
  if(is.null(rok)) {
    roky <- as.integer(c("1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
                         "2009", "2010", "2011", "2012", "2013", "2014", "2015"))
  }else{roky <- as.integer(rok)}
  
  if(is.null(vek)) {
    veky <- c(1:21)
  }else{veky <- as.integer(vek)}
  
  n_col <- length(roky)
  n_row <- length(veky)
  #######################################################################
  ### nacitanie stredneho stavu
  # pohlavie <- input$sex
  is.not.null <- function(x) ! is.null(x)
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
  #### doplnit podmienku len pre kraje !!!!
  stredny_stav_sub <- stredny_stav[veky, match(roky, vsetky_roky)]
  #
  # tabulka pocet mrtvych
  tabulka_pocet_mrtvych <- matrix(NA, nc = n_col, nr = n_row)
  colnames(tabulka_pocet_mrtvych) <- roky
  rownames(tabulka_pocet_mrtvych) <- veky
  for(i in 1:n_row) {
    for(j in 1:n_col) {
      tempData <- subset(data_subset, year == roky[j] & age_category == veky[i])
      tabulka_pocet_mrtvych[i,j] <- dim(tempData)[1]
    }
  }
  
  # hruba miera umrtnosti
  hruba_miera_umrtnosti <- tabulka_pocet_mrtvych/stredny_stav_sub
  
  # miera umrtnosti na sep
  sep <- standardizovana_populacia[veky]
  miera_umrtnosti_sep <- hruba_miera_umrtnosti * sep
  row.names(miera_umrtnosti_sep) <- names(vekove_skupiny)[veky]
  
  # return(stredny_stav_sub)
  return(miera_umrtnosti_sep)
}

## stahovanie mortality za vsetky diagnozy a okresy
  # vstupy
  rok <- vsetky_roky
  kraj <- NULL
  okres <- c("Bratislava I" = "101",
             "Bratislava II" = "102",
             "Bratislava III" = "103",
             "Bratislava IV" = "104",
             "Bratislava V" = "105",
             "Malacky" = "106",
             "Pezinok" = "107",
             "Senec" = "108",
             "Dunajská Streda" = "201",
             "Galanta" = "202",
             "Hlohovec" = "203",
             "Piešťany" = "204",
             "Senica" = "205",
             "Skalica" = "206",
             "Trnava" = "207",
             "Bánovce nad Bebravou" = "301",
             "Ilava" = "302",
             "Myjava" = "303",
             "Nové Mesto nad Váhom" = "304",
             "Partizánske" = "305",
             "Považská Bystrica" = "306",
             "Prievidza" = "307",
             "Púchov" = "308",
             "Trenčín" = "309",
             "Komárno" = "401",
             "Levice" = "402",
             "Nitra" = "403",
             "Nové Zámky" = "404",
             "Šaľa" = "405",
             "Topoľčany" = "406",
             "Zlaté Moravce" = "407",
             "Bytča" = "501",
             "Čadca" = "502",
             "Dolný Kubín" = "503",
             "Kysucké Nové Mesto" = "504",
             "Liptovský Mikuláš" = "505",
             "Martin" = "506",
             "Námestovo" = "507",
             "Ružomberok" = "508",
             "Turčianske Teplice" = "509",
             "Tvrdošín" = "510",
             "Žilina" = "511",
             "Banská Bystrica" = "601",
             "Banská Štiavnica" = "602",
             "Brezno" = "603",
             "Detva" = "604",
             "Krupina" = "605",
             "Lučenec" = "606",
             "Poltár" = "607",
             "Revúca" = "608",
             "Rimavská Sobota" = "609",
             "Veľký Krtíš" = "610",
             "Zvolen" = "611",
             "Žarnovica" = "612",
             "Žiar nad Hronom" = "613",
             "Bardejov" = "701",
             "Humenné" = "702",
             "Kežmarok" = "703",
             "Levoča" = "704",
             "Medzilaborce" = "705",
             "Poprad" = "706",
             "Prešov" = "707",
             "Sabinov" = "708",
             "Snina" = "709",
             "Stará Ľubovňa" = "710",
             "Stropkov" = "711",
             "Svidník" = "712",
             "Vranov nad Topľou" = "713",
             "Gelnica" = "801",
             "Košice I" = "802",
             "Košice II" = "803",
             "Košice III" = "804",
             "Košice IV" = "805",
             "Košice - okolie" = "806",
             "Michalovce" = "807",
             "Rožňava" = "808",
             "Sobrance" = "809",
             "Spišská Nová Ves" = "810",
             "Trebišov" = "811")
  mech_diag <- NULL
  init_diag <- NULL
  init_diag_ch <- NULL
  vek <- NULL
  # pohlavie <- c("muži" = "1", "ženy" = "2")
  # 
  # 
  # 
  # 
  # okres <- c("Bratislava I" = "101",
  #            "Bratislava II" = "102",
  #            "Bratislava III" = "103",
  #            "Bratislava IV" = "104",
  #            "Bratislava V" = "105",
  #            "Malacky" = "106",
  #            "Pezinok" = "107")
  # init_diag <- c("C00", "C01", "C02", "C03", "C04", 'C25')
  pohlavie <- c("muži" = "1", "ženy" = "2")
  ####################################
  # for (j in 1:length(init_diag)) {
  #   file_names <- paste0('D://PRACA/DATABAZA_MRTVYCH/ministerstvo/C_I/', init_diag[j], '_zeny.csv')
  #   # file_names <- paste0(init_diag[j], '_', okres, '.csv')
  #   # output_dir <- 'D://PRACA/DATABAZA_MRTVYCH/ministerstvo/C_I/'
  #   umrtnost_okresy <- matrix(NA, nc = 2, nr = length(okres))
  #   colnames(umrtnost_okresy) <- rok
  #   row.names(umrtnost_okresy) <- okres
  #   for (i in 1:length(okres)) {
  #     umrtnost_okresy[i,] <- colSums(standardDeath(rok, kraj = NULL, okres = okres[i], mech_diag, init_diag[j], init_diag_ch,
  #                                                  vek, pohlavie = pohlavie), na.rm = T)
  #   }
  #   # saveData(umrtnost_okresy, file_names[j], output_dir)
  #   # print(umrtnost_okresy)
  # 
  #   write.csv2(umrtnost_okresy, file = file_names)
  # }
  celkova_umrtnost <- array(data = NA, dim = c(length(okres), length(vsetky_roky), 2))
  #
  for (i in 1:2) {
      print(j)
      for(k in 1:length(okres)) {
        # temp_diag <- init_diag[[j]]
        celkova_umrtnost[k,,i] <- colSums(standardDeath(rok, kraj = NULL, okres = okres[k], mech_diag, init_diag, init_diag_ch,
                                                             vek, pohlavie = pohlavie[i]), na.rm = T)
      }
    }
  
  save(celkova_umrtnost, file = 'D:/PRACA/ODVRATITELNA_UMRTNOST/new_celkova_umrtnost.RData')
  
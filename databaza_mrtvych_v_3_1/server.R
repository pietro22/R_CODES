#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#
###################################################
### verzia 3
### - oprava vypoctu standardizovanej umrtnosti na urovni krajov a okresov
###################################################


library(shiny)

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
  # kniznice
  library(readr)
  library(ggplot2)
  library(plyr)
  library(reshape2)
  
  # source('mortality_standardization.R')

  # nacitanie databazy
  dataset <- read_delim("databases/mortality.csv",
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

  # nacitanie vekovzch skupin obyvatelstva na slovensku + standardizovanej populacie
  
  
  # vekove_skupiny_muzi <- read_delim("databases/vekove_skupiny_muzi.csv",
  #                                   ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ","))
  # vekove_skupiny_muzi <- as.data.frame(vekove_skupiny_muzi)
  # 
  # vekove_skupiny_zeny <- read_delim("databases/vekove_skupiny_zeny.csv",
  #                                   ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ","))
  # vekove_skupiny_zeny <- as.data.frame(vekove_skupiny_zeny)

  standardizovana_populacia <- c(1000, 4000, 5500, 5500, 5500, 6000, 6000, 6500, 7000, 7000, 7000, 7000, 6500, 6000, 5500, 5000, 4000,
                                 2500, 1500, 800, 200)
  vekove_skupiny <- c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
    "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
    "65 - 69" = "15", "70 - 74" = "16", "75 - 79" = "17", "80 - 84" = "18", "85 - 89" = "19", "90 - 94" = "20", "95+" = "21")
  vsetky_roky <- as.integer(c("1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
                                      "2009", "2010", "2011", "2012", "2013", "2014", "2015"))
  vsetky_okresy <- c("BA_I" = "101",                     "Bratislava II" = "102",
                     "BA_III" = "103",
                     "BA_IV" = "104",
                     "BA_V" = "105",
                     "MA" = "106",
                     "PK" = "107",
                     "SC" = "108",
                     "DS" = "201",
                     "GA" = "202",
                     "HC" = "203",
                     "PN" = "204",
                     "SE" = "205",
                     "SI" = "206",
                     "TT" = "207",
                     "BN" = "301",
                     "IL" = "302",
                     "MY" = "303",
                     "NM" = "304",
                     "PE" = "305",
                     "PB" = "306",
                     "PD" = "307",
                     "PU" = "308",
                     "TN" = "309",
                     "KN" = "401",
                     "LV" = "402",
                     "NR" = "403",
                     "NZ" = "404",
                     "SA" = "405",
                     "TO" = "406",
                     "ZM" = "407",
                     "BY" = "501",
                     "CA" = "502",
                     "DK" = "503",
                     "KM" = "504",
                     "LM" = "505",
                     "MT" = "506",
                     "NO" = "507",
                     "RK" = "508",
                     "TR" = "509",
                     "TS" = "510",
                     "ZA" = "511",
                     "BB" = "601",
                     "BS" = "602",
                     "BR" = "603",
                     "DT" = "604",
                     "KA" = "605",
                     "LC" = "606",
                     "PT" = "607",
                     "RA" = "608",
                     "RS" = "609",
                     "VK" = "610",
                     "ZV" = "611",
                     "ZC" = "612",
                     "ZH" = "613",
                     "BJ" = "701",
                     "HE" = "702",
                     "KK" = "703",
                     "LE" = "704",
                     "ML" = "705",
                     "PP" = "706",
                     "PO" = "707",
                     "SB" = "708",
                     "SV" = "709",
                     "SL" = "710",
                     "SP" = "711",
                     "SK" = "712",
                     "VT" = "713",
                     "GL" = "801",
                     "KE_I" = "802",
                     "KE_II" = "803",
                     "KE_III" = "804",
                     "KE_IV" = "805",
                     "KS" = "806",
                     "MI" = "807",
                     "RV" = "808",
                     "SO" = "809",
                     "SN" = "810",
                     "TV" = "811",
                     "Ine" = "999")
  ba <- c("Bratislava I" = "101",
          "Bratislava II" = "102",
          "Bratislava III" = "103",
          "Bratislava IV" = "104",
          "Bratislava V" = "105",
          "Malacky" = "106",
          "Pezinok" = "107",
          "Senec" = "108")
  tt <- c("Dunajská Streda" = "201",
          "Galanta" = "202",
          "Hlohovec" = "203",
          "Piešťany" = "204",
          "Senica" = "205",
          "Skalica" = "206",
          "Trnava" = "207")
  tn <- c("Bánovce nad Bebravou" = "301",
          "Ilava" = "302",
          "Myjava" = "303",
          "Nové Mesto nad Váhom" = "304",
          "Partizánske" = "305",
          "Považská Bystrica" = "306",
          "Prievidza" = "307",
          "Púchov" = "308",
          "Trenčín" = "309")
  nr <- c("Komárno" = "401",
          "Levice" = "402",
          "Nitra" = "403",
          "Nové Zámky" = "404",
          "Šaľa" = "405",
          "Topoľčany" = "406",
          "Zlaté Moravce" = "407")
  za <- c("Bytča" = "501",
          "Čadca" = "502",
          "Dolný Kubín" = "503",
          "Kysucké Nové Mesto" = "504",
          "Liptovský Mikuláš" = "505",
          "Martin" = "506",
          "Námestovo" = "507",
          "Ružomberok" = "508",
          "Turčianske Teplice" = "509",
          "Tvrdošín" = "510",
          "Žilina" = "511")
  bb <- c("Banská Bystrica" = "601",
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
          "Žiar nad Hronom" = "613")
  po <- c("Bardejov" = "701",
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
          "Vranov nad Topľou" = "713")
  ke <- c("Gelnica" = "801",
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
  ine <- c("Iné" = "999")
  
  ### automaticky zapis csv suborov
  saveData <- function(data, file_name, output_dir) {
    # Create a unique file name
    fileName <- sprintf(file_name, as.integer(Sys.time()), digest::digest(data))
    # Write the file to the local system
    write.csv(
      x = data,
      file = file.path(output_dir, fileName), 
      row.names = FALSE, quote = TRUE
    )
  }
  

  #######################################################################################################

  observeEvent(input$region, {
    selectedRegion <- input$region
    if(selectedRegion == 1) {
      updatePickerInput(session = session, inputId = 'district', choices = ba)
    }else if(selectedRegion == 2) {
      updatePickerInput(session = session, inputId = 'district', choices = tt)
    }else if(selectedRegion == 3) {
      updatePickerInput(session = session, inputId = 'district', choices = tn)
    }else if(selectedRegion == 4) {
      updatePickerInput(session = session, inputId = 'district', choices = nr)
    }else if(selectedRegion == 5) {
      updatePickerInput(session = session, inputId = 'district', choices = za)
    }else if(selectedRegion == 6) {
      updatePickerInput(session = session, inputId = 'district', choices = bb)
    }else if(selectedRegion == 7) {
      updatePickerInput(session = session, inputId = 'district', choices = po)
    }else if(selectedRegion == 8) {
      updatePickerInput(session = session, inputId = 'district', choices = ke)
    }else if(selectedRegion == 9) {
      updatePickerInput(session = session, inputId = 'district', choices = ine)
    }

  })
  
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
    
    # # stredny stav - subset podla filtra
    # pohlavie <- input$sex
    # if(length(pohlavie) == 0 | length(pohlavie) == 2) {
    #   stredny_stav <- vekove_skupiny_muzi + vekove_skupiny_zeny
    # }else if(input$sex == 2) {
    #   stredny_stav <- vekove_skupiny_zeny
    # }else{stredny_stav <- vekove_skupiny_muzi}
    # 
    # stredny_stav_sub <- stredny_stav[veky, match(roky, vsetky_roky)]
    
    # hruba miera umrtnosti
    hruba_miera_umrtnosti <- tabulka_pocet_mrtvych/stredny_stav_sub
    
    # miera umrtnosti na sep
    sep <- standardizovana_populacia[veky]
    miera_umrtnosti_sep <- hruba_miera_umrtnosti * sep
    row.names(miera_umrtnosti_sep) <- names(vekove_skupiny)[veky]
    
    # return(stredny_stav_sub)
    return(miera_umrtnosti_sep)
  }


  my_output <- eventReactive(input$set, {

    rok <- input$year
    kraj <- input$region
    okres <- input$district
    mech_diag <- input$mechanical_death
    init_diag <- input$initial_death
    init_diag_ch <- input$initial_diagnosis_chapter
    vek <- input$age_category
    pohlavie <- input$sex
    ####################################
    standardDeath(rok, kraj, okres, mech_diag, init_diag, init_diag_ch, vek, pohlavie)
    
    
    # if(is.null(input$year)) {
    #   data_subset1 <- data
    # }else{
    #   data_subset1 <- as.data.frame(matrix(nc=dim(data)[2]))
    #   colnames(data_subset1) <- colnames(data)
    #   my_filter <- input$year
    #   for(i in 1:length(my_filter)){
    #     data_subset1 <- rbind(data_subset1, subset(data, year == my_filter[i]))
    #   }
    #   data_subset1 <- data_subset1[-1,]
    # }
    # if(is.null(input$region)) {
    #   data_subset2 <- data_subset1
    # }else{
    #   data_subset2 <- as.data.frame(matrix(nc=dim(data)[2]))
    #   colnames(data_subset2) <- colnames(data)
    #   my_filter <- input$region
    #   for(i in 1:length(my_filter)){
    #     data_subset2 <- rbind(data_subset2, subset(data_subset1, region == my_filter[i]))
    #   }
    #   data_subset2 <- data_subset2[-1,]
    # }
    # if(is.null(input$district)) {
    #   data_subset3 <- data_subset2
    # }else{
    #   data_subset3 <- as.data.frame(matrix(nc=dim(data)[2]))
    #   colnames(data_subset3) <- colnames(data)
    #   my_filter <- input$district
    #   for(i in 1:length(my_filter)){
    #     data_subset3 <- rbind(data_subset3, subset(data_subset2, district == my_filter[i]))
    #   }
    #   data_subset3 <- data_subset3[-1,]
    # }
    # if(is.null(input$mechanical_death)) {
    #   data_subset4 <- data_subset3
    # }else{
    #   data_subset4 <- as.data.frame(matrix(nc=dim(data)[2]))
    #   colnames(data_subset4) <- colnames(data)
    #   my_filter <- input$mechanical_death
    #   for(i in 1:length(my_filter)){
    #     data_subset4 <- rbind(data_subset4, subset(data_subset3, mechanical_death_diagnosis == my_filter[i]))
    #   }
    #   data_subset4 <- data_subset4[-1,]
    # }
    # if(is.null(input$initial_death)) {
    #   data_subset5 <- data_subset4
    # }else{
    #   data_subset5 <- as.data.frame(matrix(nc=dim(data)[2]))
    #   colnames(data_subset5) <- colnames(data)
    #   my_filter <- input$initial_death
    #   for(i in 1:length(my_filter)){
    #     data_subset5 <- rbind(data_subset5, subset(data_subset4, initial_death_diagnosis == my_filter[i]))
    #   }
    #   data_subset5 <- data_subset5[-1,]
    # }
    # if(is.null(input$initial_diagnosis_chapter)) {
    #   data_subset6 <- data_subset5
    # }else{
    #   data_subset6 <- as.data.frame(matrix(nc=dim(data)[2]))
    #   colnames(data_subset6) <- colnames(data)
    #   my_filter <- input$initial_diagnosis_chapter
    #   for(i in 1:length(my_filter)){
    #     data_subset6 <- rbind(data_subset6, subset(data_subset5, initial_death_diagnosis_chapter == my_filter[i]))
    #   }
    #   data_subset6 <- data_subset6[-1,]
    # }
    # if(is.null(input$age_category)) {
    #   data_subset7 <- data_subset6
    # }else{
    #   data_subset7 <- as.data.frame(matrix(nc=dim(data)[2]))
    #   colnames(data_subset7) <- colnames(data)
    #   my_filter <- input$age_category
    #   for(i in 1:length(my_filter)){
    #     data_subset7 <- rbind(data_subset7, subset(data_subset6, age_category == my_filter[i]))
    #   }
    #   data_subset7 <- data_subset7[-1,]
    # }
    # if(is.null(input$sex)) {
    #   data_subset <- data_subset7
    # }else{
    #   data_subset <- as.data.frame(matrix(nc=dim(data)[2]))
    #   colnames(data_subset) <- colnames(data)
    #   my_filter <- input$sex
    #   for(i in 1:length(my_filter)){
    #     data_subset <- rbind(data_subset, subset(data_subset7, sex == my_filter[i]))
    #   }
    #   data_subset <- data_subset[-1,]
    # }
    # 
    # if(is.null(input$year)) {
    #   roky <- as.integer(c("1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
    #                "2009", "2010", "2011", "2012", "2013", "2014", "2015"))
    # }else{roky <- as.integer(input$year)}
    # 
    # if(is.null(input$age_category)) {
    #   veky <- c(1:21)
    # }else{veky <- as.integer(input$age_category)}
    # 
    # n_col <- length(roky)
    # n_row <- length(veky)
    # #######################################################################
    # ### nacitanie stredneho stavu
    # pohlavie <- input$sex
    # is.not.null <- function(x) ! is.null(x)
    # if(is.null(input$region) & is.null(input$district)) {
    #   if(length(pohlavie) == 0 | length(pohlavie) == 2) {
    #     vekove_skupiny_muzi <- read_delim("databases/vekove_skupiny_muzi.csv",
    #                                       ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ","))
    #     vekove_skupiny_muzi <- as.data.frame(vekove_skupiny_muzi)
    #     
    #     vekove_skupiny_zeny <- read_delim("databases/vekove_skupiny_zeny.csv",
    #                                       ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ","))
    #     vekove_skupiny_zeny <- as.data.frame(vekove_skupiny_zeny)
    #     #
    #     stredny_stav <- vekove_skupiny_muzi + vekove_skupiny_zeny
    #   }else if(input$sex == 2) {
    #     vekove_skupiny_zeny <- read_delim("databases/vekove_skupiny_zeny.csv",
    #                                       ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ","))
    #     #
    #     stredny_stav <- as.data.frame(vekove_skupiny_zeny)
    #   }else{
    #     vekove_skupiny_muzi <- read_delim("databases/vekove_skupiny_muzi.csv",
    #                                       ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ","))
    #     stredny_stav <- as.data.frame(vekove_skupiny_muzi)
    #     }
    # }else if(is.not.null(input$district)){
    #   okresy <- input$district
    #   if(length(pohlavie) == 0 | length(pohlavie) == 2) {
    #     data_names_M <- paste0('M', okresy)
    #     data_names_F <- paste0('F', okresy)
    #     stredny_stav <- matrix(0, nc = 21, nr = 21)
    #     for (i in 1:length(okresy)) {
    #       filepath_M <- file.path(paste0('databases/stredny_stav_okresy/', data_names_M[i], '.csv'))
    #       filepath_F <- file.path(paste0('databases/stredny_stav_okresy/', data_names_F[i], '.csv'))
    #       assign(data_names_M[i], read_delim(filepath_M,
    #                                          ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ",")))
    #       assign(data_names_F[i], read_delim(filepath_F,
    #                                          ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ",")))
    #       stredny_stav <- stredny_stav + eval(parse(text = data_names_M[i])) + eval(parse(text = data_names_F[i]))
    #     }
    #   }else if(input$sex == 2) {
    #     data_names_F <- paste0('F', okresy)
    #     stredny_stav <- matrix(0, nc = 21, nr = 21)
    #     for (i in 1:length(okresy)) {
    #       filepath_F <- file.path(paste0('databases/stredny_stav_okresy/', data_names_F[i], '.csv'))
    #       assign(data_names_F[i], read_delim(filepath_F,
    #                                          ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ",")))
    #       stredny_stav <- stredny_stav + eval(parse(text = data_names_F[i]))
    #     }
    #   }else{
    #     data_names_M <- paste0('M', okresy)
    #     stredny_stav <- matrix(0, nc = 21, nr = 21)
    #     for (i in 1:length(okresy)) {
    #       filepath_M <- file.path(paste0('databases/stredny_stav_okresy/', data_names_M[i], '.csv'))
    #       assign(data_names_M[i], read_delim(filepath_M,
    #                                          ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ",")))
    #       stredny_stav <- stredny_stav + eval(parse(text = data_names_M[i]))
    #     }
    #   }
    # }
    # #### doplnit podmienku len pre kraje !!!!
    # stredny_stav_sub <- stredny_stav[veky, match(roky, vsetky_roky)]
    # #
    # # tabulka pocet mrtvych
    # tabulka_pocet_mrtvych <- matrix(NA, nc = n_col, nr = n_row)
    # colnames(tabulka_pocet_mrtvych) <- roky
    # rownames(tabulka_pocet_mrtvych) <- veky
    # for(i in 1:n_row) {
    #   for(j in 1:n_col) {
    #     tempData <- subset(data_subset, year == roky[j] & age_category == veky[i])
    #     tabulka_pocet_mrtvych[i,j] <- dim(tempData)[1]
    #   }
    # }
    # 
    # # # stredny stav - subset podla filtra
    # # pohlavie <- input$sex
    # # if(length(pohlavie) == 0 | length(pohlavie) == 2) {
    # #   stredny_stav <- vekove_skupiny_muzi + vekove_skupiny_zeny
    # # }else if(input$sex == 2) {
    # #   stredny_stav <- vekove_skupiny_zeny
    # # }else{stredny_stav <- vekove_skupiny_muzi}
    # # 
    # # stredny_stav_sub <- stredny_stav[veky, match(roky, vsetky_roky)]
    # 
    # # hruba miera umrtnosti
    # hruba_miera_umrtnosti <- tabulka_pocet_mrtvych/stredny_stav_sub
    # 
    # # miera umrtnosti na sep
    # sep <- standardizovana_populacia[veky]
    # miera_umrtnosti_sep <- hruba_miera_umrtnosti * sep
    # row.names(miera_umrtnosti_sep) <- names(vekove_skupiny)[veky]
    # 
    # # return(stredny_stav_sub)
    # return(miera_umrtnosti_sep)

  })

  celkova_umrtnost <- eventReactive(input$set, {
    celkova_umrtnost <- as.data.frame(t(colSums(my_output())))
    return(celkova_umrtnost)
  })

  output$download_umrtnost <- downloadHandler(
    # filename = function(){'output.csv'},
    filename = "downloaded_data.csv",
    content = function(file) {
          write.csv2(my_output(), file)
  },
  contentType = 'application/csv'
  )
####################################################################################################################
######################################### GRAFY - STANDARDIZOVANA UMRTNOST #########################################
####################################################################################################################
  plot_values <- eventReactive(input$draw, {
    if(is.null(input$year)) {
      roky <- as.integer(c("1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008",
                           "2009", "2010", "2011", "2012", "2013", "2014", "2015"))
    }else{roky <- as.integer(input$year)}
    
    rok <- input$year
    kraj <- input$region
    okres <- input$district
    mech_diag <- input$mechanical_death
    init_diag <- input$initial_death
    init_diag_ch <- input$initial_diagnosis_chapter
    vek <- input$age_category
    pohlavie <- input$sex
    ####################################
    # vypocet standardizovanej umrtnosti
    SR_M <- standardDeath(rok, kraj = NULL, okres = NULL, mech_diag, init_diag, init_diag_ch, vek, pohlavie = 1)
    SR_F <- standardDeath(rok, kraj = NULL, okres = NULL, mech_diag, init_diag, init_diag_ch, vek, pohlavie = 2)
    sub_M <- standardDeath(rok, kraj, okres, mech_diag, init_diag, init_diag_ch, vek, pohlavie = 1)
    sub_F <- standardDeath(rok, kraj, okres, mech_diag, init_diag, init_diag_ch, vek, pohlavie = 2)
    ####################################
    stData <- data.frame('year' = as.factor(roky),
                         'SR_M' = colSums(SR_M, na.rm = T),
                         'SR_F' = colSums(SR_F, na.rm = T),
                         'X1' = colSums(sub_M, na.rm = T),
                         'X2' = colSums(sub_F, na.rm = T))
    names(stData) <- c('year', 'SR_M', 'SR_F', paste0(names(vsetky_okresy[vsetky_okresy==input$district]), '_M'), 
                       paste0(names(vsetky_okresy[vsetky_okresy==input$district]), '_F'))
    myData <- melt(stData, id.vars = 'year')
    colnames(myData) <- c('year', 'mortality', 'frequency')
    return(myData)
  })

  plotInput = function(limit0 = 1000, limit1 = 3000, myStep = 200) {
    ggplot(plot_values(), aes(year, frequency, group = mortality, color = mortality)) +
      # geom_bar(stat = 'identity', aes(fill = sex), position = 'dodge') +
      geom_line(size = 1) +
      geom_point(size = 2) +
      theme(legend.position = c(0.93,0.87) ,panel.border = element_blank(),
            axis.line = element_line(colour = "grey"),
            panel.grid.major = element_line(colour = "grey", size = 0.20),
            panel.grid.minor = element_blank(), panel.background = element_rect(fill="white")) +
      theme(axis.text.x = element_text(angle = 45, vjust = 0.6, size = 10)) +
      theme(axis.text.y = element_text(size = 10)) +
      theme(axis.title=element_text(size=10)) +
      scale_y_continuous(breaks = seq(limit0,limit1,myStep), limits = c(limit0,limit1)) +
      scale_colour_manual(values=c('lightsteelblue', 'peachpuff2', 'blue', 'red'))

    # ggplot(plot_values(), aes(year, frequency, fill = year)) +
    #   theme(legend.position = "none" ,panel.border = element_blank(),
    #         axis.line = element_line(colour = "grey"),
    #         panel.grid.major = element_line(colour = "grey", size = 0.20),
    #         panel.grid.minor = element_blank(), panel.background = element_rect(fill="white")) +
    #   geom_bar(stat = 'identity')
  }

  output$my_plot <- renderPlot({
    newlimit0 <- input$limit0
    newlimit1 <- input$limit1
    newStep <- input$myStep
    plotInput(newlimit0, newlimit1, newStep)
  })
  
  # output$my_plot <- eventReactive(input$updatePlot, {renderPlot({
  #   newlimit0 <- input$limit0
  #   newlimit1 <- input$limit1
  #   plotInput(newlimit0, newlimit1)
  # })
  # })
  
  

  output$save_plot = downloadHandler(
    filename = 'test.png',
    content = function(file) {
      device <- function(..., width, height) {
        grDevices::png(..., width = width, height = height,
                       res = 300, units = "in")
      }
      ggsave(file, plot = plotInput(input$limit0, input$limit1, input$myStep), device = device, height = 5, width = 8, units = 'in')
    })


  output$check <- renderPrint(dim(my_output()))
  output$pocet_mrtvych <- renderTable(my_output(), rownames = T)
  output$celkova_umrtnost <- renderTable(celkova_umrtnost())
  
  ######################################################################################################################################
  group_diagnosis <- eventReactive(input$groups, {
    #
    rok <- c(2011,2015)
    kraj <- NULL
    okres <- input$district
    mech_diag <- input$mechanical_death
    init_diag <- input$initial_death
    init_diag_ch <- input$initial_diagnosis_chapter
    vek <- input$age_category
    pohlavie <- input$sex
    ####################################
    # vypocet standardizovanej umrtnosti
    umrtnost_okresy <- matrix(NA, nc = 2, nr = length(okres))
    for (i in 1:length(okres)) {
      umrtnost_okresy[i,] <- colSums(standardDeath(rok, kraj = NULL, okres = okres[i], mech_diag, init_diag, init_diag_ch,
                                                   vek, pohlavie = pohlavie), na.rm = T)
    }
    #
    row.names(umrtnost_okresy) <- okres
    colnames(umrtnost_okresy) <- rok
    #
    return(umrtnost_okresy)
  })
  
  # umrtnost_vsetky_okresy <- eventReactive(input$groups, {
  #   umrtnost_okresy <- as.data.frame(group_diagnosis())
  #   return(umrtnost_okresy)
  # })
  
  output$umrtnost_vsetky_okresy <- renderTable(group_diagnosis(), rownames = T)
  
  output$download_umrtnost_vsetky_okresy <- downloadHandler(
    # filename = function(){'output.csv'},
    filename = "downloaded_data.csv",
    content = function(file) {
      write.csv2(group_diagnosis(), file)
    },
    contentType = 'application/csv'
  )
  
  ##############################################
  ### stahovanie mortality za vsetky diagnozy a okresy
  # all_diagnosis <- eventReactive(input$all_diagnosis, {
  #   # vstupy
  #   rok <- input$year
  #   kraj <- NULL
  #   okres <- input$district
  #   mech_diag <- input$mechanical_death
  #   init_diag <- input$initial_death
  #   init_diag_ch <- input$initial_diagnosis_chapter
  #   vek <- input$age_category
  #   pohlavie <- input$sex
  #   ####################################
  #   for (j in 1:length(init_diag)) {
  #     file_names <- paste0('D://PRACA/DATABAZA_MRTVYCH/ministerstvo/C_I/', init_diag[j], '_muzi.csv')
  #     # file_names <- paste0(init_diag[j], '_', okres, '.csv')
  #     # output_dir <- 'D://PRACA/DATABAZA_MRTVYCH/ministerstvo/C_I/'
  #     umrtnost_okresy <- matrix(NA, nc = 2, nr = length(okres))
  #     for (i in 1:length(okres)) {
  #       umrtnost_okresy[i,] <- colSums(standardDeath(rok, kraj = NULL, okres = okres[i], mech_diag, init_diag[j], init_diag_ch,
  #                                                    vek, pohlavie = pohlavie), na.rm = T)
  #     }
  #     # saveData(umrtnost_okresy, file_names[j], output_dir)
  #     # print(umrtnost_okresy)
  #     
  #     write.csv2(umrtnost_okresy, file = file_names)
  #   }
  # })
  
  output$downloadData <- downloadHandler(
    filename = 'pdfs.zip',
    content = function(fname) {
      tmpdir <- tempdir()
      setwd(tempdir())
      # print(tempdir())
      rok <- input$year
      kraj <- NULL
      okres <- input$district
      mech_diag <- input$mechanical_death
      init_diag <- input$initial_death
      init_diag_ch <- input$initial_diagnosis_chapter
      vek <- input$age_category
      pohlavie <- input$sex
      ####################################
      fs <- paste0(init_diag, '_muzi.csv')
      for (j in 1:length(init_diag)) {
        file_names <- paste0(init_diag[j], '_muzi.csv')
        # file_names <- paste0(init_diag[j], '_', okres, '.csv')
        # output_dir <- 'D://PRACA/DATABAZA_MRTVYCH/ministerstvo/C_I/'
        umrtnost_okresy <- matrix(NA, nc = 2, nr = length(okres))
        for (i in 1:length(okres)) {
          umrtnost_okresy[i,] <- colSums(standardDeath(rok, kraj = NULL, okres = okres[i], mech_diag, init_diag[j], init_diag_ch,
                                                       vek, pohlavie = pohlavie), na.rm = T)
        }
        # saveData(umrtnost_okresy, file_names[j], output_dir)
        # print(umrtnost_okresy)
        
        write.csv2(umrtnost_okresy, file = file_names)
      }
      

      # fs <- paste0(init_diag, '_muzi.csv')
      # write.csv(datasetInput()$rock, file = "rock.csv", sep =",")
      # write.csv(datasetInput()$pressure, file = "pressure.csv", sep =",")
      # write.csv(datasetInput()$cars, file = "cars.csv", sep =",")
      # print (fs)

      zip(zipfile=fname, files=fs)
    },
    contentType = "application/zip"
  )
  


  ######################################################################################################################################
  ########################################################### PGLE #####################################################################
  ######################################################################################################################################
  
  death_probability_men <- read_delim("databases/umrtie_muzi.csv", 
                                      ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ","))
  death_probability_men <- as.data.frame(death_probability_men)
  death_probability_women <- read_delim("databases/umrtie_zeny.csv", 
                                      ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ","))
  death_probability_women <- as.data.frame(death_probability_women)
  ### nacitanie tabuliek o pocte ludi, ktori sa dozili veku
  live_men <- read_delim("databases/zivi_muzi.csv", 
                         ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ","))
  live_men <- as.data.frame(live_men)
  live_women <- read_delim("databases/zive_zeny.csv", 
                           ";", escape_double = FALSE, trim_ws = TRUE, locale = locale(decimal_mark = ","))
  live_women <- as.data.frame(live_women)
  
  pgle_output <- eventReactive(input$pgle, {
    # pocet skumanych pricin smrti
    priciny_smrti <- input$pgle_initial_death
    pocet_pricin_smrti <- length(priciny_smrti)
    
    # filtrovanie databazy
    if(is.null(input$pgle_year)) {
      data_subset1 <- data
    }else{
      data_subset1 <- as.data.frame(matrix(nc=dim(data)[2]))
      colnames(data_subset1) <- colnames(data)
      my_filter <- input$pgle_year
      for(i in 1:length(my_filter)){
        data_subset1 <- rbind(data_subset1, subset(data, year == my_filter[i]))
      }
      data_subset1 <- data_subset1[-1,]
    }
    if(is.null(input$pgle_region)) {
      data_subset2 <- data_subset1
    }else{
      data_subset2 <- as.data.frame(matrix(nc=dim(data)[2]))
      colnames(data_subset2) <- colnames(data)
      my_filter <- input$pgle_region
      for(i in 1:length(my_filter)){
        data_subset2 <- rbind(data_subset2, subset(data_subset1, region == my_filter[i]))
      }
      data_subset2 <- data_subset2[-1,]
    }
    if(is.null(input$pgle_district)) {
      data_subset3 <- data_subset2
    }else{
      data_subset3 <- as.data.frame(matrix(nc=dim(data)[2]))
      colnames(data_subset3) <- colnames(data)
      my_filter <- input$pgle_district
      for(i in 1:length(my_filter)){
        data_subset3 <- rbind(data_subset3, subset(data_subset2, district == my_filter[i]))
      }
      data_subset3 <- data_subset3[-1,]
    }
    if(is.null(input$pgle_initial_death)) {
      data_subset4 <- data_subset3
    }else{
      data_subset4 <- as.data.frame(matrix(nc=dim(data)[2]))
      colnames(data_subset4) <- colnames(data)
      my_filter <- input$pgle_initial_death
      for(i in 1:length(my_filter)){
        data_subset4 <- rbind(data_subset4, subset(data_subset3, initial_death_diagnosis == my_filter[i]))
      }
      data_subset4 <- data_subset4[-1,]
    }
    if(is.null(input$pgle_initial_diagnosis_chapter)) {
      data_subset5 <- data_subset4
    }else{
      data_subset5 <- as.data.frame(matrix(nc=dim(data)[2]))
      colnames(data_subset5) <- colnames(data)
      my_filter <- input$pgle_initial_diagnosis_chapter
      for(i in 1:length(my_filter)){
        data_subset5 <- rbind(data_subset5, subset(data_subset4, initial_death_diagnosis_chapter == my_filter[i]))
      }
      data_subset5 <- data_subset5[-1,]
    }
    if(is.null(input$pgle_sex)) {
      data_subset <- data_subset5
    }else{
      data_subset <- as.data.frame(matrix(nc=dim(data)[2]))
      colnames(data_subset) <- colnames(data)
      my_filter <- input$pgle_sex
      for(i in 1:length(my_filter)){
        data_subset <- rbind(data_subset, subset(data_subset5, sex == my_filter[i]))
      }
      data_subset <- data_subset[-1,]
    }
    
    if(is.null(input$pgle_year)) {
      roky <- as.integer(c("1996", "1997", "1998", "1999", "2000", "2001", "2002", "2003", "2004", "2005", "2006", "2007", "2008", 
                           "2009", "2010", "2011", "2012", "2013", "2014", "2015"))
    }else{roky <- as.integer(input$pgle_year)}
    
    data_subset[,11] <- as.factor(data_subset[,11])
    
    n_col <- length(roky)
    veky <- c(0:100)
    n_row <- 101
    temp_n_row <- max(data$age)
    temp_veky <- c(0:temp_n_row)
    
    # modifikacia umrtnostnych tabuliek
    
    # pocet_umrti_na_diagnozy <- count(data_subset[,c(1,4,11)])
    # pocet_umrti_vo_vekovej_skupine <- count(data_subset[,c(1,4)])
    # 
    pocet_umrti_diag_tab <- matrix(NA, nc = n_col, nr = temp_n_row + 1)
    pocet_umrti_tab <- matrix(NA, nc = n_col, nr = temp_n_row + 1)
    colnames(pocet_umrti_diag_tab) <- roky
    rownames(pocet_umrti_diag_tab) <- temp_veky
    colnames(pocet_umrti_tab) <- roky
    rownames(pocet_umrti_tab) <- temp_veky
    for(i in 1:(temp_n_row + 1)) {
      for(j in 1:n_col) {
        tempData <- subset(data, year == roky[j] & age == temp_veky[i])
        pocet_umrti_tab[i,j] <- dim(tempData)[1]

        tempData1 <- subset(data_subset, year == roky[j] & age == temp_veky[i])
        pocet_umrti_diag_tab[i,j] <- dim(tempData1)[1]
      }
    }
    
    ### spocitanie veku 100+ do jedneho riadku
    over100_temp_veky <- which(temp_veky > 99)
    pocet_umrti_diag_tab_over100 <- colSums(pocet_umrti_diag_tab[over100_temp_veky,])
    pocet_umrti_diag_tab <- rbind(pocet_umrti_diag_tab[1:100,], pocet_umrti_diag_tab_over100)
    
    pocet_umrti_tab_over100 <- colSums(pocet_umrti_tab[over100_temp_veky,])
    pocet_umrti_tab <- rbind(pocet_umrti_tab[1:100,], pocet_umrti_tab_over100)
    
    # transformacia pravdepodobnosti
    pgle_pohlavie <- input$pgle_sex
    if(length(pgle_pohlavie) == 0 | length(pgle_pohlavie) == 2) {
      return('Zadajte pohlavie')
    }else if(input$pgle_sex == 1) {
      umrtnost <- as.matrix(death_probability_men[,-1])
      umrtnost <- umrtnost[,as.character(roky)]
      pocet_dozitych <- as.matrix(live_men[,-1])
      pocet_dozitych <- pocet_dozitych[,as.character(roky)]
      runNext <- T
    }else if(input$pgle_sex == 2) {
      umrtnost <- as.matrix(death_probability_women[,-1])
      umrtnost <- umrtnost[,as.character(roky)]
      pocet_dozitych <- as.matrix(live_women[,-1])
      pocet_dozitych <- pocet_dozitych[,as.character(roky)]
      runNext <- T
    }
    
    ######## nefunguje, ked sa zada len jeden rok!!!!!!!!!!!!!!!!!!
    if(runNext == T) {
    trans_umrtnost <- 1 - (1 - umrtnost^((pocet_umrti_tab - pocet_umrti_diag_tab) / pocet_umrti_tab))
    # return(c(dim(umrtnost), dim(pocet_umrti_tab), dim(pocet_umrti_diag_tab)))
    #
    ### upraveny pocet dozitych
    trans_doziti <- pocet_dozitych * (1 - trans_umrtnost)
    kumulovany_pocet_dozitych1 <- colSums(pocet_dozitych)
    kumulovany_pocet_dozitych <- colSums(trans_doziti)
    return(cbind(kumulovany_pocet_dozitych1, kumulovany_pocet_dozitych))
    }
  })
  
  output$pgle <- renderTable(pgle_output())
  
})

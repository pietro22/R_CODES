library(ggplot2)
library(reshape2)
library(matrixStats)
library(readr)

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
############################################################################################################################################
### odvratitelna umrtnost
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
mech_diag <- list('tuberculosis' = NULL,
                  'bacterial_infections' = NULL,
                  'hepatitis_C' = NULL,
                  'hiv_aids' = NULL,
                  'colon_neoplasm' = NULL,
                  'skin_melanoma' = NULL,
                  'breast_cancer' = NULL,
                  'cervical_cancer' = NULL,
                  'bladder_cancer' = NULL,
                  'thyroid_cancer' = NULL,
                  'hodkin_disease' = NULL,
                  'leukaemia' = NULL,
                  'benign_neoplasm' = NULL,
                  'diabetes_melitus' = NULL,
                  'ischemic_heart' = NULL,
                  'rheumatic_vavulvar' = NULL,
                  'hypertenisive_heart' = NULL,
                  'cerebrovascular_disease' = NULL,
                  'influenza' = NULL,
                  'pneumonia' = NULL,
                  'asthma' = NULL,
                  'gastric_duodenal' = NULL,
                  'acute_abdomen' = NULL,
                  'nephritis_nephrosis' = NULL,
                  'obstructive_uropathy' = NULL,
                  'complication_perinatal' = NULL,
                  'congenital_malformations' = NULL,
                  'epilepsy' = NULL,
                  'misadventures' = c('Y60', 'Y61', 'Y62', 'Y63', 'Y64', 'Y65', 'Y66', 'Y67', 'Y68', 'Y69', 'Y83', 'Y84'))
init_diag <- list('tuberculosis' = c('A15', 'A16', 'A17', 'A18', 'A19', 'B90'),
                  'bacterial_infections' = c('A38', 'A39', 'A40', 'A41', 'A46', 'A48', 'B50', 'B51', 'B52', 'B53', 'B54', 'G00', 'G03',
                                             'J02', 'L03'),
                  'hepatitis_C' = c('B17', 'B18'),
                  'hiv_aids' = c('B20', 'B21', 'B22', 'B23', 'B24'),
                  'colon_neoplasm' = c('C18', 'C19', 'C20', 'C21'),
                  'skin_melanoma' = c('C43'),
                  'breast_cancer' = c('C50'),
                  'cervical_cancer' = c('C53'),
                  'bladder_cancer' = c('C67'),
                  'thyroid_cancer' = c('C73'),
                  'hodkin_disease' = c('C81'),
                  'leukaemia' = c('C91', 'C920'),
                  'benign_neoplasm' = c('D10', 'D11', 'D12', 'D13', 'D14', 'D15', 'D16', 'D17', 'D18', 'D19', 'D20', 'D21', 'D22', 'D23',
                                        'D24', 'D25', 'D26', 'D27', 'D28', 'D29', 'D30', 'D31', 'D32', 'D33', 'D34', 'D35', 'D36'),
                  'diabetes_melitus' = c('E10', 'E11', 'E12', 'E13', 'E14'),
                  'ischemic_heart' = c('I20', 'I21', 'I22', 'I23', 'I24', 'I25'),
                  'rheumatic_vavulvar' = c('I01', 'I02', 'I03', 'I04', 'I05', 'I06', 'I07', 'I08', 'I09'),
                  'hypertenisive_heart' = c('I10', 'I11', 'I12', 'I13', 'I14', 'I15'),
                  'cerebrovascular_disease' = c('I60', 'I61', 'I62', 'I63', 'I64', 'I65', 'I66', 'I67', 'I68', 'I69'),
                  'influenza' = c('J09', 'J10', 'J11'),
                  'pneumonia' = c('J12', 'J13', 'J14', 'J15', 'J16', 'J17', 'J18'),
                  'asthma' = c('J45', 'J46'),
                  'gastric_duodenal' = c('K25', 'K26', 'K27', 'K28'),
                  'acute_abdomen' = c('K35', 'K36', 'K37', 'K38', 'K40', 'K41', 'K42', 'K43', 'K44', 'K45', 'K46', 'K80', 'K81', 'K82', 'K83',
                                      'K85', 'K86', 'K91'),
                  'nephritis_nephrosis' = c('N00', 'N01', 'N02', 'N03', 'N04', 'N05', 'N06', 'N07', 'N17', 'N18', 'N19', 'N25', 'N26', 'N27'),
                  'obstructive_uropathy' = c('N13', 'N20', 'N21', 'N35', 'N40', 'N99'),
                  'complication_perinatal' = c('P00', 'P01', 'P02', 'P03', 'P04', 'P05', 'P06', 'P07', 'P08', 'P09',
                                               'P10', 'P11', 'P12', 'P13', 'P14', 'P15', 'P16', 'P17', 'P18', 'P19',
                                               'P20', 'P22', 'P22', 'P23', 'P24', 'P25', 'P26', 'P27', 'P28', 'P29',
                                               'P30', 'P33', 'P32', 'P33', 'P34', 'P35', 'P36', 'P37', 'P38', 'P39',
                                               'P40', 'P44', 'P42', 'P43', 'P44', 'P45', 'P46', 'P47', 'P48', 'P49',
                                               'P50', 'P55', 'P52', 'P53', 'P54', 'P55', 'P56', 'P57', 'P58', 'P59',
                                               'P60', 'P66', 'P62', 'P63', 'P64', 'P65', 'P66', 'P67', 'P68', 'P69',
                                               'P70', 'P77', 'P72', 'P73', 'P74', 'P75', 'P76', 'P77', 'P78', 'P79',
                                               'P80', 'P88', 'P82', 'P83', 'P84', 'P85', 'P86', 'P87', 'P88', 'P89',
                                               'P90', 'P99', 'P92', 'P93', 'P94', 'P95', 'P96', 'P97', 'P98', 'P99',
                                               'A33'),
                  'congenital_malformations' = c('Q00', 'Q01', 'Q02', 'Q03', 'Q04', 'Q05', 'Q06', 'Q07', 'Q08', 'Q09',
                                                 'Q10', 'Q11', 'Q12', 'Q13', 'Q14', 'Q15', 'Q16', 'Q17', 'Q18', 'Q19',
                                                 'Q20', 'Q22', 'Q22', 'Q23', 'Q24', 'Q25', 'Q26', 'Q27', 'Q28', 'Q29',
                                                 'Q30', 'Q33', 'Q32', 'Q33', 'Q34', 'Q35', 'Q36', 'Q37', 'Q38', 'Q39',
                                                 'Q40', 'Q44', 'Q42', 'Q43', 'Q44', 'Q45', 'Q46', 'Q47', 'Q48', 'Q49',
                                                 'Q50', 'Q55', 'Q52', 'Q53', 'Q54', 'Q55', 'Q56', 'Q57', 'Q58', 'Q59',
                                                 'Q60', 'Q66', 'Q62', 'Q63', 'Q64', 'Q65', 'Q66', 'Q67', 'Q68', 'Q69',
                                                 'Q70', 'Q77', 'Q72', 'Q73', 'Q74', 'Q75', 'Q76', 'Q77', 'Q78', 'Q79',
                                                 'Q80', 'Q88', 'Q82', 'Q83', 'Q84', 'Q85', 'Q86', 'Q87', 'Q88', 'Q89',
                                                 'Q90', 'Q99', 'Q92', 'Q93', 'Q94', 'Q95', 'Q96', 'Q97', 'Q98', 'Q99'),
                  'epilepsy' = c('G40', 'G41'),
                  'misadventures' = NULL)
init_diag_ch <- NULL
vek <- list('tuberculosis' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                               "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                               "65 - 69" = "15", "70 - 74" = "16"),
            'bacterial_infections' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                                       "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                                       "65 - 69" = "15", "70 - 74" = "16"),
            'hepatitis_C' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                              "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                              "65 - 69" = "15", "70 - 74" = "16"),
            'hiv_aids' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                           "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                           "65 - 69" = "15", "70 - 74" = "16", "75 - 79" = "17", "80 - 84" = "18", "85 - 89" = "19", "90 - 94" = "20", "95+" = "21"),
            'colon_neoplasm' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                                 "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                                 "65 - 69" = "15", "70 - 74" = "16"),
            'skin_melanoma' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                                "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                                "65 - 69" = "15", "70 - 74" = "16"),
            'breast_cancer' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                                "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                                "65 - 69" = "15", "70 - 74" = "16"),
            'cervical_cancer' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                                  "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                                  "65 - 69" = "15", "70 - 74" = "16"),
            'bladder_cancer' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                                 "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                                 "65 - 69" = "15", "70 - 74" = "16"),
            'thyroid_cancer' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                                 "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                                 "65 - 69" = "15", "70 - 74" = "16"),
            'hodkin_disease' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                                 "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                                 "65 - 69" = "15", "70 - 74" = "16"),
            'leukaemia' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                            "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10"),
            'benign_neoplasm' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                                  "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                                  "65 - 69" = "15", "70 - 74" = "16"),
            'diabetes_melitus' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                                   "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11"),
            'ischemic_heart' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                                 "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                                 "65 - 69" = "15", "70 - 74" = "16"),
            'rheumatic_vavulvar' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                                     "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                                     "65 - 69" = "15", "70 - 74" = "16"),
            'hypertenisive_heart' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                                      "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                                      "65 - 69" = "15", "70 - 74" = "16"),
            'cerebrovascular_disease' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                                          "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                                          "65 - 69" = "15", "70 - 74" = "16"),
            'influenza' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                            "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                            "65 - 69" = "15", "70 - 74" = "16"),
            'pneumonia' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                            "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                            "65 - 69" = "15", "70 - 74" = "16"),
            'asthma' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                         "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                         "65 - 69" = "15", "70 - 74" = "16"),
            'gastric_duodenal' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                                   "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                                   "65 - 69" = "15", "70 - 74" = "16"),
            'acute_abdomen' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                                "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                                "65 - 69" = "15", "70 - 74" = "16"),
            'nephritis_nephrosis' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                                      "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                                      "65 - 69" = "15", "70 - 74" = "16"),
            'obstructive_uropathy' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                                       "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                                       "65 - 69" = "15", "70 - 74" = "16"),
            'complication_perinatal' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                                         "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                                         "65 - 69" = "15", "70 - 74" = "16", "75 - 79" = "17", "80 - 84" = "18", "85 - 89" = "19", "90 - 94" = "20", "95+" = "21"),
            'congenital_malformations' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                                           "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                                           "65 - 69" = "15", "70 - 74" = "16"),
            'epilepsy' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                           "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                           "65 - 69" = "15", "70 - 74" = "16"),
            'misadventures' = c("0" = "1", "1 - 4" = "2", "5 - 9" = "3", "10 - 14" = "4", "15 - 19" = "5", "20 - 24" = "6", "25 - 29" = "7",
                                "30 - 34" = "8", "35 - 39" = "9", "40 - 44" = "10", "45 - 49" = "11", "50 - 54" = "12", "55 - 59" = "13", "60 - 64" = "14",
                                "65 - 69" = "15", "70 - 74" = "16", "75 - 79" = "17", "80 - 84" = "18", "85 - 89" = "19", "90 - 94" = "20", "95+" = "21"))
pohlavie <- c("muži" = "1", "ženy" = "2")
####################################
liecitelna_umrtnost <- array(data = NA, dim = c(length(okres), length(vsetky_roky), length(init_diag), 2))
#
for (i in 1:2) {
  for (j in 1:length(init_diag)) {
    print(j)
    for(k in 1:length(okres)) {
      # temp_diag <- init_diag[[j]]
      liecitelna_umrtnost[k,,j,i] <- colSums(standardDeath(rok, kraj = NULL, okres = okres[k], mech_diag[[j]], init_diag[[j]], init_diag_ch,
                                                           vek[[j]], pohlavie = pohlavie[i]), na.rm = T)
    }
  }
}

save(liecitelna_umrtnost, file = 'D:/PRACA/ODVRATITELNA_UMRTNOST/new_liecitelna_umrtnost.RData')

# 
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

###########################################################################################################################################
########################################################### Pre Evku ######################################################################
###########################################################################################################################################
# svk_lu <- array(data = NA, dim = c(27, length(vsetky_roky), 2))
# for (i in 1:27) {
#   for (j in 1:2) {
#     svk_lu[i,,j] <- colMeans(liecitelna_umrtnost[,,i,j], na.rm = T)
#   }
# }
# 
# svk_lu1 <- matrix(NA, nc = 20, nr = 2)
# for (i in 1:2) {
#   svk_lu1[i,] <- colSums(svk_lu[,,i])
# }

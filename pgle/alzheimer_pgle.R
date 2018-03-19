library(reshape2)
library(plm)
library(ggplot2)
### vygenerovanie ocakavanej dlzky zivota a pgle
ba <- function_pgle(c(2001:2015),NULL,c("Bratislava I" = "101",
                                        "Bratislava II" = "102",
                                        "Bratislava III" = "103",
                                        "Bratislava IV" = "104",
                                        "Bratislava V" = "105",
                                        "Malacky" = "106",
                                        "Pezinok" = "107",
                                        "Senec" = "108"), NULL, 'G30', NULL, NULL, NULL)
tt <- function_pgle(c(2001:2015),NULL,c("Dunajská Streda" = "201",
                                        "Galanta" = "202",
                                        "Hlohovec" = "203",
                                        "Piešťany" = "204",
                                        "Senica" = "205",
                                        "Skalica" = "206",
                                        "Trnava" = "207"), NULL, 'G30', NULL, NULL, NULL)
tn <- function_pgle(c(2001:2015),NULL,c("Bánovce nad Bebravou" = "301",
                                        "Ilava" = "302",
                                        "Myjava" = "303",
                                        "Nové Mesto nad Váhom" = "304",
                                        "Partizánske" = "305",
                                        "Považská Bystrica" = "306",
                                        "Prievidza" = "307",
                                        "Púchov" = "308",
                                        "Trenčín" = "309"), NULL, 'G30', NULL, NULL, NULL)
nr <- function_pgle(c(2001:2015),NULL,c("Komárno" = "401",
                                        "Levice" = "402",
                                        "Nitra" = "403",
                                        "Nové Zámky" = "404",
                                        "Šaľa" = "405",
                                        "Topoľčany" = "406",
                                        "Zlaté Moravce" = "407"), NULL, 'G30', NULL, NULL, NULL)
za <- function_pgle(c(2001:2015),NULL,c("Bytča" = "501",
                                        "Čadca" = "502",
                                        "Dolný Kubín" = "503",
                                        "Kysucké Nové Mesto" = "504",
                                        "Liptovský Mikuláš" = "505",
                                        "Martin" = "506",
                                        "Námestovo" = "507",
                                        "Ružomberok" = "508",
                                        "Turčianske Teplice" = "509",
                                        "Tvrdošín" = "510",
                                        "Žilina" = "511"), NULL, 'G30', NULL, NULL, NULL)
bb <- function_pgle(c(2001:2015),NULL,c("Banská Bystrica" = "601",
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
                                        "Žiar nad Hronom" = "613"), NULL, 'G30', NULL, NULL, NULL)
po <- function_pgle(c(2001:2015),NULL,c("Bardejov" = "701",
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
                                        "Vranov nad Topľou" = "713"), NULL, 'G30', NULL, NULL, NULL)
ke <- function_pgle(c(2001:2015),NULL,c("Gelnica" = "801",
                                        "Košice I" = "802",
                                        "Košice II" = "803",
                                        "Košice III" = "804",
                                        "Košice IV" = "805",
                                        "Košice - okolie" = "806",
                                        "Michalovce" = "807",
                                        "Rožňava" = "808",
                                        "Sobrance" = "809",
                                        "Spišská Nová Ves" = "810",
                                        "Trebišov" = "811"), NULL, 'G30', NULL, NULL, NULL)
############################################################################################################################################
################################################# priprava dat na regresiu #################################################################
############################################################################################################################################
### pgle
pgle <- rbind(ba[[3]][1,], tt[[3]][1,], tn[[3]][1,], nr[[3]][1,], za[[3]][1,], bb[[3]][1,], po[[3]][1,], ke[[3]][1,])
pgle <- data.frame(region = c('ba', 'tt', 'tn', 'nr', 'za', 'bb', 'po', 'ke'), pgle)
colnames(pgle) <- c('region', 2001:2015)
m_pgle <- melt(pgle, id.vars = 'region')
### hdp
hdp <- read.csv2('D:/PRACA/CLANKY/alzheimer/data/Datastatistickyurad/Datastatistickyurad/Regionálny_HDP.csv')
hdp <- hdp[-c(1,3,7,10),]
hdp[,1] <- c('ba', 'tt', 'tn', 'nr', 'za', 'bb', 'po', 'ke')
hdp <- hdp[,-c(2:6)]
colnames(hdp) <- c('region', 2001:2015)
m_hdp <- melt(hdp, id.vars = 'region')
### prijem
prijem <- read.csv2('D:/PRACA/CLANKY/alzheimer/data/Datastatistickyurad/Datastatistickyurad/Priemerna_nominálna_mesačna_mzda_zamestnanca.csv')
prijem <- prijem[,-17]
prijem[,1] <- c('ba', 'tt', 'tn', 'nr', 'za', 'bb', 'po', 'ke')
colnames(prijem) <- c('region', 2001:2015)
m_prijem <- melt(prijem, id.vars = 'region')
### nezamestnanost
nezamestnanost <- read.csv2('D:/PRACA/CLANKY/alzheimer/data/Datastatistickyurad/Datastatistickyurad/Miera_nezamestnanosti.csv')
nezamestnanost <- nezamestnanost[,-c(2,18)]
nezamestnanost[,1] <- c('ba', 'tt', 'tn', 'nr', 'za', 'bb', 'po', 'ke')
colnames(nezamestnanost) <- c('region', 2001:2015)
m_nezamestnanost <- melt(nezamestnanost, id.vars = 'region')
### vzdelanie
vzdelanie <- read.csv2('D:/PRACA/CLANKY/alzheimer/data/Datastatistickyurad/Datastatistickyurad/vysokoskolsky_vzdelani.csv')
vzdelanie[,1] <- c('ba', 'tt', 'tn', 'nr', 'za', 'bb', 'po', 'ke')
colnames(vzdelanie) <- c('region', 2001:2015)
m_vzdelanie <- melt(vzdelanie, id.vars = 'region')
##########################################
### spojenie do jednej databazy
panel_data <- cbind(m_pgle, m_hdp[,3], m_prijem[,3], m_nezamestnanost[,3], m_vzdelanie[,3])
colnames(panel_data) <- c('region', 'year', 'pgle', 'gdp', 'wage', 'unemployment', 'education')
##########################################
### korelacna matica
cor_matrix <- cor(as.matrix(panel_data[,3:7]))
cor.test(panel_data[,3], panel_data[,4])
cor.test(panel_data[,3], panel_data[,5])
cor.test(panel_data[,3], panel_data[,6])
cor.test(panel_data[,3], panel_data[,7])
cor.test(panel_data[,4], panel_data[,5])
cor.test(panel_data[,4], panel_data[,6])
cor.test(panel_data[,4], panel_data[,7])
cor.test(panel_data[,5], panel_data[,6])
cor.test(panel_data[,5], panel_data[,7])
cor.test(panel_data[,6], panel_data[,7])
### panelova regresia
p.data <- pdata.frame(panel_data, index=c("region", "year"))
# form <- pgle ~ gdp + wage + unemployment + education
form <- log(pgle) ~ log(gdp) + log(wage) + log(unemployment) + log(education)
# testovanie
pooltest(form, data=p.data, model="within", effect="individual")
pooltest(form, data=p.data, model="within", effect="time")
pooltest(form, data=p.data, model="pooling", effect="individual")
pooltest(form, data=p.data, model="pooling", effect="time")
#
pFtest(form, data = p.data, effect = "individual")
pFtest(form, data = p.data, effect = "time")
# cross-sectional dependence
pcdtest(form, data=p.data, model="pooling")
# serial correlation
pwtest(form, data = p.data)
pbsytest(form, data = p.data)
pbltest(form, data = p.data)
pbgtest(form, data = p.data)
pwartest(form, data = p.data)
###########################################
### modely
modelA <- plm(form, data=p.data, model = "within", effect = "individual")
summary(modelA)
modelB <- plm(form, data=p.data, model = "random", effect = "individual")
summary(modelB)
modelC <- plm(form, data=p.data, model = "pooling", effect = "individual")
summary(modelC)
modelD <- plm(form, data=p.data, model = "fd", effect = "individual")
summary(modelD)
### hausman
phtest(modelA, modelB)
#############################################################################################################################################
###################################################################### grafy ################################################################
#############################################################################################################################################
### vyvoj ocakavanej dlzky zivota v case
ocakavana_dlzka_zivota <- rbind(ba[[1]][1,], tt[[1]][1,], tn[[1]][1,], nr[[1]][1,], za[[1]][1,], bb[[1]][1,], po[[1]][1,], ke[[1]][1,])
ocakavana_dlzka_zivota <- data.frame(region = c('ba', 'tt', 'tn', 'nr', 'za', 'bb', 'po', 'ke'), ocakavana_dlzka_zivota)
colnames(ocakavana_dlzka_zivota) <- c('region', c(2001:2015))
m_ocakavana_dlzka_zivota <- melt(ocakavana_dlzka_zivota, id.var = 'region')
colnames(m_ocakavana_dlzka_zivota) <- c('region', 'year', 'value')
m_ocakavana_dlzka_zivota[,2] <- as.numeric(levels(m_ocakavana_dlzka_zivota[,2]))[m_ocakavana_dlzka_zivota[,2]]
theme_set(theme_bw())
p_ocakavana_dlzka_zivota <- ggplot(m_ocakavana_dlzka_zivota, aes(x = year, y = value, linetype = region)) + 
      geom_line() + 
      labs(# title="Yearly Time Series", 
           # subtitle="Returns Percentage from Economics Dataset", 
           # caption="Source: Economics", 
           y="Expected life length", x = 'Year') +  # title and caption
      scale_x_continuous(labels = c(2001:2015), 
                   breaks = seq(2001, 2015, 1)) +
      scale_y_continuous(limits = c(72, 79), breaks = seq(0, 100, 0.5)) + # change to monthly ticks and labels
      theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
            panel.grid.minor = element_blank())  # turn off minor grid
p_ocakavana_dlzka_zivota
ggsave('D:/PRACA/CLANKY/alzheimer/data/grafy/ocakavana_dlzka_zivota.png', p_ocakavana_dlzka_zivota, width = 4.5, height = 2.8, units = 'in')
### vyvoj ocakavanej dlzky zivota v case bez alzheimera
ocakavana_dlzka_zivota <- rbind(ba[[2]][1,], tt[[2]][1,], tn[[2]][1,], nr[[2]][1,], za[[2]][1,], bb[[2]][1,], po[[2]][1,], ke[[2]][1,])
ocakavana_dlzka_zivota <- data.frame(region = c('ba', 'tt', 'tn', 'nr', 'za', 'bb', 'po', 'ke'), ocakavana_dlzka_zivota)
colnames(ocakavana_dlzka_zivota) <- c('region', c(2001:2015))
m_ocakavana_dlzka_zivota <- melt(ocakavana_dlzka_zivota, id.var = 'region')
colnames(m_ocakavana_dlzka_zivota) <- c('region', 'year', 'value')
m_ocakavana_dlzka_zivota[,2] <- as.numeric(levels(m_ocakavana_dlzka_zivota[,2]))[m_ocakavana_dlzka_zivota[,2]]
theme_set(theme_bw())
p_ocakavana_dlzka_zivota <- ggplot(m_ocakavana_dlzka_zivota, aes(x = year, y = value, linetype = region)) + 
  geom_line() + 
  labs(# title="Yearly Time Series", 
    # subtitle="Returns Percentage from Economics Dataset", 
    # caption="Source: Economics", 
    y="Expected life length", x = 'Year') +  # title and caption
  scale_x_continuous(labels = c(2001:2015), 
                     breaks = seq(2001, 2015, 1)) +
  scale_y_continuous(limits = c(72, 79), breaks = seq(0, 100, 0.5)) + # change to monthly ticks and labels
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid
p_ocakavana_dlzka_zivota
ggsave('D:/PRACA/CLANKY/alzheimer/data/grafy/ocakavana_dlzka_zivota_bez_alzheimera.png', p_ocakavana_dlzka_zivota, width = 4.5, height = 2.8, units = 'in')
### vyvoj hdp
pm_hdp <- m_hdp
colnames(pm_hdp) <- c('region', 'year', 'value')
pm_hdp[,2] <- as.numeric(levels(pm_hdp[,2]))[pm_hdp[,2]]
theme_set(theme_bw())
p_hdp <- ggplot(pm_hdp, aes(x = year, y = value, linetype = region)) + 
  geom_line() + 
  labs(# title="Yearly Time Series", 
    # subtitle="Returns Percentage from Economics Dataset", 
    # caption="Source: Economics", 
    y="GDP per capita in EUR", x = 'Year') +  # title and caption
  scale_x_continuous(labels = c(2001:2015), 
                     breaks = seq(2001, 2015, 1)) +
  scale_y_continuous(limits = c(0, 40000), breaks = seq(0, 100000, 5000)) + # change to monthly ticks and labels
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid
p_hdp
ggsave('D:/PRACA/CLANKY/alzheimer/data/grafy/hdp.png', p_hdp, width = 4.5, height = 2.8, units = 'in')
### vyvoj prijem
pm_hdp <- m_prijem
colnames(pm_hdp) <- c('region', 'year', 'value')
pm_hdp[,2] <- as.numeric(levels(pm_hdp[,2]))[pm_hdp[,2]]
theme_set(theme_bw())
p_hdp <- ggplot(pm_hdp, aes(x = year, y = value, linetype = region)) + 
  geom_line() + 
  labs(# title="Yearly Time Series", 
    # subtitle="Returns Percentage from Economics Dataset", 
    # caption="Source: Economics", 
    y="Average wage in EUR", x = 'Year') +  # title and caption
  scale_x_continuous(labels = c(2001:2015), 
                     breaks = seq(2001, 2015, 1)) +
  scale_y_continuous(limits = c(200, 1400), breaks = seq(0, 100000, 200)) + # change to monthly ticks and labels
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid
p_hdp
ggsave('D:/PRACA/CLANKY/alzheimer/data/grafy/prijem.png', p_hdp, width = 4.5, height = 2.8, units = 'in')
### vyvoj nezamestnanost
pm_hdp <- m_nezamestnanost
colnames(pm_hdp) <- c('region', 'year', 'value')
pm_hdp[,2] <- as.numeric(levels(pm_hdp[,2]))[pm_hdp[,2]]
theme_set(theme_bw())
p_hdp <- ggplot(pm_hdp, aes(x = year, y = value, linetype = region)) + 
  geom_line() + 
  labs(# title="Yearly Time Series", 
    # subtitle="Returns Percentage from Economics Dataset", 
    # caption="Source: Economics", 
    y="Unemployment rate in %", x = 'Year') +  # title and caption
  scale_x_continuous(labels = c(2001:2015), 
                     breaks = seq(2001, 2015, 1)) +
  scale_y_continuous(limits = c(0, 30), breaks = seq(0, 100, 5)) + # change to monthly ticks and labels
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid
p_hdp
ggsave('D:/PRACA/CLANKY/alzheimer/data/grafy/nezamestnanost.png', p_hdp, width = 4.5, height = 2.8, units = 'in')
### vyvoj vzdelanie
pm_hdp <- m_vzdelanie
pm_hdp[,3] <- pm_hdp[,3] * 100
colnames(pm_hdp) <- c('region', 'year', 'value')
pm_hdp[,2] <- as.numeric(levels(pm_hdp[,2]))[pm_hdp[,2]]
theme_set(theme_bw())
p_hdp <- ggplot(pm_hdp, aes(x = year, y = value, linetype = region)) + 
  geom_line() + 
  labs(# title="Yearly Time Series", 
    # subtitle="Returns Percentage from Economics Dataset", 
    # caption="Source: Economics", 
    y="Tertiary educated employees in %", x = 'Year') +  # title and caption
  scale_x_continuous(labels = c(2001:2015), 
                     breaks = seq(2001, 2015, 1)) +
  scale_y_continuous(limits = c(5, 45), breaks = seq(0, 100, 5)) + # change to monthly ticks and labels
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid
p_hdp
ggsave('D:/PRACA/CLANKY/alzheimer/data/grafy/vzdelanie.png', p_hdp, width = 4.5, height = 2.5, units = 'in')
### vyvoj pgle
pm_hdp <- m_pgle
# pm_hdp[,3] <- pm_hdp[,3] * 100
colnames(pm_hdp) <- c('region', 'year', 'value')
pm_hdp[,2] <- as.numeric(levels(pm_hdp[,2]))[pm_hdp[,2]]
theme_set(theme_bw())
p_hdp <- ggplot(pm_hdp, aes(x = year, y = value, linetype = region)) + 
  geom_line() + 
  labs(# title="Yearly Time Series", 
    # subtitle="Returns Percentage from Economics Dataset", 
    # caption="Source: Economics", 
    y="Potential gain in life expectancy", x = 'Year') +  # title and caption
  scale_x_continuous(labels = c(2001:2015), 
                     breaks = seq(2001, 2015, 1)) +
  scale_y_continuous(limits = c(0, .25), breaks = seq(0, 1, .05)) + # change to monthly ticks and labels
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid
p_hdp
ggsave('D:/PRACA/CLANKY/alzheimer/data/grafy/pgle.png', p_hdp, width = 4.5, height = 2.8, units = 'in')

#### mapka za rok 2015
###################################################
### tabulky
### tabulka ocakavana dlzka zivota za rok 2001
ocakavana_dlzka_zivota_2001 <- cbind(ba[[1]][,1], tt[[1]][,1], tn[[1]][,1], nr[[1]][,1], za[[1]][,1], bb[[1]][,1], po[[1]][,1], ke[[1]][,1])
ocakavana_dlzka_zivota_2001 <- data.frame(x = c("0", "1 - 4", "5 - 9", "10 - 14", "15 - 19", "20 - 24", "25 - 29",
                                       "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64",
                                       "65 - 69", "70 - 74", "75 - 79", "80 - 84", "85 - 89", "90 - 94", "95+"), ocakavana_dlzka_zivota_2001)
colnames(ocakavana_dlzka_zivota_2001) <- c('age', 'ba', 'tt', 'tn', 'nr', 'za', 'bb', 'po', 'ke')
ocakavana_dlzka_zivota_2001[,-1] <- round(ocakavana_dlzka_zivota_2001[,-1],2)
write.csv(ocakavana_dlzka_zivota_2001, file = 'D:/PRACA/CLANKY/alzheimer/data/grafy/ocakavana_dlzka_zivota.csv')
### tabulka ocakavana dlzka zivota za rok 2015
ocakavana_dlzka_zivota_2015 <- cbind(ba[[1]][,15], tt[[1]][,15], tn[[1]][,15], nr[[1]][,15], za[[1]][,15], bb[[1]][,15], po[[1]][,15], ke[[1]][,15])
ocakavana_dlzka_zivota_2015 <- data.frame(x = c("0", "1 - 4", "5 - 9", "10 - 14", "15 - 19", "20 - 24", "25 - 29",
                                                "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64",
                                                "65 - 69", "70 - 74", "75 - 79", "80 - 84", "85 - 89", "90 - 94", "95+"), ocakavana_dlzka_zivota_2015)
colnames(ocakavana_dlzka_zivota_2015) <- c('age', 'ba', 'tt', 'tn', 'nr', 'za', 'bb', 'po', 'ke')
ocakavana_dlzka_zivota_2015[,-1] <- round(ocakavana_dlzka_zivota_2015[,-1],2)
write.csv(ocakavana_dlzka_zivota_2015, file = 'D:/PRACA/CLANKY/alzheimer/data/grafy/ocakavana_dlzka_zivota2015.csv')
### tabulka ocakavana dlzka zivota za rok 2001 bez alzheimera
ocakavana_dlzka_zivota_2001 <- cbind(ba[[2]][,1], tt[[2]][,1], tn[[2]][,1], nr[[2]][,1], za[[2]][,1], bb[[2]][,1], po[[2]][,1], ke[[2]][,1])
ocakavana_dlzka_zivota_2001 <- data.frame(x = c("0", "1 - 4", "5 - 9", "10 - 14", "15 - 19", "20 - 24", "25 - 29",
                                                "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64",
                                                "65 - 69", "70 - 74", "75 - 79", "80 - 84", "85 - 89", "90 - 94", "95+"), ocakavana_dlzka_zivota_2001)
colnames(ocakavana_dlzka_zivota_2001) <- c('age', 'ba', 'tt', 'tn', 'nr', 'za', 'bb', 'po', 'ke')
ocakavana_dlzka_zivota_2001[,-1] <- round(ocakavana_dlzka_zivota_2001[,-1],2)
write.csv(ocakavana_dlzka_zivota_2001, file = 'D:/PRACA/CLANKY/alzheimer/data/grafy/ocakavana_dlzka_zivota2001alz.csv')
### tabulka ocakavana dlzka zivota za rok 2015 bez alzheimera
ocakavana_dlzka_zivota_2015 <- cbind(ba[[2]][,15], tt[[2]][,15], tn[[2]][,15], nr[[2]][,15], za[[2]][,15], bb[[2]][,15], po[[2]][,15], ke[[2]][,15])
ocakavana_dlzka_zivota_2015 <- data.frame(x = c("0", "1 - 4", "5 - 9", "10 - 14", "15 - 19", "20 - 24", "25 - 29",
                                                "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64",
                                                "65 - 69", "70 - 74", "75 - 79", "80 - 84", "85 - 89", "90 - 94", "95+"), ocakavana_dlzka_zivota_2015)
colnames(ocakavana_dlzka_zivota_2015) <- c('age', 'ba', 'tt', 'tn', 'nr', 'za', 'bb', 'po', 'ke')
ocakavana_dlzka_zivota_2015[,-1] <- round(ocakavana_dlzka_zivota_2015[,-1],2)
write.csv(ocakavana_dlzka_zivota_2015, file = 'D:/PRACA/CLANKY/alzheimer/data/grafy/ocakavana_dlzka_zivota2015alz.csv')
### tabulka pgle 2001
ocakavana_dlzka_zivota_2001 <- cbind(ba[[3]][,1], tt[[3]][,1], tn[[3]][,1], nr[[3]][,1], za[[3]][,1], bb[[3]][,1], po[[3]][,1], ke[[3]][,1])
ocakavana_dlzka_zivota_2001 <- data.frame(x = c("0", "1 - 4", "5 - 9", "10 - 14", "15 - 19", "20 - 24", "25 - 29",
                                                "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64",
                                                "65 - 69", "70 - 74", "75 - 79", "80 - 84", "85 - 89", "90 - 94", "95+"), ocakavana_dlzka_zivota_2001)
colnames(ocakavana_dlzka_zivota_2001) <- c('age', 'ba', 'tt', 'tn', 'nr', 'za', 'bb', 'po', 'ke')
ocakavana_dlzka_zivota_2001[,-1] <- round(ocakavana_dlzka_zivota_2001[,-1],2)
write.csv(ocakavana_dlzka_zivota_2001, file = 'D:/PRACA/CLANKY/alzheimer/data/grafy/pgle2001.csv')
### tabulka pgle 2015
ocakavana_dlzka_zivota_2015 <- cbind(ba[[3]][,15], tt[[3]][,15], tn[[3]][,15], nr[[3]][,15], za[[3]][,15], bb[[3]][,15], po[[3]][,15], ke[[3]][,15])
ocakavana_dlzka_zivota_2015 <- data.frame(x = c("0", "1 - 4", "5 - 9", "10 - 14", "15 - 19", "20 - 24", "25 - 29",
                                                "30 - 34", "35 - 39", "40 - 44", "45 - 49", "50 - 54", "55 - 59", "60 - 64",
                                                "65 - 69", "70 - 74", "75 - 79", "80 - 84", "85 - 89", "90 - 94", "95+"), ocakavana_dlzka_zivota_2015)
colnames(ocakavana_dlzka_zivota_2015) <- c('age', 'ba', 'tt', 'tn', 'nr', 'za', 'bb', 'po', 'ke')
ocakavana_dlzka_zivota_2015[,-1] <- round(ocakavana_dlzka_zivota_2015[,-1],2)
write.csv(ocakavana_dlzka_zivota_2015, file = 'D:/PRACA/CLANKY/alzheimer/data/grafy/pgle2015.csv')
###############################################################
i = 13
ocakavana_dlzka_zivota_x <- cbind(ba[[3]][,i], tt[[3]][,i], tn[[3]][,i], nr[[3]][,i], za[[3]][,i], bb[[3]][,i], po[[3]][,i], ke[[3]][,i])
ocakavana_dlzka_zivota_x

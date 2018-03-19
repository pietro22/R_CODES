##########################################################################################################
################################ ODVRATITELNA UMRTNOST SPRACOVANIE VYSLEDKOV #############################
##########################################################################################################
### nacitanie balikov
library(reshape2)
library(ggplot2)
### nacitanie dat
load("D:/PRACA/ODVRATITELNA_UMRTNOST/new_celkova_umrtnost.RData")
load("D:/PRACA/ODVRATITELNA_UMRTNOST/new_liecitelna_umrtnost.RData")
#
### doplnenie chybajucich hodnot
celkova_umrtnost[5,17,1] <- mean(celkova_umrtnost[5,16,1], celkova_umrtnost[5,18,1])
celkova_umrtnost[38,8,1] <- mean(celkova_umrtnost[38,7,1], celkova_umrtnost[38,9,1])
celkova_umrtnost[44,20,1] <- mean(celkova_umrtnost[44,19,1], celkova_umrtnost[44,18,1])
##########################################################################################################
### graf zavislost odvratitelna vs celkova standardizovana umrtnost
sub_liecitelna_umrtnost_muzi <- matrix(NA, nc = 20, nr = 79)
colnames(sub_liecitelna_umrtnost_muzi) <- c(1996:2015)
okresy <- c("Bratislava I" = "101",
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
rownames(sub_liecitelna_umrtnost_muzi) <- okresy
for(i in 1:dim(sub_liecitelna_umrtnost_muzi)[1]) {
  for(j in 1:dim(sub_liecitelna_umrtnost_muzi)[2]) {
    temp_data <- liecitelna_umrtnost[i,j,,1]
    sub_liecitelna_umrtnost_muzi[i,j] <- sum(temp_data, na.rm = T)
  }
}
# celkova umrtnost muzi
sub_celkova_umrtnost_muzi <- celkova_umrtnost[,,1]
colnames(sub_celkova_umrtnost_muzi) <- c(1996:2015)
rownames(sub_celkova_umrtnost_muzi) <- okresy
# graf pre rok 2015 muzi
plot_data <- data.frame(district = okresy, AM = sub_liecitelna_umrtnost_muzi[,20], SMR = sub_celkova_umrtnost_muzi[,20])
m.plot_data <- melt(plot_data, id.vars = 'district')
theme_set(theme_bw())
my_plot <- ggplot(plot_data, aes(x = SMR, y = AM, label = district)) + 
  geom_point() + 
  geom_text(hjust=0, vjust=1.5) +
  # labs(# title="Yearly Time Series", 
    # subtitle="Returns Percentage from Economics Dataset", 
    # caption="Source: Economics", 
    # y="Average wage in EUR", x = 'Year') +  # title and caption
  scale_x_continuous(limits = c(1200, 2600), 
                     breaks = seq(1000, 3000, 100)) +
  scale_y_continuous(limits = c(150, 500), breaks = seq(0, 1000, 50)) + # change to monthly ticks and labels
  theme(axis.text.x = element_text(angle = 90, vjust=0.5),  # rotate x axis text
        panel.grid.minor = element_blank())  # turn off minor grid
my_plot
ggsave('D:/PRACA/ODVRATITELNA_UMRTNOST/ivka/liecitelna_muzi_2015.png', my_plot, width = 8, height = 8, units = 'in')

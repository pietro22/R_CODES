all_data <- read.csv2("databases/stredny_stav_okresy/all_data.csv")
test <- c("Bratislava I" = "101",
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
test1 <- test[order(names(test))]
noveLevely <- c("Banskobystrický kraj" = '6',
                "Bratislava" = '11',
                "Bratislavský kraj" = '1',
                "Košice" = '81',
                "Košický kraj" = '8',
                "Nitriansky kraj" = '4',
                test1,
                "Prešovský kraj" = '7',
                "Slovenská republika" = '0',
                "Trenčiansky kraj" = '3',
                "Trnavský kraj" = '2',
                "Žilinský kraj" = '5')

levels(all_data$district) <- noveLevely
write.csv2(all_data, file = 'D:/PRACA/DATABAZA_MRTVYCH/databaza_mrtvych_v_3_1/databases/stredny_stav_okresy/new_all_data.csv')


[1] "Banskobystrický kraj"       "Bratislava"                 "Bratislavský kraj"          "Košice"                     "Košický kraj"              
[6] "Nitriansky kraj"            "Okres Bánovce nad Bebravou" "Okres Banská Bystrica"      "Okres Banská Štiavnica"     "Okres Bardejov"            
[11] "Okres Bratislava I"         "Okres Bratislava II"        "Okres Bratislava III"       "Okres Bratislava IV"        "Okres Bratislava V"        
[16] "Okres Brezno"               "Okres Bytča"                "Okres Čadca"                "Okres Detva"                "Okres Dolný Kubín"         
[21] "Okres Dunajská Streda"      "Okres Galanta"              "Okres Gelnica"              "Okres Hlohovec"             "Okres Humenné"             
[26] "Okres Ilava"                "Okres Kežmarok"             "Okres Komárno"              "Okres Košice - okolie"      "Okres Košice I"            
[31] "Okres Košice II"            "Okres Košice III"           "Okres Košice IV"            "Okres Krupina"              "Okres Kysucké Nové Mesto"  
[36] "Okres Levice"               "Okres Levoča"               "Okres Liptovský Mikuláš"    "Okres Lučenec"              "Okres Malacky"             
[41] "Okres Martin"               "Okres Medzilaborce"         "Okres Michalovce"           "Okres Myjava"               "Okres Námestovo"           
[46] "Okres Nitra"                "Okres Nové Mesto nad Váhom" "Okres Nové Zámky"           "Okres Partizánske"          "Okres Pezinok"             
[51] "Okres Piešťany"             "Okres Poltár"               "Okres Poprad"               "Okres Považská Bystrica"    "Okres Prešov"              
[56] "Okres Prievidza"            "Okres Púchov"               "Okres Revúca"               "Okres Rimavská Sobota"      "Okres Rožňava"             
[61] "Okres Ružomberok"           "Okres Sabinov"              "Okres Senec"                "Okres Senica"               "Okres Skalica"             
[66] "Okres Snina"                "Okres Sobrance"             "Okres Spišská Nová Ves"     "Okres Stará Ľubovňa"        "Okres Stropkov"            
[71] "Okres Svidník"              "Okres Šaľa"                 "Okres Topoľčany"            "Okres Trebišov"             "Okres Trenčín"             
[76] "Okres Trnava"               "Okres Turčianske Teplice"   "Okres Tvrdošín"             "Okres Veľký Krtíš"          "Okres Vranov nad Topľou"   
[81] "Okres Zlaté Moravce"        "Okres Zvolen"               "Okres Žarnovica"            "Okres Žiar nad Hronom"      "Okres Žilina"              
[86] "Prešovský kraj"             "Slovenská republika"        "Trenčiansky kraj"           "Trnavský kraj"              "Žilinský kraj"


colnames(all_data) <- c('distric', 'sex', 'age', 1996:2016)
library(reshape2)
m_all_data <- melt(all_data, id.vars = c('distric', 'sex', 'age'))
new_all_data <- acast(m_all_data, age ~ variable ~ distric ~ sex)
save(new_all_data, file = 'D:/PRACA/DATABAZA_MRTVYCH/databaza_mrtvych_v_3_1/databases/stredny_stav_okresy/new_all_data.RData')




dat <- data.frame(a = rep(letters[1:3], 2), b = rep(letters[1:2], 3), c = c(rep("a", 5), "b"), x = rnorm(6), stringsAsFactors = FALSE)

l <- by(dat[ , "x"], dat[ , 1:3], mean)

l["a", "a", "a"] # works  
l[c("a", "a", "a")] # does not work



library(hhh4contacts)
subset(counts, 1, 4:7)


mat <- array(1:81, dim=c(3, 3, 3, 3))
apply(mat, c(1,2,4), sum)

library(reshape2)
library(ggplot2)
publikacie_2017 <- data.frame(typ = c('vedecke_monografie_zahranicne',
                                 'vedecke_monografie_domace',
                                 'kap_vo_ved_monografii_zahranicna',
                                 'kap_vo_ved_monografii_domaca',
                                 'karent_zahranicny',
                                 'karent_domaci',
                                 'indexovane',
                                 'nekarent_zahranicny',
                                 'nekarent_domaci',
                                 'odborne_nekarent_zahranicny',
                                 'odborne_nekarent_domaci'),
                         KAMaHI = c(0,0,0.5,0,3,0,11.3,2,1,0,0),
                         KBaI = c(0,0,2.4,0,0,1.8,28.8,1.5,1.5,0,0),
                         KET = c(0,0,2.4,0,0,0,8.1,1.0,7.8,0,0),
                         KF = c(0,0,0.3,0,4,2.2,20.5,7.5,7.7,0,0),
                         KRVaM = c(0,0,1.4,0,2,2,3.3,0,0,0,0))
publikacie_2016 <- data.frame(typ = c('vedecke_monografie_zahranicne',
                                       'vedecke_monografie_domace',
                                       'kap_vo_ved_monografii_zahranicna',
                                       'kap_vo_ved_monografii_domaca',
                                       'karent_zahranicny',
                                       'karent_domaci',
                                       'indexovane',
                                       'nekarent_zahranicny',
                                       'nekarent_domaci',
                                       'odborne_nekarent_zahranicny',
                                       'odborne_nekarent_domaci'),
                               KAMaHI = c(0,0,0,0,3.5,0,3,1,2.5,0,0),
                               KBaI = c(0,0,1,0,1.5,0,17.3,3,5,0,0),
                               KET = c(0,0,0,0,1,1,5,3.5,11.5,0,0),
                               KF = c(0,0,0,0,1,0,24.7,9.5,8,0,1),
                               KRVaM = c(0,1,0,0,3,1,7,3,0,0,0))
sum_publikacie_2017 <- data.frame(typ = c('ostatne_vedecke_prace',
                                          'indexovane_casopisy',
                                          'karenty',
                                          'monografie'),
                                  KAMaHI = c(3,11.3,3,0.5),
                                  KBaI = c(3,28.8,1.8,2.4),
                                  KET = c(8.8,8.1,0,2.4),
                                  KF = c(15.2,20.5,6.2,0.3),
                                  KRVaM = c(0,3.3,4,1.4)
                                  )
### struktura publikacii podla ich typu
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
m_sum_publikacie_2017 <- melt(sum_publikacie_2017, id.vars = 'typ')
colnames(m_sum_publikacie_2017) <- c('typ', 'katedra', 'počet')
levels(m_sum_publikacie_2017[,1]) <- c("indexované časopisy", "karenty", "monografie", "ostatné vedecké práce")
data_labels <- m_sum_publikacie_2017[which(m_sum_publikacie_2017[,3] != 0),]
data_labels <-data_labels[order(data_labels$katedra,decreasing=T),]
my_plot <- ggplot(m_sum_publikacie_2017[order(m_sum_publikacie_2017$katedra,decreasing=T),], aes(x = typ, y = počet)) + 
  geom_bar(stat = "identity", aes(fill = katedra), width = 0.35) + 
  coord_flip() +
  geom_text(data = data_labels, aes(label = počet), position = position_stack(vjust = 0.5), cex = 3) +
  theme(legend.position="bottom",
        axis.title.y=element_blank()) +
  scale_y_continuous(limits = c(0, 80), breaks = seq(0, 80, 10)) +
  scale_fill_manual(values=cbPalette)
my_plot
ggsave('D:/PRACA/BORDEL/sprava_vedecka_cinnost/struktura_publikacia_typ_2017.png', my_plot, width = 160, height = 140, units = 'mm')
### struktura publikacii podla katedier
cbPalette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
m_sum_publikacie_2017 <- melt(sum_publikacie_2017, id.vars = 'typ')
colnames(m_sum_publikacie_2017) <- c('typ', 'katedra', 'počet')
levels(m_sum_publikacie_2017[,1]) <- c("indexované časopisy", "karenty", "monografie", "ostatné vedecké práce")
data_labels <- m_sum_publikacie_2017[which(m_sum_publikacie_2017[,3] != 0),]
data_labels <-data_labels[order(data_labels$typ,decreasing=T),]
my_plot <- ggplot(m_sum_publikacie_2017[order(m_sum_publikacie_2017$typ,decreasing=T),], aes(x = katedra, y = počet)) + 
  geom_bar(stat = "identity", aes(fill = typ), width = 0.35) + 
  coord_flip() +
  geom_text(data = data_labels, aes(label = počet), position = position_stack(vjust = 0.5), cex = 3) +
  theme(legend.position="bottom",
        axis.title.y=element_blank()) +
  scale_y_continuous(limits = c(0, 50), breaks = seq(0, 80, 10)) +
  scale_fill_manual(values=cbPalette)
my_plot
ggsave('D:/PRACA/BORDEL/sprava_vedecka_cinnost/struktura_publikacia_katedra_2017.png', my_plot, width = 160, height = 140, units = 'mm')
### celkovy pocet publikacii
celkove_publikacie <- data.frame(katedra = rep(levels(m_sum_publikacie_2017[,2]),2),
                                 rok = rep(c(2017,2016), each = 5),
                                 počet = c(17.8,33.6,16.9,41.9,7.3,))
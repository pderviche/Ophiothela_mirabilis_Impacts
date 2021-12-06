#The effects of the nonnative brittle star Ophiothela mirabilis Verrill, 1867 on the feeding performance of an octocoral host in a southwestern Atlantic rocky shore 
#Patrick Derviche and Paulo da Cunha Lana 

#Packages
library(ggplot2)
library(ggpubr)

#Figure 7
#Comparison of a the feeding rate (particles g DW-1 h -1 ; means ± standard error) and b the heterotrophic carbon input (µg C g DW-1 h -1 ) of the Leptogorgia punicea hosts under high natural colonization of Ophiothela mirabilis (treatment) and the hosts naturally without brittle star individuals (host control) over time
data <- read.csv(file="feedingtimes.csv",header=TRUE,sep=";",dec=".")
datanumber <- data[,1:6]
datacarbon <- data[,c(1:2,7:10)]

pd <- position_dodge(0.3) # move them .05 to the left and right

legFR <- expression(atop(Feeding~rate,(particles~g~DW^-1~h^-1)))
legHCI <- expression(atop(Heterotrophic~carbon~input,(µg~C~g~DW^-1~h^-1)))

FR <- ggplot(datanumber, aes(x=time, y=particles, colour=group)) + 
  theme_minimal(base_size = 22)+
  geom_errorbar(aes(ymin=particles-se, ymax=particles+se), width=.3,position=pd, size= 1) +
  geom_point(position=pd, size =3)+
  xlab("Time (hours)") + ylab(legFR)+
  scale_color_manual(values=c('steelblue','indianred'),name=" ")+
  theme(legend.text = element_text(color ="black", size = 22))+
  theme(axis.text.x = element_text(color="black",size=22))+
  theme(axis.text.y = element_text(color="black",size=22))+ 
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

HCI <- ggplot(datacarbon, aes(x=time, y=carbon, colour=group)) + 
  theme_minimal(base_size = 22)+
  geom_errorbar(aes(ymin=carbon-sec, ymax=carbon+sec), width=.3,position=pd, size= 1) +
  geom_point(position=pd, size =3)+
  xlab("Time (hours)") + ylab(legHCI)+
  scale_color_manual(values=c('steelblue','indianred'),name=" ")+
  theme(legend.text = element_text(color ="black", size = 22))+
  theme(axis.text.x = element_text(color="black",size=22))+
  theme(axis.text.y = element_text(color="black",size=22))+ 
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

Fig7 <- ggarrange(FR, HCI,  
                  common.legend = TRUE, legend = "right",
                  labels = c("a","b"),
                  font.label = list(size = 22, color = "black", face = "bold", family = NULL),
                  ncol = 2, nrow = 1)
Fig7

ggsave(Fig7, file="Fig7.png", width=15, height=5.2, dpi=1500)

#End

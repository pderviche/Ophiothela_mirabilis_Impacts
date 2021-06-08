#The effects of the nonnative brittle star Ophiothela mirabilis Verrill, 1867 on the feeding performance of an octocoral host in a southwestern Atlantic rocky shore 
#Patrick Derviche and Paulo da Cunha Lana 

#Packages
library(ggplot2)
library(ggpubr)

#Figure 6
data <- read.csv(file="feedingrates.csv",header=TRUE,sep=";",dec=".")

legFR <- expression(atop(Feeding~rate,(particles~g~DW^-1~h^-1)))
legHCI <- expression(atop(Heterotrophic~carbon~input,(µg~C~g~DW^-1~h^-1)))


FR<- ggplot(data, aes(x=group, y=fr)) + 
  geom_boxplot(fill="slateblue", alpha=0.2, width=.25) + 
  theme_minimal(base_size = 22)+
  xlab(" ") + ylab(legFR)+
  scale_y_continuous(breaks=seq(0,4000000,1000000))+
  theme(axis.text.x = element_text(color="black",size=22))+
  theme(axis.text.y = element_text(color="black",size=22))+ 
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))


HCI<- ggplot(data, aes(x=group, y=hci)) + 
  geom_boxplot(fill="slateblue", alpha=0.2, width=.25) + 
  theme_minimal(base_size = 22)+
  xlab(" ") + ylab(legHCI)+
  scale_y_continuous(breaks=seq(0,600,100))+
  theme(axis.text.x = element_text(color="black",size=22))+
  theme(axis.text.y = element_text(color="black",size=22))+ 
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

Fig6 <- ggarrange(FR, HCI,  
                  labels = c("a", "b"),
                  font.label = list(size = 22, color = "black", face = "bold", family = NULL),
                  ncol = 2, nrow = 1)
Fig6


ggsave(Fig6, file="Fig6.png", width=17.4, height=7, dpi=1500)
#END
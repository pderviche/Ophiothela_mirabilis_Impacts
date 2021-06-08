#The effects of the nonnative brittle star Ophiothela mirabilis Verrill, 1867 on the feeding performance of an octocoral host in a southwestern Atlantic rocky shore 
#Patrick Derviche and Paulo da Cunha Lana 

#Packages
library(ggplot2)
library(ggpubr)

#Figure 9
data <- read.csv(file="sweptclear.csv",header=TRUE,sep=";",dec=".")
datanumber <- data[,1:6]
datacarbon <- data[,c(1:2,7:10)]

pd <- position_dodge(0.3) # move them .05 to the left and right
legparticle <- expression(atop(Particle~concentration,(particles~mL^-1)))
legcarbon <- expression(atop(Carbon~content,(µg~C~mL^-1)))

particle <- ggplot(datanumber, aes(x=time, y=particles, colour=group)) + 
  theme_minimal(base_size = 22)+
  geom_errorbar(aes(ymin=particles-se, ymax=particles+se), width=.3,position=pd, size =1) +
  geom_line(position=pd) +
  geom_point(position=pd, size =3)+
  xlab("Sampling time (hours)") + ylab(legparticle)+
  scale_color_manual(values=c('lightgreen','steelblue','indianred'),name=" ")+
  theme(legend.text = element_text(color ="black", size = 22))+
  theme(axis.text.x = element_text(color="black",size=22))+
  theme(axis.text.y = element_text(color="black",size=22))+ 
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

carbon <- ggplot(datacarbon, aes(x=time, y=carbon, colour=group)) + 
  theme_minimal(base_size = 22)+
  geom_errorbar(aes(ymin=carbon-sec, ymax=carbon+sec), width=.3,position=pd, size =1) +
  geom_line(position=pd) +
  geom_point(position=pd, size =3)+
  xlab("Sampling time (hours)") + ylab(legcarbon)+
  scale_color_manual(values=c('lightgreen','steelblue','indianred'),name=" ")+
  theme(legend.text = element_text(color ="black", size = 22))+
  theme(axis.text.x = element_text(color="black",size=22))+
  theme(axis.text.y = element_text(color="black",size=22))+ 
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

Fig9 <- ggarrange(particle, carbon,  
              common.legend = TRUE, legend = "right",
              labels = c("a", "b"),
              font.label = list(size = 22, color = "black", face = "bold", family = NULL),
              ncol = 2, nrow = 1)
Fig9

ggsave(Fig9, file="Fig9.png", width=17.4, height=6, dpi=1500)
#END
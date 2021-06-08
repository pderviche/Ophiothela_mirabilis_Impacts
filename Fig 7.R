#The effects of the nonnative brittle star Ophiothela mirabilis Verrill, 1867 on the feeding performance of an octocoral host in a southwestern Atlantic rocky shore 
#Patrick Derviche and Paulo da Cunha Lana 

#Packages
library(ggplot2)

#Figure 7
data <- read.csv(file="particles.csv",header=TRUE,sep=";",dec=".")

legparticle <- expression(Particle~concentration~(particles~mL^-1))
legcarbon <- expression(Carbon~content~(µg~C~mL^-1))

Fig7 <-ggplot(data, aes(x=particles, y=carbon)) + 
  geom_point()+
  theme_minimal(base_size = 22)+
  geom_smooth(method=glm , color="steelblue", fill="#69b3a2", se=TRUE)+
  xlab(legparticle) + ylab(legcarbon)+
  scale_y_continuous(breaks=seq(0.0,0.8,0.1))+
  scale_x_continuous(breaks=seq(5000,17000,2000))+
      theme(axis.text.x = element_text(color="black",size=22,angle = -45))+
  theme(axis.text.y = element_text(color="black",size=22))+ 
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

Fig7

ggsave(Fig7, file="Fig7.png", width=8.4, height=7, dpi=1500)
#END
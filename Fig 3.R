#The effects of the nonnative brittle star Ophiothela mirabilis Verrill, 1867 on the feeding performance of an octocoral host in a southwestern Atlantic rocky shore 
#Patrick Derviche and Paulo da Cunha Lana 

#Package
library(ggplot2)
library(ggpubr)

#Figure 3
#Relationship between a the dry weight (g) and area (cm2 ) of Leptogorgia punicea and between b the dry weight (g) and the number of individuals of Ophiothela mirabilis. Black dots are raw data, and lines and shaded areas represent GLM predictions ± standard error
data <- read.csv(file="dw.csv",header=TRUE,sep=";",dec=".")

data1<-subset(data, select = c(species, group, replicate,dw, area))
data1 <- na.omit(data1)

data2<-subset(data, select = c(species, replicate,dw, n))
data2 <- na.omit(data2)

#Leptogorgia punicea
DWocto <- ggplot(data1, aes(x=dw, y=area)) + 
  geom_point() +
  theme_minimal(base_size = 22)+
  xlab("Dry-weight (g)") + ylab("Area (cm²)")+
  geom_smooth(method=glm , color="steelblue", fill="#69b3a2", se=TRUE)+
  theme(axis.text.x = element_text(color="black",size=22))+
  theme(axis.text.y = element_text(color="black",size=22))+ 
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

#Ophiothela mraibilis
DW.ophi <- ggplot(data2, aes(x=dw, y=n)) + 
  geom_point() +
  theme_minimal(base_size = 22)+
  xlab("Dry-weight (g)") + ylab("Number of brittle stars")+
  geom_smooth(method=glm , color="steelblue", fill="#69b3a2", se=TRUE)+
  theme(axis.text.x = element_text(color="black",size=22))+
  theme(axis.text.y = element_text(color="black",size=22))+
  scale_x_continuous(limits = c(0.3, 0.55))+ 
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

#Product
Fig3 <- ggarrange(DWocto, DW.ophi,  
                  ncol = 1, nrow = 2)
Fig3

ggsave(Fig3, file="Fig4.png", width=17.4, height=7, dpi=1500)

#End

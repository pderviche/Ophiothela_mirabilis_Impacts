#The effects of the nonnative brittle star Ophiothela mirabilis Verrill, 1867 on the feeding performance of an octocoral host in a southwestern Atlantic rocky shore 
#Patrick Derviche and Paulo da Cunha Lana 

#Package
library(ggplot2)

#Figure 5
data <- read.csv(file="disc.csv",header=TRUE,sep=";",dec=".")
summary(data)
mean(data$diameter)
sd(data$diameter)

Fig5 <- ggplot(data, aes(x=diameter)) +
  geom_histogram( binwidth=50, fill="#69b3a2", color="#e9ecef", alpha=0.9)  +
  theme_minimal(base_size = 22)+
  theme(axis.text.x = element_text(color="black",size=22))+
  theme(axis.text.y = element_text(color="black",size=22))+
  xlab("Disc diameter (µm)") + ylab("Frequency")+ 
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

Fig5

ggsave(Fig5, file="Fig5.png", width=17.4, height=7, dpi=1500)
#END
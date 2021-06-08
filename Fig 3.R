#The effects of the nonnative brittle star Ophiothela mirabilis Verrill, 1867 on the feeding performance of an octocoral host in a southwestern Atlantic rocky shore 
#Patrick Derviche and Paulo da Cunha Lana 

#Package
library(ggplot2)

#Figure 3
data <- read.csv(file="discstate.csv",header=TRUE,sep=";",dec=".")

Fig3<- ggplot(data, aes(x = treatment, y = perc, fill = disc)) + 
  geom_bar(stat = "identity", position = position_dodge(), col = "white") +
  scale_fill_manual(values=c("steelblue","lightgreen","aquamarine"), name=" ") + 
  theme_minimal(base_size = 22)+  
  theme(axis.text.x = element_text(color="black"))+
  theme(axis.text.y = element_text(color="black",size=22))+
  xlab(" ") + ylab(expression(atop("Percentage (%)")))+ 
  theme(axis.line = element_line(colour = "black", size = 1, linetype = "solid"))

Fig3

ggsave(Fig3, file="Fig3.png", width=17.4, height=7, dpi=1500)
#END
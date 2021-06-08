#The effects of the nonnative brittle star Ophiothela mirabilis Verrill, 1867 on the feeding performance of an octocoral host in a southwestern Atlantic rocky shore 
#Patrick Derviche and Paulo da Cunha Lana 

#Package
library("car")

######### ANOVA

# 1 Feeding rates (Table 1)
data <- read.csv(file="feedingrates.csv",header=TRUE,sep=";",dec=".")

# 1.1 Shapiro-Wilk
shapiro.test(data$fr) #p-value > 0.05, we can assume the normality
qqPlot(data$fr) #As all the points fall approximately along the reference line, we can assume normality

# 1.2 Levene's Test
leveneTest(fr ~ group, data=data)  #p-value > 0.05, we can assume the homogeneity of variance 
leveneTest(fr ~ time, data=data) #p-value > 0.05, we can assume the homogeneity of variance 

# 1.3 Model
model <- aov(fr ~ group + time + group*time, data=data)
summary(model) #No significant differences
anova(model) #No significant differences

# 1.4 Akaike information criterion 
AIC(model) #AIC = 382.1529

#########

# 2 Heterotrophic carbon inputs (Table 1)
data <- read.csv(file="feedingrates.csv",header=TRUE,sep=";",dec=".")

# 2.1 Shapiro-Wilk
shapiro.test(data$hci) #p-value < 0.05, we can not assume the normality
qqPlot(data$hci) #As some of the points fall outside the reference line, we can not assume normality
data$hci <- 1/(data$hci) #We applied inverse transformation to achieve the normal distribution 
shapiro.test(data$hci) # p-value = 0.05, is close to normality assumption. We chose to proceed with this transformation to not use non-parametric analysis. Other transformations did not assume normality or homogeneity of variance (e.g. sqrt(x); log10(x))

# 2.2 Levene's Test
leveneTest(hci ~ group, data=data) #p-value > 0.05, we can assume the homogeneity of variance
leveneTest(hci ~ time, data=data) #p-value > 0.05, we can assume the homogeneity of variance

# 2.3 Model
model <- aov(hci ~ group + time + group*time, data=data)
summary(model) #No significant differences
anova(model) #No significant differences

# 2.4 Akaike information criterion 
AIC(model) #AIC = -43.26106

#########

# 3 Swept clear of the seston (Table 1)
data <- read.csv(file="sweptclearoftheseston.csv",header=TRUE,sep=";",dec=".")
data$group <- gsub("treatment", "octocoral", data$group)
data$group <- gsub("host control", "octocoral", data$group)

# 3.1 Shapiro-Wilk
shapiro.test(data$particle) #p-value < 0.05, we can not assume the normality
qqPlot(data$particle) #As some of the points fall outside the reference line, we can not assume normality
data$particle <- sqrt(max(data$particle+1) - data$particle) #We applied square-root transformation to achieve the normal distribution 
shapiro.test(data$particle) #p-value > 0.05, we can assume the normality

# 3.2 Levene's Test
leveneTest(particle ~ group, data=data) #p-value > 0.05, we can assume the homogeneity of variance
leveneTest(particle ~ time, data=data) #p-value > 0.05, we can assume the homogeneity of variance
leveneTest(particle ~ group*time, data=data) #p-value > 0.05, we can assume the homogeneity of variance

# 2.3 Model
model <- aov(particle ~ group + time + group*time, data=data)
summary(model) #Significant differences
anova(model) #Significant differences

# 2.4 Post hoc
TukeyHSD(model)

# 2.5 Akaike information criterion 
AIC(model) #AIC = 239.5142

#########

# 4 Comparision of the concentration of particles at time zero within groups
data <- read.csv(file="particles.csv",header=TRUE,sep=";",dec=".")
data <- data[c(1:9),]#We only selected data from the beginning of the experiment

# 4.1 Shapiro-Wilk
shapiro.test(data$particles) #p-value > 0.05, we can assume the normality
shapiro.test(data$carbon) #p-value > 0.05, we can assume the normality

# 4.2 Levene's Test
leveneTest(particles ~ group, data=data)  #p-value > 0.05, we can assume the homogeneity of variance
leveneTest(carbon ~ group, data=data)  #p-value > 0.05, we can assume the homogeneity of variance

# 4.3 Model
model <- aov(particles ~ group, data=data)
summary(model) #No significant differences
anova(model) #No significant differences
#The concentration of particles at time zero within groups was not significantly different

model <- aov(carbon ~ group, data=data)
summary(model) #No significant differences
anova(model) #No significant differences
#The carbon content at time zero within groups was not significantly different

#END
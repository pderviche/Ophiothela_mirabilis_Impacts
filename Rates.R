#The effects of the nonnative brittle star Ophiothela mirabilis Verrill, 1867 on the feeding performance of an octocoral host in a southwestern Atlantic rocky shore 
#Patrick Derviche and Paulo da Cunha Lana 

#Package
library("dplyr")

# 1. Feeding rates and Heterotrophic carbon input
data <- read.csv(file="feedingrates.csv",header=TRUE,sep=";",dec=".")

datafr<-data[,c(1:4)]
datafr<-na.omit(datafr)

datahci<-data[,c(1:3,5)]
datahci<-na.omit(datahci)

# 1.1. Overall
mean(datafr$fr) 
sd(datafr$fr) #Feeding rates: 3,047,118 ± 1,843,183 particles mL-1 DW-1 h-1 
mean(datahci$hci) 
sd(datahci$hci) #Heterotrophic carbon input: 116.1 ± 159.0 µgC gDW-1 h-1

# 1.2. Treatment
datafr.t <- filter(datafr, group == "Treatment")
datahci.t <- filter(datahci, group == "Treatment")

mean(datafr.t$fr)
sd(datafr.t$fr) #Feeding rates: 3,029,699 ± 1,730,106 particles mL-1 DW-1 h-1 
mean(datahci.t$hci)
sd(datahci.t$hci) #Heterotrophic carbon input: 163.0 ± 214.7 µgC gDW-1 h-1

# 1.3. Host control
datafr.hc <- filter(datafr, group == "Host control")
datahci.hc <- filter(datahci, group == "Host control")

mean(datafr.hc$fr)
sd(datafr.hc$fr) #Feeding rates: 3,062,049 ± 2,073,571 particles mL-1 AFDW-1 h-1 
mean(datahci.hc$hci)
sd(datahci.hc$hci) #Heterotrophic carbon input: 69.2 ± 72.7 µgC gDW-1 h-1

# 2.Grazing rate
dens <- 8.33 #colonies m-2 (data from Derviche et al 2021)
dw <- 2.15 #g DW colony-1 (mean)
biomass <-dens*dw #g DW m-2

# 2.1. Overall
(mean(datahci$hci)*biomass)*24/1000
(sd(datahci$hci)*biomass)*24/1000 # 49.9 ± 68.3 mg C m-2 day-1

# 1.2. Treatment
(mean(datahci.t$hci)*biomass)*24/1000
(sd(datahci.t$hci)*biomass)*24/1000 # 70.1 ± 92.3 mg C m-2 day-1

# 1.3. Host control
(mean(datahci.hc$hci)*biomass)*24/1000
(sd(datahci.hc$hci)*biomass)*24/1000 # 29.7 ± 31.2 mg C m-2 day-1

#End
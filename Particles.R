#The effects of the nonnative brittle star Ophiothela mirabilis Verrill, 1867 on the feeding performance of an octocoral host in a southwestern Atlantic rocky shore 
#Patrick Derviche and Paulo da Cunha Lana 

#package
library("dplyr")
library("plyr")
library("tidyverse")

# 1.Creating file 'particles.csv'
# Read the raw data
data <- read.csv(file="rawdata.csv",header=TRUE,sep=";",dec=".")
data2 <- read.csv(file="rawdata.csv",header=FALSE,sep=";",dec=".")
data$time <- as.factor(data$time)
data$replicate <- as.factor(data$replicate)
str(data)

# 1.1. Generalizing particle diameter as spherical
info <- data[,c(1:3)]
diameter <- data2[1,c(4:780)]
particles <- data[,c(4:780)]

spherical <- 4/3*3.14*(diameter/2)^3
names(spherical) <- diameter[,]
dim(spherical)

# 1.2. Applying the conversion factors 

spherical.3to19  <- spherical[,c(1:349)]
particles.3to19  <- particles[,c(1:349)]

spherical.20to120  <- spherical[,c(350:777)]
particles.20to120  <- particles[,c(350:777)]

Verity <- 0.433*spherical.3to19^0.863 #for particles < 19.99 µm, Verity et al. 1992
Montagnes <- 0.109*spherical.20to120^0.991 #for particles > 20.00 µm, Montagnes et al. 1994

# 1.3. Multiplycating the carbon content by the number of particles

Verity <- Verity[rep(seq_len(nrow(Verity)), times = 36), ]
Verityc <- Verity*particles.3to19

Montagnes <- Montagnes[rep(seq_len(nrow(Montagnes)), times = 36), ]
Montagnesc <- Montagnes*particles.20to120

carbon <- cbind (Verityc,Montagnesc)
dim(carbon)

carbon.info <- cbind(info,carbon)
dim(carbon.info)

# 1.3. Creating a dataframe
abstract <- data.frame(carbon.info[,1:3], carbon=rowSums(carbon.info[,-(1:3)]))
abstract$carbon<-abstract$carbon/1000000 #tranforming data from pg C to ug C
abstract <- subset(abstract,select = c(carbon)) #values in ug C ml-1
abstract2 <- data.frame(data[,1:3], particles=rowSums(data[,-(1:3)])) #values in number of particles mL-1

particles <- cbind (abstract2,abstract)

#Save csv
write.csv(particles, "particles.csv",sep = ",", row.names = FALSE)

#END
install.packages("tidyverse")
install.packages ("jsonlite")
library(jsonlite)
library(tidyverse)

MechaCar_mpg <- read.csv("MechaCar_mpg.csv", header = TRUE, sep = ",", quote = "\"", check.names = F, stringsAsFactors = F)
#--------------------------------------------------

# Multiple linear regression 

lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance,data=MechaCar_mpg) #generate multiple linear regression model


summary(lm(mpg ~ vehicle_length + vehicle_weight + spoiler_angle + ground_clearance,data=MechaCar_mpg)) #generate summary statistics
#________________________________________________________________________

#2 Suspension Coil Summary

scoil <- read.csv("Suspension_Coil.csv", header = TRUE, sep = ",", quote = "\"", check.names = F, stringsAsFactors = F)
head(scoil)

# 1) create a Summary table of statistics for PSI coil
scoil_stats <- scoil %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),var_PSI=var(PSI), standard_dev_PSI=sd(PSI)) #create summary table with multiple columns


#2) There are three lots of Manufacturing lots in the datasets.  While overall shows variation, we may want to know from each lot.
scoil_by_lot1 <- scoil %>% group_by(condition) %>% summarize(Mean_Mileage=mean(odometer)) #create summary table

Lot1 <- subset(scoil,Manufacturing_Lot=="Lot1") %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),var_PSI=var(PSI), standard_dev_PSI=sd(PSI)) #create summary table
Lot2 <- subset(scoil,Manufacturing_Lot=="Lot2") %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),var_PSI=var(PSI), standard_dev_PSI=sd(PSI)) #create summary table
Lot3 <- subset(scoil,Manufacturing_Lot=="Lot3") %>% group_by(Manufacturing_Lot) %>% summarize(Mean_PSI=mean(PSI),Median_PSI=median(PSI),var_PSI=var(PSI), standard_dev_PSI=sd(PSI)) #create summary table


#------------------------------------------------------
# T-test
#compare sample versus population means

# 1 Step Determine Distribution - Is it Normal or not.  
ggplot(scoil,aes(x=PSI))+ geom_density() # visualize distribution

shapiro.test(scoil$PSI)

# Null testing 

t.test(scoil$PSI, mu= 1500)

#_________________________________________________________________________________________________________



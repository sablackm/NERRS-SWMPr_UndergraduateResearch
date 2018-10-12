rm(list = ls())
# remove.packages(c("ggplot2", "plyr","dplyr", "reshape2", "lubridate", "SWMPr", "tidyverse"))
# install.packages('ggplot2', dependencies = TRUE)
# install.packages('plyr', dependencies = TRUE)
# install.packages('dplyr', dependencies = TRUE)
# install.packages('reshape2', dependencies = TRUE)
# install.packages('lubridate', dependencies = TRUE)
# install.packages('SWMPr', dependencies = TRUE)
# install.packages('tidyverse', dependencies = TRUE)
#install.packages('jtools')
#install.packages("fmsb")

#install.packages("car")
library('car')
library("ggplot2")
library("plyr")
library("tidyverse")
library("dplyr")
library("SWMPr")
library("reshape2")
library("lubridate")
library("gridExtra")
library("data.table")
library("jtools")
library("fmsb")
library("RColorBrewer")
library("grid")
#Importing Marshy Marsh Creek Data:

gulfNames <- c("gndbhwq", "gndbcwq", "apacpwq", "apadbwq", "apaebwq", "apaeswq", "apapcwq", "gndblwq", "gndpcwq", "apalmwq")

for(i in 1:length(gulfNames)){

#path <- "C:\\Users\\sabla\\Documents\\Research\\Download5_Current\\GulfofMexico"
path <- "/Users/samuelblackman/Desktop/Research/NERRS/GulfOfMexico"
sitename <- gulfNames[i]
data_collected <- import_local(path, sitename, trace = FALSE)
bh <- qaqc(data_collected)

bh_test <-bh
bh_test$datetimestamp <- force_tz(bh_test$datetimestamp, tzone ="EST")

#For Missing Values gnd_bh
#DO
missing_bh_do <- round(sum(is.na(bh_test$do_pct))/nrow(bh_test)*100,2)
missing_bh_do

#Organizing Monthly and Seasonal Sets within the dataframe
site_analyzed<- bh_test[bh_test$datetimestamp>='2007-01-01 00:00' & bh_test$datetimestamp<='2017-12-31 23:45',]
Sitecode <- rep('gndbhwq',nrow(site_analyzed))
site_analyzed <- cbind(site_analyzed, Sitecode)

diel <- format(as.POSIXct(site_analyzed$datetimestamp, format="%Y-%m-%d H:M"), "%Y-%m-%d")
site_analyzed <-cbind(site_analyzed,diel)

monthly<- format(as.POSIXct(site_analyzed$datetimestamp,format="%Y-%m-%d H:M"), "%Y-%m")
site_analyzed<-cbind(site_analyzed,monthly)

site_analyzed <- site_analyzed %>%
  mutate(Month = month(datetimestamp)) %>%
  mutate(Season = ifelse(Month == 1 | Month == 2, paste("Winter", year(datetimestamp), sep=" "),ifelse(Month == 12, paste("Winter", (year(datetimestamp)+1), sep=" "), ifelse(Month == 3 | Month == 4 | Month == 5, paste("Spring", year(datetimestamp), sep=" "),ifelse(Month == 6 | Month == 7 | Month == 8, paste("Summer", year(datetimestamp), sep=" "),ifelse(Month == 9 | Month == 10 | Month == 11, paste("Fall", year(datetimestamp), sep=" "),NA)))))) %>%
  mutate(Year = year(datetimestamp))

# #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# southANames <- c("sapdcwq", "acefcwq", "acespwq", "gtmpcwq", "niwolwq", "niwtawq", "nocrcwq", "acemcwq", "gtmfmwq", "gtmpiwq", "gtmsswq", "niwcbwq", "niwdcwq", "nocecwq", "noclcwq", "noczbwq", "sapcawq", "saphdwq", "sapldwq")
#
# for(i in 1:length(southANames)){
# #path <- "C:\\Users\\sabla\\Documents\\Research\\Download5_Current\\SouthAtlantic"
# path <- "/Users/samuelblackman/Desktop/Research/NERRS/SouthAtlantic"
# sitename <- southANames[i]
# print(southANames[i])
# data_collected <- import_local(path, sitename, trace = FALSE)
# dc <- qaqc(data_collected)
#
#
# site_analyzed<- dc[dc$datetimestamp>='2007-01-01 00:00' & dc$datetimestamp<='2017-12-31 23:45',]
# Sitecode <- rep('sapdcwq',nrow(site_analyzed))
# site_analyzed <- cbind(site_analyzed, Sitecode)
#
# diel <- format(as.POSIXct(site_analyzed$datetimestamp, format="%Y-%m-%d H:M"), "%Y-%m-%d")
# site_analyzed <-cbind(site_analyzed,diel)
#
# monthly<- format(as.POSIXct(site_analyzed$datetimestamp,format="%Y-%m-%d H:M"), "%Y-%m")
# site_analyzed<-cbind(site_analyzed,monthly)
#
# site_analyzed <- site_analyzed %>%
#   mutate(Month = month(datetimestamp)) %>%
#   mutate(Season = ifelse(Month == 1 | Month == 2, paste("Winter", year(datetimestamp), sep=" "),ifelse(Month == 12, paste("Winter", (year(datetimestamp)+1), sep=" "), ifelse(Month == 3 | Month == 4 | Month == 5, paste("Spring", year(datetimestamp), sep=" "),ifelse(Month == 6 | Month == 7 | Month == 8, paste("Summer", year(datetimestamp), sep=" "),ifelse(Month == 9 | Month == 10 | Month == 11, paste("Fall", year(datetimestamp), sep=" "),NA)))))) %>%
#   mutate(Year = year(datetimestamp))



#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

#Adding STD
calculations_monthly <- site_analyzed %>% group_by(monthly) %>% summarise(monthly_STD = sd(do_mgl, na.rm=TRUE), monthly_AVG = mean(do_mgl,na.rm=TRUE)) %>% mutate(monthly_CV=(monthly_STD/monthly_AVG))
site_analyzed <- merge(site_analyzed, calculations_monthly, by="monthly")

#Adding Missing DOpct% per month
DOpct_Missing <- 1:nlevels(site_analyzed$monthly)
turb_Missing<- 1:nlevels(site_analyzed$monthly)
temp_Missing <- 1:nlevels(site_analyzed$monthly)
sal_Missing <- 1:nlevels(site_analyzed$monthly)
depth_Missing <- 1:nlevels(site_analyzed$monthly)

for(i in 1:nlevels(site_analyzed$monthly)){
  
  temp_month <- calculations_monthly$monthly[i]
  temp <- site_analyzed[site_analyzed$monthly==temp_month,]
  
  DOpct_Missing[i] <- round(((sum(is.na(temp$do_pct)))/nrow(temp))*100,2)
  turb_Missing[i] <- round(((sum(is.na(temp$turb)))/nrow(temp))*100,2)
  temp_Missing[i] <- round(((sum(is.na(temp$temp)))/nrow(temp))*100,2)
  sal_Missing[i] <- round(((sum(is.na(temp$sal)))/nrow(temp))*100,2)
  depth_Missing[i] <- round(((sum(is.na(temp$depth)))/nrow(temp))*100,2)
  #print(paste(temp_month, round((1-(sum(is.na(temp$do_mgl)))/nrow(temp))*100,2), sep=': '))
}

#Adding Errors
error_month <- data.frame(calculations_monthly$monthly, DOpct_Missing, turb_Missing, temp_Missing, sal_Missing, depth_Missing)
colnames(error_month) <- c("monthly","DOpctMissing", "TurbMissing", "TempMissing", "SalMissing", "DepthMissing")
site_analyzed <- join(site_analyzed,error_month, type="left")


#Adding R squared adjusted
# rsquared_M <- 1:nlevels(site_analyzed$monthly)
# 
# for( i in 1:nlevels(site_analyzed$monthly)){
#   
#   temp_month2 <- site_analyzed[site_analyzed$monthly==calculations_monthly$monthly[i],]
#   print(i)
#   if(temp_month2$DOpctMissing<=75 && temp_month2$TempMissing<=75 && temp_month2$TurbMissing<=75 && temp_month2$DepthMissing<=75 && temp_month2$SalMissing<=75){
#     fit <- lm(temp_month2$do_pct ~  temp_month2$temp + temp_month2$depth + temp_month2$turb + temp_month2$sal, data=temp_month2)
#     #print(summary(fit))
#     rsquared_M[i] <- summary(fit)$adj.r.squared
#   }
#   else{
#     rsquared_M[i] <- NA
#   }
# }
# 
# r2_month <- data.frame(calculations_monthly$monthly, rsquared_M)
# colnames(r2_month) <- c("monthly","R^2 Adjusted")
# site_analyzed <- join(site_analyzed,r2_month, type="left")

#Plotting Raster--------------------------------------------------------------------------------------------------

pDO <-ggplot(site_analyzed, aes(x=Month, y=Year)) +
  geom_raster(aes(fill = DOpctMissing)) +
  scale_x_continuous(name="Month", breaks=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  scale_y_continuous(name="Year", breaks=c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)) + 
  scale_fill_distiller(palette = "Reds", direction = 1, limits = c(0,100)) + 
  ggtitle("DOpct") +
  theme_classic()

pTemp <-ggplot(site_analyzed, aes(x=Month, y=Year)) +
  geom_raster(aes(fill = TempMissing)) +
  scale_x_continuous(name="Month", breaks=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  scale_y_continuous(name="Year", breaks=c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)) + 
  scale_fill_distiller(palette = "Reds", direction = 1, limits = c(0,100)) + 
  ggtitle("Temperature") +
  theme_classic()

pSal <-ggplot(site_analyzed, aes(x=Month, y=Year)) +
  geom_raster(aes(fill = SalMissing)) +
  scale_x_continuous(name="Month", breaks=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  scale_y_continuous(name="Year", breaks=c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)) + 
  scale_fill_distiller(palette = "Reds", direction = 1, limits = c(0,100)) + 
  ggtitle("Salinity") +
  theme_classic()

pTurb <-ggplot(site_analyzed, aes(x=Month, y=Year)) +
  geom_raster(aes(fill = TurbMissing)) +
  scale_x_continuous(name="Month", breaks=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  scale_y_continuous(name="Year", breaks=c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)) + 
  scale_fill_distiller(palette = "Reds", direction = 1, limits = c(0,100)) + 
  ggtitle("Turbidity") +
  theme_classic()

pDepth <-ggplot(site_analyzed, aes(x=Month, y=Year)) +
  geom_raster(aes(fill = DepthMissing)) +
  scale_x_continuous(name="Month", breaks=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  scale_y_continuous(name="Year", breaks=c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)) + 
  scale_fill_distiller(palette = "Reds", direction = 1, limits = c(0,100)) + 
  ggtitle("Depth") +
  theme_classic()

pText <- textGrob(paste("Sitename: ",sitename))

setwd("/Users/samuelblackman/Desktop/Research/NERRS/Plots/Missing")
jpeg(paste(sitename, "Missing.jpg"))
  x <- grid.arrange(pText,pDO,pTemp,pSal,pTurb,pDepth, nrow=3, ncol=2)
  print(x)
dev.off()

}
# #tester-----------------------------------------------------------------------------------------------------------
#  temp_month <-calculations_monthly$monthly[24]
#  temp1 <- site_analyzed[site_analyzed$monthly==temp_month,]
#  
#  round((1-(sum(is.na(temp1$sal)))/nrow(temp))*100,2)
#  
#  fit <- lm(temp1$do_pct ~  temp1$temp + temp1$depth + temp1$turb + temp1$sal, data=temp1)
#  x<-summ(fit, vifs=TRUE)
#  x
#  VIF(fit) #Should I include? dunno (does not give individual values, only grouped)



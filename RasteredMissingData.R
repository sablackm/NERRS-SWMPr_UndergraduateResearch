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

excluded <- data.frame(matrix(ncol=1, nrow=300))
count <- 1


gulfNames <- c("apacpwq", "apadbwq", "apaebwq", "apaeswq")
  #c("gndbhwq", "gndbcwq", "gndblwq", "gndpcwq")
 #Timezone shift not required: #c("apacpwq", "apadbwq", "apaebwq", "apaeswq", "apapcwq", "apalmwq")

for(i in 1:length(gulfNames)){

#path <- "C:\\Users\\sabla\\Documents\\Research\\Download5_Current\\GulfofMexico"
path <- "/Users/samuelblackman/Desktop/Research/NERRS/GulfOfMexico"
sitename <- gulfNames[i]
print(sitename)
data_collected <- import_local(path, sitename, trace = FALSE)
bh <- qaqc(data_collected)

bh_test <-bh
#bh_test$datetimestamp <- force_tz(bh_test$datetimestamp, tzone ="EST")

#For Missing Values gnd_bh
#DO
missing_bh_do <- round(sum(is.na(bh_test$do_pct))/nrow(bh_test)*100,2)
missing_bh_do

#Organizing Monthly and Seasonal Sets within the dataframe
site_analyzed<- bh_test[bh_test$datetimestamp>='2007-01-01 00:00' & bh_test$datetimestamp<='2017-12-31 23:45',]
Sitecode <- rep(sitename,nrow(site_analyzed))
site_analyzed <- cbind(site_analyzed, Sitecode)

diel <- format(as.POSIXct(site_analyzed$datetimestamp, format="%Y-%m-%d H:M"), "%Y-%m-%d")
site_analyzed <-cbind(site_analyzed,diel)

monthly<- format(as.POSIXct(site_analyzed$datetimestamp,format="%Y-%m-%d H:M"), "%Y-%m")
site_analyzed<-cbind(site_analyzed,monthly)

site_analyzed <- site_analyzed %>%
  mutate(Month = month(datetimestamp)) %>%
  mutate(Season = ifelse(Month == 1 | Month == 2, paste("Winter", year(datetimestamp), sep=" "),ifelse(Month == 12, paste("Winter", (year(datetimestamp)+1), sep=" "), ifelse(Month == 3 | Month == 4 | Month == 5, paste("Spring", year(datetimestamp), sep=" "),ifelse(Month == 6 | Month == 7 | Month == 8, paste("Summer", year(datetimestamp), sep=" "),ifelse(Month == 9 | Month == 10 | Month == 11, paste("Fall", year(datetimestamp), sep=" "),NA)))))) %>%
  mutate(Year = year(datetimestamp))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

# southANames <- c("sapdcwq", "acefcwq", "acespwq", "gtmpcwq", "niwolwq", "niwtawq", "nocrcwq", "acemcwq", "gtmfmwq", "gtmpiwq", "gtmsswq", "niwcbwq", "niwdcwq", "nocecwq", "sapcawq", "saphdwq", "sapldwq")
# 
# #must do this site individually because Month #5 has a calculation issue (3 data points)
# individual1 <- "noclcwq"
# #Month #6 has calcualtion issue (4 data ponits)
# individual2 <- "noczbwq"
 

# for(i in 1:length(southANames)){
# #path <- "C:\\Users\\sabla\\Documents\\Research\\Download5_Current\\SouthAtlantic"
# path <- "/Users/samuelblackman/Desktop/Research/NERRS/SouthAtlantic"
# sitename <- southANames[i]
# #sitename <- "nocrcwq"
# print(southANames[i])
# data_collected <- import_local(path, sitename, trace = FALSE)
# dc <- qaqc(data_collected)
# 
# 
# site_analyzed<- dc[dc$datetimestamp>='2007-01-01 00:00' & dc$datetimestamp<='2017-12-31 23:45',]
# Sitecode <- rep(sitename,nrow(site_analyzed))
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
# 


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

#Print Total R squared Value
fit <- lm(site_analyzed$do_pct ~  site_analyzed$temp + site_analyzed$depth + site_analyzed$turb + site_analyzed$sal, data=site_analyzed)
summary(fit)$adj.r.squared
vif(fit)

#Adding R squared adjusted
rsquared_M <- 1:nlevels(site_analyzed$monthly)
vifTemp <- 1:nlevels(site_analyzed$monthly)
vifSal <- 1:nlevels(site_analyzed$monthly)
vifTurb <- 1:nlevels(site_analyzed$monthly)
vifDepth <- 1:nlevels(site_analyzed$monthly)

df <- site_analyzed[,c(-4,-9,-10,-11,-12,-14)]


for( i in 1:nlevels(site_analyzed$monthly)){

  temp_month <- df[df$monthly==calculations_monthly$monthly[i],]
  print(i)
  temp_month2 <- na.omit(temp_month)
  
  print(length(temp_month2$do_pct))
  print(length(temp_month2$temp))
  print(length(temp_month2$sal))
  print(length(temp_month2$turb))
  print(length(temp_month2$depth))
  
  if((length(temp_month2$do_pct)>1 && length(unique(temp_month2$do_pct))>1) && (length(temp_month2$temp)>1 && length(unique(temp_month2$temp))>1) && (length(temp_month2$sal)>1 && length(unique(temp_month2$sal))>1) && (length(temp_month2$turb)>1 && length(unique(temp_month2$turb))>1) && (length(temp_month2$depth)>1 && length(unique(temp_month2$depth))>1)){
    fit <- lm(temp_month2$do_pct ~  temp_month2$temp + temp_month2$depth + temp_month2$turb + temp_month2$sal, data=temp_month2)
    rsquared_M[i] <- summary(fit)$adj.r.squared
    vifTemp[i] <-vif(fit)[1]
    vifDepth[i] <-vif(fit)[2]
    vifTurb[i] <-vif(fit)[3]
    vifSal[i] <-vif(fit)[4]
    
  }
  else{
    rsquared_M[i] <- NA
    vifTemp[i] <- NA
    vifDepth[i] <- NA
    vifTurb[i] <- NA
    vifSal[i] <- NA
    
    excluded[count,1] <- (paste(temp_month$Sitecode[1],temp_month$monthly[1]))
    count <- count + 1
  }
}

r2_month <- data.frame(calculations_monthly$monthly, rsquared_M, vifTemp, vifDepth, vifTurb, vifSal)
colnames(r2_month) <- c("monthly","R^2 Adjusted", "VIF Temp", "VIF Depth", "VIF Turb", "VIF Sal")
site_analyzed <- join(site_analyzed,r2_month, type="left")


#Plotting Raster--------------------------------------------------------------------------------------------------

# pDO <-ggplot(site_analyzed, aes(x=Month, y=Year)) +
#   geom_raster(aes(fill = DOpctMissing)) +
#   scale_x_continuous(name="Month", breaks=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
#   scale_y_continuous(name="Year", breaks=c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)) + 
#   scale_fill_distiller(palette = "Reds", direction = 1, limits = c(0,100)) + 
#   ggtitle("DOpct") +
#   theme_classic()
# 
# pTemp <-ggplot(site_analyzed, aes(x=Month, y=Year)) +
#   geom_raster(aes(fill = TempMissing)) +
#   scale_x_continuous(name="Month", breaks=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
#   scale_y_continuous(name="Year", breaks=c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)) + 
#   scale_fill_distiller(palette = "Reds", direction = 1, limits = c(0,100)) + 
#   ggtitle("Temperature") +
#   theme_classic()
# 
# pSal <-ggplot(site_analyzed, aes(x=Month, y=Year)) +
#   geom_raster(aes(fill = SalMissing)) +
#   scale_x_continuous(name="Month", breaks=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
#   scale_y_continuous(name="Year", breaks=c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)) + 
#   scale_fill_distiller(palette = "Reds", direction = 1, limits = c(0,100)) + 
#   ggtitle("Salinity") +
#   theme_classic()
# 
# pTurb <-ggplot(site_analyzed, aes(x=Month, y=Year)) +
#   geom_raster(aes(fill = TurbMissing)) +
#   scale_x_continuous(name="Month", breaks=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
#   scale_y_continuous(name="Year", breaks=c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)) + 
#   scale_fill_distiller(palette = "Reds", direction = 1, limits = c(0,100)) + 
#   ggtitle("Turbidity") +
#   theme_classic()
# 
# pDepth <-ggplot(site_analyzed, aes(x=Month, y=Year)) +
#   geom_raster(aes(fill = DepthMissing)) +
#   scale_x_continuous(name="Month", breaks=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
#   scale_y_continuous(name="Year", breaks=c(2007,2008,2009,2010,2011,2012,2013,2014,2015,2016,2017)) + 
#   scale_fill_distiller(palette = "Reds", direction = 1, limits = c(0,100)) + 
#   ggtitle("Depth") +
#   theme_classic()
# 
# pText <- textGrob(paste("Sitename: ",sitename))
# 
# #setwd("/Users/samuelblackman/Desktop/Research/NERRS/Plots/Missing")
# #jpeg(paste(sitename, "Missing.jpg"))
#   x <- grid.arrange(pText,pDO,pTemp,pSal,pTurb,pDepth, nrow=3, ncol=2)
#   print(x)
# #dev.off()

}


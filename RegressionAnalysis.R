#Multilinear Regression Analysis of GND Sight (No Previous data manipulation besides QAQC)

rm(list = ls())
# remove.packages(c("ggplot2", "plyr","dplyr", "reshape2", "lubridate", "SWMPr", "tidyverse"))
# install.packages('ggplot2', dependencies = TRUE)
# install.packages('plyr', dependencies = TRUE)
# install.packages('dplyr', dependencies = TRUE)
# install.packages('reshape2', dependencies = TRUE)
# install.packages('lubridate', dependencies = TRUE)
# install.packages('SWMPr', dependencies = TRUE)
# install.packages('tidyverse', dependencies = TRUE)

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

#Importing Marshy Marsh Creek Data:

#path <- "C:\\Users\\sabla\\Documents\\Research\\Download5_Current\\GulfofMexico"
path <- "/Users/samuelblackman/Desktop/Research/NERRS/GulfOfMexico"
sitename = 'gndbhwq'
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
  mutate(Season = ifelse(Month == 1 | Month == 2, paste("Winter", year(datetimestamp), sep=" "),ifelse(Month == 12, paste("Winter", (year(datetimestamp)+1), sep=" "), ifelse(Month == 3 | Month == 4 | Month == 5, paste("Spring", year(datetimestamp), sep=" "),ifelse(Month == 6 | Month == 7 | Month == 8, paste("Summer", year(datetimestamp), sep=" "),ifelse(Month == 9 | Month == 10 | Month == 11, paste("Fall", year(datetimestamp), sep=" "),NA))))))

#-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------


#path <- "C:\\Users\\sabla\\Documents\\Research\\Download5_Current\\SouthAtlantic"
path <- "/Users/samuelblackman/Desktop/Research/NERRS/GulfOfMexico"
sitename = 'apaebwq'
data_collected <- import_local(path, sitename, trace = FALSE)
dc <- qaqc(data_collected)


site_analyzed<- dc[dc$datetimestamp>='2007-01-01 00:00' & dc$datetimestamp<='2017-12-31 23:45',]
Sitecode <- rep('sapdcwq',nrow(site_analyzed))
site_analyzed <- cbind(site_analyzed, Sitecode)

diel <- format(as.POSIXct(site_analyzed$datetimestamp, format="%Y-%m-%d H:M"), "%Y-%m-%d")
site_analyzed <-cbind(site_analyzed,diel)

monthly<- format(as.POSIXct(site_analyzed$datetimestamp,format="%Y-%m-%d H:M"), "%Y-%m")
site_analyzed<-cbind(site_analyzed,monthly)

site_analyzed <- site_analyzed %>% 
  mutate(Month = month(datetimestamp)) %>% 
  mutate(Season = ifelse(Month == 1 | Month == 2, paste("Winter", year(datetimestamp), sep=" "),ifelse(Month == 12, paste("Winter", (year(datetimestamp)+1), sep=" "), ifelse(Month == 3 | Month == 4 | Month == 5, paste("Spring", year(datetimestamp), sep=" "),ifelse(Month == 6 | Month == 7 | Month == 8, paste("Summer", year(datetimestamp), sep=" "),ifelse(Month == 9 | Month == 10 | Month == 11, paste("Fall", year(datetimestamp), sep=" "),NA))))))



#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------







getSeason <- "Fall"
#getMonth <- "2007-01"


for(i in 1:12){
  #Choosing Month/Season
  #calculcating Missing Values Percentages
  timeseries <- site_analyzed[site_analyzed$datetimestamp>='2009-01-01 00:00' & site_analyzed$datetimestamp<='2009-12-31 23:45',]
  timeseries <- subset(timeseries, Month==i)
  #timeseries <- subset(timeseries, Season==paste(getSeason, i))
  missing_for_year_DO <- round(sum(is.na(timeseries$do_pct))/nrow(timeseries)*100,2)
  missing_for_year_DO
  missing_for_year_turb <- round(sum(is.na(timeseries$turb))/nrow(timeseries)*100,2)
  missing_for_year_turb
  missing_for_year_sal <- round(sum(is.na(timeseries$sal))/nrow(timeseries)*100,2)
  missing_for_year_sal
  missing_for_year_temp <- round(sum(is.na(timeseries$temp))/nrow(timeseries)*100,2)
  missing_for_year_temp
  missing_for_year_depth <- round(sum(is.na(timeseries$depth))/nrow(timeseries)*100,2)
  missing_for_year_depth
  
  
  
  
  #setwd("C:\\Users\\sabla\\Documents\\Research\\Plots\\Regression\\Summaries")
  setwd("/Users/samuelblackman/Desktop/Research/NERRS/Plots/Summaries")
  
  if(!is.na(missing_for_year_DO)){
    #Fitting the Data
    fit <- lm(timeseries$do_pct ~  timeseries$temp + timeseries$depth + timeseries$turb + timeseries$sal, data=timeseries)
    
    sink(paste(i,paste(i, "2009 Radj.txt")))
    #sink('Summer Radj_2012.txt')
    print(summary(fit))
    print(vif(fit))
    print(paste("Missing Data DO: ",missing_for_year_DO))
    print(paste("Missing Data turb: ",missing_for_year_turb))
    print(paste("Missing Data sal: ",missing_for_year_sal))
    print(paste("Missing Data temp: ",missing_for_year_temp))
    print(paste("Missing Data depth: ",missing_for_year_depth))
    sink()
    
    
    #Timeseries
    #2017
     # yr <- site_analyzed[site_analyzed$datetimestamp>='2007-01-01 00:00' & site_analyzed$datetimestamp<='2017-12-31 23:45',]
     # #timeseries_OG <- subset(yr, Season=='Summer 2017') #yr
     # p1 <- ggplot(yr, aes(x=datetimestamp, y=do_pct)) +
     #   geom_line() +
     #   xlab("2017") +
     #   ylab("DO %") +
     #   ylim(c(0,200)) +
     #   ggtitle("wkbmrwq OG MC DO over 2017") +
     #   theme_bw()
     # p1
    
    #setwd("C:\\Users\\sabla\\Documents\\Research\\Plots\\Regression")
    setwd("/Users/samuelblackman/Desktop/Research/NERRS/Plots/GND_BC")
    predicted_df <- data.frame(datetimestamp=timeseries$datetimestamp, do_pred = predict(fit, timeseries))
    
    jpeg(paste(i,paste(getSeason, ".jpeg")))
    #jpeg('Summer 2012.jpeg')
    p2 <- ggplot(timeseries, aes(x=datetimestamp, y=do_pct)) +
      geom_line() + geom_line(color='red',data=predicted_df,aes(datetimestamp,do_pred)) +
      xlab(i) +
      ylab("DO %") +
      #ylim(c(0,100)) +
      ggtitle(paste(paste(sitename," MC DO over "),i)) +
      theme_bw()
    print(p2)
    dev.off()
    #grid.arrange(p1,p2,nrow=2, ncol=1)
  }
}

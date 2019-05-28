#Loads data from all sites under review from the South Atlantic and Gulf of Mexico and saves into 2 csv files that can be loaded into other scripts
#YOU WILL NEED TO CHANGE YOUR DIRECTORIES FOR THIS TO WORK.
#THIS SHOULD BE THE FIRST FILE YOU RUN BEFORE ANY OTHER SCRIPTS

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

#Gulf of Mexico, sites done separately because of timezone shift
gndNames <- c("gndbhwq", "gndbcwq", "gndblwq", "gndpcwq")

#GND Site
for(i in 1:length(gndNames)){
  
  path <- "C:\\Users\\sabla\\Documents\\Research\\Download5_Current\\GulfofMexico"
  #path <- "/Users/samuelblackman/Desktop/Research/NERRS/GulfOfMexico"
  sitename <- gndNames[i]
  print(sitename)
  data_collected <- import_local(path, sitename, trace = FALSE)
  bh <- qaqc(data_collected)
  
  bh_test <-bh
  bh_test$datetimestamp <- force_tz(bh_test$datetimestamp, tzone ="EST")
  
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
  
  site_analyzed <- site_analyzed[,c(-3,-8,-9,-10,-13)]
  
  #setwd("/Users/samuelblackman/Desktop/Research/NERRS/CurrentData")
  setwd("C:\\Users\\sabla\\Documents\\Research\\CurrentData")
  write.csv(site_analyzed, file = paste(sitename,"data.csv")) 
}
  

apaNames <- c("apacpwq", "apadbwq", "apaebwq", "apaeswq")

#APA Site
for(i in 1:length(apaNames)){
  
  path <- "C:\\Users\\sabla\\Documents\\Research\\Download5_Current\\GulfofMexico"
  #path <- "/Users/samuelblackman/Desktop/Research/NERRS/GulfOfMexico"
  sitename <- apaNames[i]
  print(sitename)
  data_collected <- import_local(path, sitename, trace = FALSE)
  bh <- qaqc(data_collected)
  
  #Organizing Monthly and Seasonal Sets within the dataframe
  site_analyzed<- bh[bh$datetimestamp>='2007-01-01 00:00' & bh$datetimestamp<='2017-12-31 23:45',]
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
 
  site_analyzed <- site_analyzed[,c(-3,-8,-9,-10,-13)]
  
  #setwd("/Users/samuelblackman/Desktop/Research/NERRS/CurrentData")
  setwd("C:\\Users\\sabla\\Documents\\Research\\CurrentData")
  write.csv(site_analyzed, file = paste(sitename,"data.csv"))  
}

  #-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------

  southANames <- c("sapdcwq", "acefcwq", "acespwq", "gtmpcwq", "niwolwq", "niwtawq", "nocrcwq", "acemcwq", "gtmfmwq", "gtmpiwq", "gtmsswq", "niwcbwq", "niwdcwq", "nocecwq", "sapcawq", "saphdwq", "sapldwq")

for(i in 1:length(southANames)){
  
  path <- "C:\\Users\\sabla\\Documents\\Research\\Download5_Current\\SouthAtlantic"
  #path <- "/Users/samuelblackman/Desktop/Research/NERRS/SouthAtlantic"
  sitename <- southANames[i]
  print(southANames[i])
  data_collected <- import_local(path, sitename, trace = FALSE)
  dc <- qaqc(data_collected)
  
  site_analyzed<- dc[dc$datetimestamp>='2007-01-01 00:00' & dc$datetimestamp<='2017-12-31 23:45',]
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
  
  site_analyzed <- site_analyzed[,c(-3,-8,-9,-10,-13)]
  
  #setwd("/Users/samuelblackman/Desktop/Research/NERRS/CurrentData")
  setwd("C:\\Users\\sabla\\Documents\\Research\\CurrentData")
  write.csv(site_analyzed, file = paste(sitename,"data.csv")) 
  
}

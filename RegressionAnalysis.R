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

install.packages("car")
library(car)

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

#path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\GulfofMexico"
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

#------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------






#Choosing Month/Season
#calculcating Missing Values Percentages
timeseries <- site_analyzed[site_analyzed$datetimestamp>='2009-01-01 00:00' & site_analyzed$datetimestamp<='2009-12-31 23:45',]
timeseries <- subset(timeseries, Season=='Summer 2009')
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



#2017 turb, sal, temp 
#2017 temp, depth, ph, turb
#2017 temp, ph
#2010 sal, temp, depth, ph
#2013 sal, temp, depth, ph (could remove ph or depth)


#Fitting the Data
fit <- lm(timeseries$do_pct ~  timeseries$temp + timeseries$depth + timeseries$turb + timeseries$sal, data=timeseries)
summary(fit)
vif(fit)


#Timeseries
#2017
# yr <- site_analyzed[site_analyzed$datetimestamp>='2017-01-01 00:00' & site_analyzed$datetimestamp<='2017-12-31 23:45',]
# timeseries_OG <- subset(yr, Season=='Summer 2017') #yr
# p1 <- ggplot(timeseries_OG, aes(x=datetimestamp, y=do_pct)) +
#   geom_line() +
#   xlab("2017") +
#   ylab("DO %")+
#   ylim(c(0,100)) +
#   ggtitle("gndbhwq OG MC DO over 2017") +
#   theme_bw()

predicted_df <- data.frame(datetimestamp=timeseries$datetimestamp, do_pred = predict(fit, timeseries))
p2 <- ggplot(timeseries, aes(x=datetimestamp, y=do_pct)) +
  geom_line() + geom_line(color='red',data=predicted_df,aes(datetimestamp,do_pred)) +
  xlab("2009") +
  ylab("DO %")+
  ylim(c(0,100)) +
  ggtitle("gndbhwq MC DO over 2009") +
  theme_bw()
p2
#grid.arrange(p1,p2,nrow=2, ncol=1)


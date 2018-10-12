#--------------------------------------
#Sam Blackman 4/17/18
#This script is used to calculate Cumulative Distribution Function (CDF) for
#The defined Marshy Marsh Creek Habitiat types (currently only seen in the
#South Atlantic and Gulf of Mexico).
#Also seen in this script is the MIssing Value % seen in the data sets examined.
#--------------------------------------

rm(list = ls())
# remove.packages(c("ggplot2", "plyr","dplyr", "reshape2", "lubridate", "SWMPr", "tidyverse"))
 # install.packages('ggplot2', dependencies = TRUE)
 # install.packages('plyr', dependencies = TRUE)
 # install.packages('dplyr', dependencies = TRUE)
 # install.packages('reshape2', dependencies = TRUE)
 # install.packages('lubridate', dependencies = TRUE)
 # install.packages('SWMPr', dependencies = TRUE)
 # install.packages('tidyverse', dependencies = TRUE)

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

#path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\SouthAtlantic"
path <- "/Users/samuelblackman/Desktop/Research/NERRS/Southeast"
sitename = 'sapdcwq'
data_collected <- import_local(path, sitename, trace = FALSE)
dc <- qaqc(data_collected)

#path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\SouthAtlantic"
path <- "/Users/samuelblackman/Desktop/Research/NERRS/Southeast"
sitename = 'sapldwq'
data_collected <- import_local(path, sitename, trace = FALSE)
ld <- qaqc(data_collected)

#path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\SouthAtlantic"
path <- "/Users/samuelblackman/Desktop/Research/NERRS/Southeast"
sitename = 'gtmpcwq'
data_collected <- import_local(path, sitename, trace = FALSE)
pc <- qaqc(data_collected)

#path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\GulfofMexico"
path <- "/Users/samuelblackman/Desktop/Research/NERRS/GulfOfMexico"
sitename = 'gndbcwq'
data_collected <- import_local(path, sitename, trace = FALSE)
bc <- qaqc(data_collected)

#path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\GulfofMexico"
path <- "/Users/samuelblackman/Desktop/Research/NERRS/GulfOfMexico"
sitename = 'gndblwq'
data_collected <- import_local(path, sitename, trace = FALSE)
bl <- qaqc(data_collected)


bh_test <-bh
bc_test <-bc
bl_test <-bl
#Change Grand Bay into EST
bh_test$datetimestamp <- force_tz(bh_test$datetimestamp, tzone ="EST")
bc_test$datetimestamp <- force_tz(bc_test$datetimestamp, tzone ="EST")
bl_test$datetimestamp <- force_tz(bl_test$datetimestamp, tzone ="EST")
#sub_dat_test<- bh_test[bh_test$datetimestamp>='2007-01-01 00:00' & bh_test$datetimestamp<='2016-12-31 23:45',]
#sub_dat_test2<- bh[bh$datetimestamp>='2007-01-01 00:00' & bh$datetimestamp<='2016-12-31 23:45',]

#CDF Plots
sub_dat1<- bh_test[bh_test$datetimestamp>='2007-01-01 00:00' & bh_test$datetimestamp<='2017-12-31 23:45',]
g_gndbh <- ggplot(sub_dat1, aes(x=sub_dat1$do_mgl)) + 
  stat_ecdf() +
  xlab("DO (mg/L)") +
  ylab("%")+
  ggtitle("'Marshy' gndBH CDF 2007-2017") +
  theme_bw()

sub_dat6<- bc_test[bc_test$datetimestamp>='2007-01-01 00:00' & bc_test$datetimestamp<='2017-12-31 23:45',]
g_gndbc <- ggplot(sub_dat6, aes(x=sub_dat6$do_mgl)) + 
  stat_ecdf() +
  xlab("DO (mg/L)") +
  ylab("%")+
  ggtitle("Marsh Creek gndBC CDF 2007-2017") +
  theme_bw()

sub_dat5<- bl_test[bl_test$datetimestamp>='2007-01-01 00:00' & bl_test$datetimestamp<='2017-12-31 23:45',]
g_gndbl <- ggplot(sub_dat5, aes(x=sub_dat5$do_mgl)) + 
  stat_ecdf() +
  xlab("DO (mg/L)") +
  ylab("%")+
  ggtitle("Open Water gndBL CDF 2007-2017") +
  theme_bw()

grid.arrange(g_gndbh,g_gndbc,g_gndbl, nrow=2, ncol=2)





sub_dat2<- dc[dc$datetimestamp>='2007-01-01 00:00' & dc$datetimestamp<='2017-12-31 23:45',]
g_sap <- ggplot(sub_dat2, aes(x=sub_dat2$do_mgl)) + 
  stat_ecdf()+
  xlab("DO (mg/L)") +
  ylab("%")+
  ggtitle("'Marshy' sapDC CDF 2007-2017") +
  theme_bw()

sub_dat3<- ld[ld$datetimestamp>='2007-01-01 00:00' & ld$datetimestamp<='2017-12-31 23:45',]
g_sap_control <-ggplot(sub_dat3, aes(x=sub_dat3$do_mgl)) + 
  stat_ecdf() +
  xlab("DO (mg/L)") +
  ylab("%")+
  ggtitle("Openwater CDF 2007-2017") +
  theme_bw()

sub_dat4<- pc[pc$datetimestamp>='2007-01-01 00:00' & pc$datetimestamp<='2017-12-31 23:45',]
g_pc_control <-ggplot(sub_dat4, aes(x=sub_dat4$do_mgl)) + 
  stat_ecdf() +
  xlab("DO (mg/L)") +
  ylab("%")+
  ggtitle("Marsh Creek CDF 2007-2017") +
  theme_bw()

grid.arrange(g_sap,g_sap_control,g_pc_control, nrow=2, ncol=2)
#-------------------------------------------------------------------------------------------------
# Percentage of Missing Values:
#-------------------------------------------------------------------------------------------------

#For gnd_bh
#DO
missing_bh_do <- round(sum(is.na(sub_dat1$do_mgl))/nrow(sub_dat1)*100,2)
missing_bh_do
#pH
missing_bh_ph <- round(sum(is.na(sub_dat1$ph))/nrow(sub_dat1)*100,2)
missing_bh_ph

#For sap_dc
#DO
missing_dc_do <- round(sum(is.na(sub_dat2$do_mgl))/nrow(sub_dat2)*100,2)
missing_dc_do
#pH
missing_dc_ph <- round(sum(is.na(sub_dat2$ph))/nrow(sub_dat2)*100,2)
missing_dc_ph

#For Openwater Comparison
#DO
missing_ld_do <- round(sum(is.na(sub_dat3$do_mgl))/nrow(sub_dat3)*100,2)
missing_ld_do
#pH
missing_ld_ph <- round(sum(is.na(sub_dat3$ph))/nrow(sub_dat3)*100,2)
missing_ld_ph

#For Marsh Creek Comparison
#DO
missing_pc_do <- round(sum(is.na(sub_dat4$do_mgl))/nrow(sub_dat4)*100,2)
missing_pc_do
#pH
missing_pc_ph <- round(sum(is.na(sub_dat4$ph))/nrow(sub_dat4)*100,2)

# site<-c("gnd_bh", "sap_dc", "Openwater", "Marsh Creek")
# missing_DO<-c(missing_bh_do,missing_dc_do,missing_ld_do,missing_ta_do)
# missing_ph<-c(missing_bh_ph,missing_dc_ph,missing_ld_ph,missing_ta_ph)
# 
# df_missing <-data.frame(Sites=site, do_miss=missing_DO, ph_miss=missing_ph)
# colnames(df_missing) <- c("Site","|% DO Values Missing","|% pH Values Missing")
# df_missing




#-----------------------------------------------------------------------------------------------------------------------
#Testing Timescale Additions to the Dataset
#-----------------------------------------------------------------------------------------------------------------------


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

#Testing my calculations for CV = STD/mean
bh_calculations_diel <- site_analyzed %>% group_by(diel) %>% summarise(diel_STD = sd(do_mgl, na.rm=TRUE), diel_AVG = mean(do_mgl, na.rm=TRUE)) %>% mutate(diel_CV=(diel_STD/diel_AVG))
site_analyzed <- merge(site_analyzed, bh_calculations_diel, by="diel")


bh_calculations_monthly <- site_analyzed %>% group_by(monthly) %>% summarise(monthly_STD = sd(do_mgl, na.rm=TRUE), monthly_AVG = mean(do_mgl,na.rm=TRUE)) %>% mutate(monthly_CV=(monthly_STD/monthly_AVG))
site_analyzed <- merge(site_analyzed, bh_calculations_monthly, by="monthly")


bh_calculations_seasonal <- site_analyzed %>% group_by(Season) %>% summarise(seasonal_STD = sd(do_mgl, na.rm=TRUE), seasonal_AVG = mean(do_mgl, na.rm = TRUE)) %>% mutate(seasonal_CV=(seasonal_STD/seasonal_AVG))
site_analyzed <- merge(site_analyzed, bh_calculations_seasonal, by="Season")


#gndbh missing values:
#Monthly:
error_percent_M <- 1:nlevels(site_analyzed$monthly)

for(i in 1:nlevels(site_analyzed$monthly)){
  
  temp_month <-bh_calculations_monthly$monthly[i]
  temp <- site_analyzed[site_analyzed$monthly==temp_month,]
  
  error_percent_M[i] <- round((1-(sum(is.na(temp$do_mgl)))/nrow(temp))*100,2)
  #print(paste(temp_month, round((1-(sum(is.na(temp$do_mgl)))/nrow(temp))*100,2), sep=': '))
}

#Error Table: Monthly
error_month <- data.frame(bh_calculations_monthly$monthly, error_percent_M)

#Daily
error_percent_D <- 1:nlevels(site_analyzed$diel)

for(i in 1:nlevels(site_analyzed$diel)){
  
  temp_diel <-bh_calculations_diel$diel[i]
  temp <- site_analyzed[site_analyzed$diel==temp_diel,]
  
  error_percent_D[i] <- round((1-(sum(is.na(temp$do_mgl)))/nrow(temp))*100,2)
  #print(paste(temp_diel, round((1-(sum(is.na(temp$do_mgl)))/nrow(temp))*100,2), sep=': '))
}

#Error Table: Daily
error_diel <- data.frame(bh_calculations_diel$diel, error_percent_D)

#Seasonal
error_percent_S <- 1:45

for(i in 1:45){
  
  temp_season <-bh_calculations_seasonal$Season[i]
  temp <- site_analyzed[site_analyzed$Season==temp_season,]
  
  error_percent_S[i] <- round((1-(sum(is.na(temp$do_mgl)))/nrow(temp))*100,2)
  #print(paste(temp_season, round((1-(sum(is.na(temp$do_mgl)))/nrow(temp))*100,2), sep=': '))
}

#Error Table: Seasonally
error_season <- data.frame(bh_calculations_seasonal$Season, error_percent_S)



#Removing any Values with error below 90%


#Start with Daily:
error_diel <- error_diel %>% mutate(error_cleaned = ifelse(error_diel$error_percent_D<90, NA, error_diel$error_percent_D))
colnames(error_diel) <- c("diel","error_percent_D","error_cleaned_D")

site_analyzed_clean<- bh_test[bh_test$datetimestamp>='2007-01-01 00:00' & bh_test$datetimestamp<='2017-12-31 23:45',]


Sitecode <- rep('gndbhwq',nrow(site_analyzed_clean))
site_analyzed_clean <- cbind(site_analyzed_clean, Sitecode)

diel <- format(as.POSIXct(site_analyzed_clean$datetimestamp, format="%Y-%m-%d H:M"), "%Y-%m-%d")
site_analyzed_clean <-cbind(site_analyzed_clean,diel)

monthly<- format(as.POSIXct(site_analyzed_clean$datetimestamp,format="%Y-%m-%d H:M"), "%Y-%m")
site_analyzed_clean<-cbind(site_analyzed_clean,monthly)

site_analyzed <- arrange(site_analyzed, site_analyzed$datetimestamp)

site_analyzed_clean <- site_analyzed_clean %>% 
  mutate(Month = month(datetimestamp)) %>% 
  mutate(Season = ifelse(Month == 1 | Month == 2, paste("Winter", year(datetimestamp), sep=" "),ifelse(Month == 12, paste("Winter", (year(datetimestamp)+1), sep=" "), ifelse(Month == 3 | Month == 4 | Month == 5, paste("Spring", year(datetimestamp), sep=" "),ifelse(Month == 6 | Month == 7 | Month == 8, paste("Summer", year(datetimestamp), sep=" "),ifelse(Month == 9 | Month == 10 | Month == 11, paste("Fall", year(datetimestamp), sep=" "),NA))))))

site_analyzed_clean <-  cbind(site_analyzed_clean, do_mgl_D=rep(site_analyzed$do_mgl))

site_analyzed_clean <- join(site_analyzed_clean,error_diel, type="left")
for(i in 1:length(site_analyzed_clean$do_mgl)){
  ifelse(is.na(site_analyzed_clean$error_cleaned_D[i]), site_analyzed_clean$do_mgl_D[i]<-NA, site_analyzed_clean$do_mgl[i])
}

bh_calculations_diel_2 <- site_analyzed_clean %>% group_by(diel) %>% summarise(diel_STD = sd(do_mgl_D, na.rm=TRUE), diel_AVG = mean(do_mgl_D, na.rm=TRUE)) %>% mutate(diel_CV=(diel_STD/diel_AVG))
site_analyzed_clean <- merge(site_analyzed_clean, bh_calculations_diel_2, by="diel")


#Monthly:
error_percent_M <- 1:nlevels(site_analyzed_clean$monthly)

for(i in 1:nlevels(site_analyzed_clean$monthly)){
  
  temp_month <- bh_calculations_monthly$monthly[i]
  temp <- site_analyzed_clean[site_analyzed_clean$monthly==temp_month,]
  
  error_percent_M[i] <- round((1-(sum(is.na(temp$do_mgl_D)))/nrow(temp))*100,2)
  #print(paste(temp_month, round((1-(sum(is.na(temp$do_mgl)))/nrow(temp))*100,2), sep=': '))
}

#Error Table: Monthly
error_month_2 <- data.frame(bh_calculations_monthly$monthly, error_percent_M)

error_month_2 <- error_month_2 %>% mutate(error_cleaned = ifelse(error_month_2$error_percent_M<90, NA, error_month_2$error_percent_M))
colnames(error_month_2) <- c("monthly","error_percent_M","error_cleaned_M")

site_analyzed_clean <-  cbind(site_analyzed_clean, do_mgl_M=rep(site_analyzed_clean$do_mgl_D))

site_analyzed_clean <- join(site_analyzed_clean,error_month_2, type="left")
for(i in 1:length(site_analyzed_clean$do_mgl)){
  ifelse(is.na(site_analyzed_clean$error_cleaned_M[i]), site_analyzed_clean$do_mgl_M[i]<-NA, site_analyzed_clean$do_mgl_D[i])
}

bh_calculations_monthly_2 <- site_analyzed_clean %>% group_by(monthly) %>% summarise(monthly_STD = sd(do_mgl_M, na.rm=TRUE), monthly_AVG = mean(do_mgl_M,na.rm=TRUE)) %>% mutate(monthly_CV=(monthly_STD/monthly_AVG))
site_analyzed_clean <- merge(site_analyzed_clean, bh_calculations_monthly_2, by="monthly")


#Seasonal:
error_percent_S <- 1:45

for(i in 1:45){
  
  temp_season <-bh_calculations_seasonal$Season[i]
  temp <- site_analyzed_clean[site_analyzed_clean$Season==temp_season,]
  
  error_percent_S[i] <- round((1-(sum(is.na(temp$do_mgl_M)))/nrow(temp))*100,2)
  #print(paste(temp_season, round((1-(sum(is.na(temp$do_mgl)))/nrow(temp))*100,2), sep=': '))
}

#Error Table: Seasonally
error_season_2 <- data.frame(bh_calculations_seasonal$Season, error_percent_S)

error_season_2 <- error_season_2 %>% mutate(error_cleaned = ifelse(error_season_2$error_percent_S<90, NA, error_season_2$error_percent_S))
colnames(error_season_2) <- c("Season","error_percent_S","error_cleaned_S")

site_analyzed_clean <-  cbind(site_analyzed_clean, do_mgl_S=rep(site_analyzed_clean$do_mgl_M))

site_analyzed_clean <- join(site_analyzed_clean,error_season_2, type="left")
for(i in 1:length(site_analyzed_clean$do_mgl)){
  ifelse(is.na(site_analyzed_clean$error_cleaned_S[i]), site_analyzed_clean$do_mgl_S[i]<-NA, site_analyzed_clean$do_mgl_M[i])
}

bh_calculations_seasonal_2 <- site_analyzed_clean %>% group_by(Season) %>% summarise(seasonal_STD = sd(do_mgl_S, na.rm=TRUE), seasonal_AVG = mean(do_mgl_S,na.rm=TRUE)) %>% mutate(seasonal_CV=(seasonal_STD/seasonal_AVG))
site_analyzed_clean <- merge(site_analyzed_clean, bh_calculations_seasonal_2, by="Season")







timeseries <- site_analyzed_clean[site_analyzed_clean$datetimestamp>='2016-08-01 00:00' & site_analyzed_clean$datetimestamp<='2016-08-31 23:45',]
fit <- lm(timeseries$do_mgl_D ~ timeseries$turb + timeseries$ph +timeseries$depth, data=timeseries)
summary(fit)

fit <- lm(site_analyzed_clean$do_mgl_D ~ site_analyzed_clean$turb + site_analyzed_clean$ph, data=site_analyzed_clean)
summary(fit)














#Timeseries
#2008
timeseries_OG <- bh_test[bh_test$datetimestamp>='2016-01-01 00:00' & bh_test$datetimestamp<='2016-01-31 23:45',]
p1 <- ggplot(timeseries_OG, aes(x=datetimestamp, y=do_mgl)) +
  geom_line() +
  xlab("2016") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("gndbhwq OG MC DO over 2016") +
  theme_bw()

predicted_df <- data.frame(datetimestamp=timeseries$datetimestamp, do_pred = predict(fit, timeseries))
p2 <- ggplot(timeseries, aes(x=datetimestamp, y=do_mgl)) +
  geom_line() + geom_line(color='red',data=predicted_df,aes(datetimestamp,do_pred)) +
  xlab("2016") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("gndbhwq MC DO over 2016") +
  theme_bw()

grid.arrange(p1,p2,nrow=2, ncol=1)


#Seasonal Plots (2007-2017 gndbh)

#Spring, Summer, Fall
setwd("C:/Users/sabla/Documents/Research/Plots")
for(i in 1:45){
  
  temp_season <- bh_calculations_seasonal$Season[i]
  temp <- site_analyzed[site_analyzed$Season==temp_season,]
  
  jpeg(paste(temp_season,'.jpg'))
  
  seasonPlot <- ggplot(temp, aes(x=datetimestamp, y=do_mgl)) +
    geom_line() +
    xlab("Year") +
    ylab("DO (mg/L)")+
    ylim(c(0,15)) +
    ggtitle("") +
    theme_bw()
  
  print(seasonPlot)
  dev.off()

}

#Winter
temp <- site_analyzed[site_analyzed$datetimestamp>='2007-01-01 00:00' & site_analyzed$datetimestamp<='2007-02-28 23:45',]

jpeg(paste('Winter 2007','.jpg'))

seasonPlot <- ggplot(temp, aes(x=datetimestamp, y=do_mgl)) +
  geom_line() +
  xlab("Year") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("") +
  theme_bw()

print(seasonPlot)
dev.off()


#Coefficient of Variation Plots: Diel

subdat1<- site_analyzed_clean[site_analyzed_clean$datetimestamp>='2007-01-01 00:00' & site_analyzed_clean$datetimestamp<='2017-12-31 23:45',]
g_gnd_cv_D <- ggplot(subdat1, aes(x=subdat1$datetimestamp, y=subdat1$diel_CV)) + 
  geom_point() +
  xlab("Time") +
  ylab("CV")+
  ggtitle("'Marshy' gndBH CV Diel 2007-2017") +
  theme_bw()
plot(g_gnd_cv_D)


subdat1<- site_analyzed_clean[site_analyzed_clean$datetimestamp>='2007-01-01 00:00' & site_analyzed_clean$datetimestamp<='2017-12-31 23:45',]
g_gnd_cv_M <- ggplot(subdat1, aes(x=monthly, y=monthly_CV)) + 
  geom_point() +
  xlab("Time") +
  ylab("CV")+
  ggtitle("'Marshy' gndBH CV Monthly 2007-2017") +
  theme_bw()
plot(g_gnd_cv_M)

subdat1<- site_analyzed_clean[site_analyzed_clean$datetimestamp>='2007-01-01 00:00' & site_analyzed_clean$datetimestamp<='2017-12-31 23:45',]
g_gnd_cv_M <- ggplot(subdat1, aes(x=Season, y=seasonal_CV)) + 
  geom_point() +
  xlab("Time") +
  ylab("CV")+
  ggtitle("'Marshy' gndBH CV Seasonal 2007-2017") +
  theme_bw()
plot(g_gnd_cv_M)

subdat1<- site_analyzed_clean[site_analyzed_clean$datetimestamp>='2007-01-01 00:00' & site_analyzed_clean$datetimestamp<='2017-12-31 23:45',]
g_gnd_cv_monthly <- ggplot(subdat1, aes(x=Month, y=monthly_CV, group=Month)) +
  geom_boxplot() +
  geom_jitter(position = position_jitter(w=.001,h=.001)) +
  #xlab("Month") +
  ylab("CV for each Month")+
  scale_x_discrete(name="Month", limits=c(1,2,3,4,5,6,7,8,9,10,11,12)) +
  ggtitle("gndbhwq Monthly DO CV", subtitle = "2007-2017") +
  theme_bw()
plot(g_gnd_cv_monthly)
















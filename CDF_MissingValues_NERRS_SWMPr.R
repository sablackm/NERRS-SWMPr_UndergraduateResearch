#--------------------------------------
#Sam Blackman 4/17/18
#This script is used to calculate Cumulative Distribution Function (CDF) for
#The defined Marshy Marsh Creek Habitiat types (currently only seen in the
#South Atlantic and Gulf of Mexico).
#Also seen in this script is the MIssing Value % seen in the data sets examined.
#--------------------------------------

rm(list = ls())

library("SWMPr")
library("tidyverse")
library("dplyr")
library("ggplot2")
library("reshape2")
library("lubridate")
library("gridExtra")
library("data.table")

#Importing Marshy Marsh Creek Data:

path <- "/Users/samuelblackman/Desktop/Research/NERRS/GulfofMexico"
sitename = 'gndbhwq'
data_collected <- import_local(path, sitename, trace = FALSE)
bh <- qaqc(data_collected)

path <- "/Users/samuelblackman/Desktop/Research/NERRS/SouthEast"
sitename = 'sapdcwq'
data_collected <- import_local(path, sitename, trace = FALSE)
dc <- qaqc(data_collected)

path <- "/Users/samuelblackman/Desktop/Research/NERRS/SouthEast"
sitename = 'sapldwq'
data_collected <- import_local(path, sitename, trace = FALSE)
ld <- qaqc(data_collected)

path <- "/Users/samuelblackman/Desktop/Research/NERRS/SouthEast"
sitename = 'niwtawq'
data_collected <- import_local(path, sitename, trace = FALSE)
ta <- qaqc(data_collected)

bh_test <-bh
#Change Grand Bay into EST
bh_test$datetimestamp <- force_tz(bh_test$datetimestamp, tzone ="EST")
#sub_dat_test<- bh_test[bh_test$datetimestamp>='2007-01-01 00:00' & bh_test$datetimestamp<='2016-12-31 23:45',]
#sub_dat_test2<- bh[bh$datetimestamp>='2007-01-01 00:00' & bh$datetimestamp<='2016-12-31 23:45',]

#CDF Plots
sub_dat1<- bh_test[bh_test$datetimestamp>='2007-01-01 00:00' & bh_test$datetimestamp<='2016-12-31 23:45',]
g_gnd <- ggplot(sub_dat1, aes(x=sub_dat1$do_mgl)) + 
  stat_ecdf() +
  xlab("DO (mg/L)") +
  ylab("%")+
  ggtitle("'Marshy' gndBH CDF 2007-2016") +
  theme_bw()
head(sub_dat1)
sub_dat2<- dc[dc$datetimestamp>='2007-01-01 00:00' & dc$datetimestamp<='2016-12-31 23:45',]
g_sap <- ggplot(sub_dat2, aes(x=sub_dat2$do_mgl)) + 
  stat_ecdf()+
  xlab("DO (mg/L)") +
  ylab("%")+
  ggtitle("'Marshy' sapDC CDF 2007-2016") +
  theme_bw()

sub_dat3<- ld[ld$datetimestamp>='2007-01-01 00:00' & ld$datetimestamp<='2016-12-31 23:45',]
g_sap_control <-ggplot(sub_dat3, aes(x=sub_dat3$do_mgl)) + 
  stat_ecdf() +
  xlab("DO (mg/L)") +
  ylab("%")+
  ggtitle("Openwater CDF 2007-2016") +
  theme_bw()

sub_dat4<- ta[ta$datetimestamp>='2007-01-01 00:00' & ta$datetimestamp<='2016-12-31 23:45',]
g_niw_control <-ggplot(sub_dat4, aes(x=sub_dat4$do_mgl)) + 
  stat_ecdf() +
  xlab("DO (mg/L)") +
  ylab("%")+
  ggtitle("Marsh Creek CDF 2007-2016") +
  theme_bw()

grid.arrange(g_gnd,g_sap,g_sap_control,g_niw_control, nrow=2, ncol=2)
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
missing_ta_do <- round(sum(is.na(sub_dat4$do_mgl))/nrow(sub_dat4)*100,2)
missing_ta_do
#pH
missing_ta_ph <- round(sum(is.na(sub_dat4$ph))/nrow(sub_dat4)*100,2)

site<-c("gnd_bh", "sap_dc", "Openwater", "Marsh Creek")
missing_DO<-c(missing_bh_do,missing_dc_do,missing_ld_do,missing_ta_do)
missing_ph<-c(missing_bh_ph,missing_dc_ph,missing_ld_ph,missing_ta_ph)

df_missing <-data.frame(Sites=site, do_miss=missing_DO, ph_miss=missing_ph)
colnames(df_missing) <- c("Site","|% DO Values Missing","|% pH Values Missing")
df_missing


#-----------------------------------------------------------------------------------------------------------------------
#Testing Timescale Additions to the Dataset
#-----------------------------------------------------------------------------------------------------------------------


bh_test_2<- bh_test[bh_test$datetimestamp>='2007-01-01 00:00' & bh_test$datetimestamp<='2017-12-31 23:45',]


Sitecode <- rep('gndbhwq',nrow(bh_test_2))
bh_test_2 <- cbind(bh_test_2, Sitecode)

diel <- format(as.POSIXct(bh_test_2$datetimestamp, format="%Y-%m-%d H:M"), "%Y-%m-%d")
bh_test_2 <-cbind(bh_test_2,diel)

monthly<- format(as.POSIXct(bh_test_2$datetimestamp,format="%Y-%m-%d H:M"), "%Y-%m")
bh_test_2<-cbind(bh_test_2,monthly)

#Chnage to include year so that they are unique
bh_test_2 <- bh_test_2 %>% 
  mutate(Month = month(datetimestamp)) %>% 
  mutate(Season = ifelse(Month == 1 | Month == 2 | Month == 12, "Winter",ifelse(Month == 3 | Month == 4 | Month == 5, "Spring",ifelse(Month == 6 | Month == 7 | Month == 8, "Summer",ifelse(Month == 9 | Month == 10 | Month == 11, "Fall",NA)))))

#Testing my calculations for CV = STD/mean
#If any NA's, will not calculate the CV
bh_calculations_diel <- bh_test_2 %>% group_by(diel) %>% summarise(diel_STD = sd(do_mgl), diel_AVG = mean(do_mgl)) %>% mutate(diel_CV=(diel_STD/diel_AVG))
bh_test_2 <- merge(bh_test_2, bh_calculations_diel, by="diel")


bh_calculations_monthly <- bh_test_2 %>% group_by(monthly) %>% summarise(monthly_STD = sd(do_mgl, na.rm=TRUE), monthly_AVG = mean(do_mgl,na.rm=TRUE)) %>% mutate(monthly_CV=(monthly_STD/monthly_AVG))
bh_test_2 <- merge(bh_test_2, bh_calculations_monthly, by="monthly")


bh_calculations_seasonal <- bh_test_2 %>% group_by(Season) %>% summarise(seasonal_STD = sd(do_mgl), seasonal_AVG = mean(do_mgl)) %>% mutate(seasonal_CV=(seasonal_STD/seasonal_AVG))
bh_test_2 <- merge(bh_test_2, bh_calculations_monthly, by="Season")
#Season is not unique, so will have issue merging. 


#2007
ggplot(bh_test_2, aes(x=datetimestamp, y=do_mgl)) +
  geom_line() +
  xlab("2007") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("gndbhwq MC DO over 2007") +
  theme_bw()

#2008
bh_test_2<- bh_test[bh_test$datetimestamp>='2016-01-01 00:00' & bh_test$datetimestamp<='2016-12-31 23:45',]
ggplot(bh_test_2, aes(x=datetimestamp, y=do_mgl)) +
  geom_line() +
  xlab("2008") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("gndbhwq MC DO over 2008") +
  theme_bw()








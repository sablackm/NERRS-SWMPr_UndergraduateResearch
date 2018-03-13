install.packages("SWMPr")
install.packages("tidyverse")
install.packages("dplyr")
install.packages("tidyr")
install.packages("lubridate")

rm(list = ls())


library("SWMPr")
library("tidyverse")
library("dplyr")
library("ggplot2")
library("reshape2")
library("lubridate")

setwd("C:/Users/sabla/Documents/Research/plots")

#SOUTHEAST DATA
path <- "C:/Users/sabla/Documents/Research/SecondDownload_Current/SouthEast"

#GULF DATA
path2 <- "C:/Users/sabla/Documents/Research/SecondDownload_Current/GulfOfMexico"
#path <- "/Users/samuelblackman/Desktop/Research/SouthEast"

sitename = 'acefcwq'
sitenames <- c('acemcwq', 'acespwq','gtmpcwq','niwcbwq','niwdcwq', 'niwolwq', 'niwtawq','nocrcwq','sapdcwq','acejiwq','gtmpiwq','gtmfmwq','gtmsswq','noclcwq','noczbwq','nocecwq','sapcawq','saphdwq','sapldwq')

sitename2 = 'apacpwq'
sitenames2 <- c('apadbwq', 'gndpcwq', 'gndblwq', 'marabwq', 'marcewq','marcwwq', 'marmbwq', 'wkbfrwq','wkbwbwq', 'wkbmbwq', 'apalmwq','gndbhwq','gndbcwq', 'wkbmrwq')

data_collected <- import_local(path, sitename, trace = FALSE)
wq_dat <- qaqc(data_collected)
Sitecode <- rep(sitename,nrow(wq_dat))
wq_dat <- cbind(wq_dat, Sitecode)

data_collected <- import_local(path, sitenames[1], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames[1],nrow(current))
current <- cbind(current, Sitecode)

wq_dat <- rbind(wq_dat, current)

data_collected <- import_local(path, sitenames[2], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames[2],nrow(current))
current <- cbind(current, Sitecode)

wq_dat <- rbind(wq_dat, current)

data_collected <- import_local(path, sitenames[3], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames[3],nrow(current))
current <- cbind(current, Sitecode)

wq_dat <- rbind(wq_dat, current)

data_collected <- import_local(path, sitenames[4], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames[4],nrow(current))
current <- cbind(current, Sitecode)

wq_dat <- rbind(wq_dat, current)

data_collected <- import_local(path, sitenames[5], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames[5],nrow(current))
current <- cbind(current, Sitecode)

wq_dat <- rbind(wq_dat, current)

data_collected <- import_local(path, sitenames[6], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames[6],nrow(current))
current <- cbind(current, Sitecode)

wq_dat <- rbind(wq_dat, current)

data_collected <- import_local(path, sitenames[7], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames[7],nrow(current))
current <- cbind(current, Sitecode)

wq_dat <- rbind(wq_dat, current)

data_collected <- import_local(path, sitenames[8], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames[9],nrow(current))
current <- cbind(current, Sitecode)

wq_dat <- rbind(wq_dat, current)

data_collected <- import_local(path, sitenames[10], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames[10],nrow(current))
current <- cbind(current, Sitecode)

wq_dat <- rbind(wq_dat, current)

data_collected <- import_local(path, sitenames[11], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames[11],nrow(current))
current <- cbind(current, Sitecode)

wq_dat <- rbind(wq_dat, current)

data_collected <- import_local(path, sitenames[12], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames[12],nrow(current))
current <- cbind(current, Sitecode)

wq_dat <- rbind(wq_dat, current)

data_collected <- import_local(path, sitenames[13], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames[13],nrow(current))
current <- cbind(current, Sitecode)

wq_dat <- rbind(wq_dat, current)

data_collected <- import_local(path, sitenames[14], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames[14],nrow(current))
current <- cbind(current, Sitecode)

wq_dat <- rbind(wq_dat, current)

data_collected <- import_local(path, sitenames[15], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames[15],nrow(current))
current <- cbind(current, Sitecode)

wq_dat <- rbind(wq_dat, current)

data_collected <- import_local(path, sitenames[16], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames[16],nrow(current))
current <- cbind(current, Sitecode)

wq_dat <- rbind(wq_dat, current)

data_collected <- import_local(path, sitenames[17], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames[17],nrow(current))
current <- cbind(current, Sitecode)

wq_dat <- rbind(wq_dat, current)

data_collected <- import_local(path, sitenames[18], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames[18],nrow(current))
current <- cbind(current, Sitecode)

wq_dat <- rbind(wq_dat, current)

data_collected <- import_local(path, sitenames[19], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames[19],nrow(current))
current <- cbind(current, Sitecode)

wq_dat <- rbind(wq_dat, current)

View(wq_dat)

wq_dat <- wq_dat %>% 
  mutate(Month = month(datetimestamp)) %>% 
  mutate(Season = ifelse(Month == 1 | Month == 2 | Month == 12, "Winter",ifelse(Month == 3 | Month == 4 | Month == 5, "Spring",ifelse(Month == 6 | Month == 7 | Month == 8, "Summer",ifelse(Month == 9 | Month == 10 | Month == 11, "Fall",NA))))) %>%
  mutate(Estuary_Type = ifelse(Sitecode == 'acefcwq' | Sitecode == 'acemcwq' | Sitecode == 'acespwq' | Sitecode == 'gtmpcwq' | Sitecode == 'niwcbwq' | Sitecode == 'niwdcwq' | Sitecode == 'niwolwq' | Sitecode == 'niwtawq' | Sitecode == 'nocrcwq' | Sitecode == 'sapdcwq', "Marsh Creek", ifelse(Sitecode == 'acejiwq' |  Sitecode == 'gtmpiwq' | Sitecode == 'gtmfmwq' | Sitecode == 'gtmsswq' | Sitecode == 'noclcwq' | Sitecode == 'noczbwq' |  Sitecode == 'nocecwq' | Sitecode == 'sapacawq' | Sitecode == 'saphdwq' | Sitecode == 'sapldwq', "Open Water", NA)))
View(wq_dat)

#------------------------------------------------------------------Gulf

data_collected <- import_local(path2, sitename2, trace = FALSE)
wq_dat2 <- qaqc(data_collected)
Sitecode <- rep(sitename2,nrow(wq_dat2))
wq_dat2 <- cbind(wq_dat2, Sitecode)

data_collected <- import_local(path2, sitenames2[1], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames2[1],nrow(current))
current <- cbind(current, Sitecode)

wq_dat2 <- rbind(wq_dat2, current)

data_collected <- import_local(path2, sitenames2[2], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames2[2],nrow(current))
current <- cbind(current, Sitecode)

wq_dat2 <- rbind(wq_dat2, current)

data_collected <- import_local(path2, sitenames2[3], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames2[3],nrow(current))
current <- cbind(current, Sitecode)

wq_dat2 <- rbind(wq_dat2, current)

data_collected <- import_local(path2, sitenames2[4], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames2[4],nrow(current))
current <- cbind(current, Sitecode)

wq_dat2 <- rbind(wq_dat2, current)

data_collected <- import_local(path2, sitenames2[5], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames2[5],nrow(current))
current <- cbind(current, Sitecode)

wq_dat2 <- rbind(wq_dat2, current)

data_collected <- import_local(path2, sitenames2[6], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames2[6],nrow(current))
current <- cbind(current, Sitecode)

wq_dat2 <- rbind(wq_dat2, current)

data_collected <- import_local(path2, sitenames2[7], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames2[7],nrow(current))
current <- cbind(current, Sitecode)

wq_dat2 <- rbind(wq_dat2, current)

data_collected <- import_local(path2, sitenames2[8], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames2[8],nrow(current))
current <- cbind(current, Sitecode)

wq_dat2 <- rbind(wq_dat2, current)

data_collected <- import_local(path2, sitenames2[9], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames2[9],nrow(current))
current <- cbind(current, Sitecode)

wq_dat2 <- rbind(wq_dat2, current)

data_collected <- import_local(path2, sitenames2[10], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames2[10],nrow(current))
current <- cbind(current, Sitecode)

wq_dat2 <- rbind(wq_dat2, current)

data_collected <- import_local(path2, sitenames2[11], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames2[11],nrow(current))
current <- cbind(current, Sitecode)

wq_dat2 <- rbind(wq_dat2, current)

data_collected <- import_local(path2, sitenames2[12], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames2[12],nrow(current))
current <- cbind(current, Sitecode)

wq_dat2 <- rbind(wq_dat2, current)

data_collected <- import_local(path2, sitenames2[13], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames2[13],nrow(current))
current <- cbind(current, Sitecode)

wq_dat2 <- rbind(wq_dat2, current)

data_collected <- import_local(path2, sitenames2[14], trace = FALSE)
current <- qaqc(data_collected)
Sitecode <- rep(sitenames2[14],nrow(current))
current <- cbind(current, Sitecode)

wq_dat2 <- rbind(wq_dat2, current)

wq_dat2 <- wq_dat2 %>% 
  mutate(Month = month(datetimestamp)) %>% 
  mutate(Season = ifelse(Month == 1 | Month == 2 | Month == 12, "Winter",ifelse(Month == 3 | Month == 4 | Month == 5, "Spring",ifelse(Month == 6 | Month == 7 | Month == 8, "Summer",ifelse(Month == 9 | Month == 10 | Month == 11, "Fall",NA))))) %>%
  mutate(Estuary_Type = ifelse(Sitecode == 'apacpwq' | Sitecode == 'apadbwq' | Sitecode == 'gndpcwq' | Sitecode == 'gndblwq' | Sitecode == 'marabwq' | Sitecode == 'marcewq' | Sitecode == 'marcwwq' | Sitecode == 'marambwq' | Sitecode == 'wkbfrwq' | Sitecode == 'wkbwbwq' | Sitecode == 'wkbmbwq', 'Open Water', ifelse(Sitecode == 'apalmwq' | Sitecode == 'gndbhwq' | Sitecode == 'gndbcwq' | Sitecode == 'wkbmrwq','Marsh Creek', NA)))
View(wq_dat2)

#Combine Gulf and Southeast
wq_dat <- rbind(wq_dat, wq_dat2)
save <- wq_dat

#-----------------------------------------------------------------------------------------------------------------
#Seasonal Plots
#SOUTHEAST
sub_dat <- subset(wq_dat, wq_dat$Estuary_Type == "Marsh Creek")
sub_dat_2 <- sub_dat[sub_dat$datetimestamp>='2007-01-01 00:00' & sub_dat$datetimestamp<='2016-12-31 23:45',]

sub_dat_2 <- swmpr(sub_dat_2, "Southeast_Seasonsal_1")

jpeg(file='MC_DO.jpg', width = 600, height = 700, units = "px")
p1 <- ggplot(sub_dat_2, aes(x=Season, y=do_mgl)) +
  geom_boxplot(outlier.shape = NA) +
  xlab("Seasons") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("Marsh Creek Seasonal DO", subtitle = "2007-2016") +
  theme_bw()

print(p1)
dev.off()
  

jpeg(file='MC_PH.jpg', width = 600, height = 700, units = "px")
p2 <- ggplot(sub_dat_2, aes(x=Season, y=ph)) +
  geom_boxplot(outlier.shape = NA) +
  xlab("Seasons") +
  ylab("pH")+
  ylim(c(5,10)) +
  ggtitle("Marsh Creek Seasonal pH", subtitle = "2007-2016") +
  theme_bw()
  
print(p2)
dev.off()

sub_dat <- subset(wq_dat, wq_dat$Estuary_Type == "Open Water")
sub_dat_2 <- sub_dat[sub_dat$datetimestamp>='2007-01-01 00:00' & sub_dat$datetimestamp<='2016-12-31 23:45',]

sub_dat_2 <- swmpr(sub_dat_2, "Southeast_Seasonsal_2")

jpeg(file='OW_DO.jpg', width = 600, height = 700, units = "px")
p3 <- ggplot(sub_dat_2, aes(x=Season, y=do_mgl)) +
  geom_boxplot(outlier.shape = NA) +
  xlab("Seasons") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("Open Water Seasonal DO", subtitle = "2007-2016") +
  theme_bw()

print(p3)
dev.off()

jpeg(file='OW_PH.jpg', width = 600, height = 700, units = "px")
p4 <- ggplot(sub_dat_2, aes(x=Season, y=ph)) +
  geom_boxplot(outlier.shape = NA) +
  xlab("Seasons") +
  ylab("pH")+
  ylim(c(5,10)) +
  ggtitle("Open water Seasonal pH", subtitle = "2007-2016") +
  theme_bw()

print(p4)
dev.off()


#DO plot overtime series 2007-2016

sub_dat <- subset(wq_dat, wq_dat$Estuary_Type == "Open Water")
sub_dat_2 <- sub_dat[sub_dat$datetimestamp>='2007-01-01 00:00' & sub_dat$datetimestamp<='2016-12-31 23:45',]

sub_dat_2 <- swmpr(sub_dat_2, "Southeast_Seasonsal_2")

plot(do_mgl ~ datetimestamp, sub_dat_2)

jpeg(file='OW_DO_timeseries.jpg', width = 600, height = 700, units = "px")
p5 <- ggplot(sub_dat_2, aes(x=datetimestamp, y=do_mgl)) +
  geom_line() +
  xlab("2007-2016") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("Open Water DO over 2007-2016") +
  theme_bw()

print(p5)
dev.off()

plot(ph ~ datetimestamp, sub_dat_2)

jpeg(file='OW_PH_timeseries.jpg', width = 600, height = 700, units = "px")
p6 <- ggplot(sub_dat_2, aes(x=datetimestamp, y=ph)) +
  geom_feqpoly() +
  xlab("2007-2016") +
  ylab("pH")+
  ylim(c(5,10)) +
  ggtitle("Open water over pH 2007-2016") +
  theme_bw()

print(p6)
dev.off()

sub_dat <- subset(wq_dat, wq_dat$Estuary_Type == "Marsh Creek")
sub_dat_2 <- sub_dat[sub_dat$datetimestamp>='2007-01-01 00:00' & sub_dat$datetimestamp<='2016-12-31 23:45',]

sub_dat_2 <- swmpr(sub_dat_2, "Southeast_Seasonsal_1")

plot(do_mgl ~ datetimestamp, sub_dat_2)

jpeg(file='MC_DO_timeseries.jpg', width = 600, height = 700, units = "px")
p7 <- ggplot(sub_dat_2, aes(x=datetimestamp, y=do_mgl)) +
  geom_dotplot() +
  xlab("2007-2016") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("Marsh Creek DO over 2007-2016") +
  theme_bw()

print(p7)
dev.off()

plot(ph ~ datetimestamp, sub_dat_2)

jpeg(file='MC_PH_timeseries.jpg', width = 600, height = 700, units = "px")
p8 <- ggplot(sub_dat_2, aes(x=datetimestamp, y=ph)) +
  geom_dotplot() +
  xlab("2007-2016") +
  ylab("pH")+
  ylim(c(5,10)) +
  ggtitle("Marsh Creek over pH 2007-2016") +
  theme_bw()

print(p8)
dev.off()




















#GULF
# 
# sub_dat <- subset(wq_dat2, wq_dat2$Estuary_Type == "Marsh Creek")
# sub_dat_2 <- sub_dat[sub_dat$datetimestamp>='2007-01-01 00:00' & sub_dat$datetimestamp<='2016-12-31 23:45',]
# 
# sub_dat_2 <- swmpr(sub_dat_2, "Gulf_Seasonsal_1")
# 
# jpeg(file='G_MC_DO.jpg', width = 600, height = 700, units = "px")
# p1 <- ggplot(sub_dat_2, aes(x=Season, y=do_mgl)) +
#   geom_boxplot(outlier.shape = NA) +
#   xlab("Seasons") +
#   ylab("DO (mg/L)")+
#   ggtitle("Gulf of Mexico Marsh Creek Seasonal DO", subtitle = "2007-2016") +
#   theme_bw()
# 
# print(p1)
# dev.off()
# 
# jpeg(file='G_MC_PH.jpg', width = 600, height = 700, units = "px")
# p2 <- ggplot(sub_dat_2, aes(x=Season, y=ph)) +
#   geom_boxplot(outlier.shape = NA) +
#   xlab("Seasons") +
#   ylab("pH")+
#   ggtitle("Gulf of Mexico Marsh Creek Seasonal pH", subtitle = "2007-2016") +
#   theme_bw()
# 
# print(p2)
# dev.off()
# 
# sub_dat <- subset(wq_dat2, wq_dat2$Estuary_Type == "Open Water")
# sub_dat_2 <- sub_dat[sub_dat$datetimestamp>='2007-01-01 00:00' & sub_dat$datetimestamp<='2016-12-31 23:45',]
# 
# sub_dat_2 <- swmpr(sub_dat_2, "Gulf_Seasonsal_2")
# 
# jpeg(file='G_OW_DO.jpg', width = 600, height = 700, units = "px")
# p3 <- ggplot(sub_dat_2, aes(x=Season, y=do_mgl)) +
#   geom_boxplot(outlier.shape = NA) +
#   xlab("Seasons") +
#   ylab("DO (mg/L)")+
#   ggtitle("Gulf of Mexico Open Water Seasonal DO", subtitle = "2007-2016") +
#   theme_bw()
# 
# print(p3)
# dev.off()
# 
# jpeg(file='G_OW_PH.jpg', width = 600, height = 700, units = "px")
# p4 <- ggplot(sub_dat_2, aes(x=Season, y=ph)) +
#   geom_boxplot(outlier.shape = NA) +
#   xlab("Seasons") +
#   ylab("pH")+
#   ggtitle("Gulf of Mexico Open water Seasonal pH", subtitle = "2007-2016") +
#   theme_bw()
# 
# print(p4)
# dev.off()


#Oyster Reefs:
#Gulf

data_collected <- import_local(path2, 'apacpwq', trace = FALSE)
reef1 <- qaqc(data_collected)
Sitecode <- rep(sitename,nrow(reef1))
est_type <-rep('Oyster Reef', nrow(reef1))
reef1 <- cbind(reef1, Sitecode)

data_collected <- import_local(path2, 'apadbwq', trace = FALSE)
reef2 <- qaqc(data_collected)
Sitecode <- rep(sitename,nrow(reef2))
est_type <-rep('Oyster Reef', nrow(reef2))
reef2 <- cbind(reef2, Sitecode)

gulf_reefs <- rbind(reef1, reef2)

gulf_reefs <- gulf_reefs %>% 
  mutate(Month = month(datetimestamp)) %>% 
  mutate(Season = ifelse(Month == 1 | Month == 2 | Month == 12, "Winter",ifelse(Month == 3 | Month == 4 | Month == 5, "Spring",ifelse(Month == 6 | Month == 7 | Month == 8, "Summer",ifelse(Month == 9 | Month == 10 | Month == 11, "Fall",NA)))))

View(gulf_reefs)

sub_dat <- gulf_reefs[gulf_reefs$datetimestamp>='2007-01-01 00:00' & gulf_reefs$datetimestamp<='2016-12-31 23:45',]

sub_dat <- swmpr(sub_dat, "Gulf_Seasonsal_3")

jpeg(file='G_OR_DO.jpg', width = 600, height = 700, units = "px")
p1 <- ggplot(sub_dat, aes(x=Season, y=do_mgl)) +
  geom_boxplot(outlier.shape = NA) +
  xlab("Seasons") +
  ylab("DO (mg/L)")+
  ggtitle("Gulf of Mexico Oyster Reef Seasonal DO", subtitle = "2007-2016") +
  theme_bw()

print(p1)
dev.off()

jpeg(file='G_OR_PH.jpg', width = 600, height = 700, units = "px")
p2 <- ggplot(sub_dat, aes(x=Season, y=ph)) +
  geom_boxplot(outlier.shape = NA) +
  xlab("Seasons") +
  ylab("pH")+
  ggtitle("Gulf of Mexico Oyster Reef Seasonal pH", subtitle = "2007-2016") +
  theme_bw()

print(p2)
dev.off()

#Southeast

data_collected <- import_local(path, 'sapdcwq', trace = FALSE)
reef1 <- qaqc(data_collected)
Sitecode <- rep(sitename,nrow(reef1))
est_type <-rep('Oyster Reef', nrow(reef1))
reef1 <- cbind(reef1, Sitecode)

data_collected <- import_local(path, 'acespwq', trace = FALSE)
reef2 <- qaqc(data_collected)
Sitecode <- rep(sitename,nrow(reef2))
est_type <-rep('Oyster Reef', nrow(reef2))
reef2 <- cbind(reef2, Sitecode)

data_collected <- import_local(path,'sapcawq', trace = FALSE)
reef3 <- qaqc(data_collected)
Sitecode <- rep(sitename,nrow(reef3))
est_type <-rep('Oyster Reef', nrow(reef3))
reef3 <- cbind(reef3, Sitecode)


s_east_reefs <-rbind(reef1, reef2)
s_east_reefs <-rbind(s_east_reefs, reef3)

s_east_reefs <- s_east_reefs %>% 
  mutate(Month = month(datetimestamp)) %>% 
  mutate(Season = ifelse(Month == 1 | Month == 2 | Month == 12, "Winter",ifelse(Month == 3 | Month == 4 | Month == 5, "Spring",ifelse(Month == 6 | Month == 7 | Month == 8, "Summer",ifelse(Month == 9 | Month == 10 | Month == 11, "Fall",NA)))))

sub_dat <- s_east_reefs[s_east_reefs$datetimestamp>='2007-01-01 00:00' & s_east_reefs$datetimestamp<='2016-12-31 23:45',]

sub_dat <- swmpr(sub_dat, "Southeast_Seasonsal_3")


jpeg(file='SE_OR_DO.jpg', width = 600, height = 700, units = "px")
p1 <- ggplot(sub_dat, aes(x=Season, y=do_mgl)) +
  geom_boxplot(outlier.shape = NA) +
  xlab("Seasons") +
  ylab("DO (mg/L)")+
  ggtitle("Southeast Oyster Reef Seasonal DO", subtitle = "2007-2016") +
  theme_bw()

print(p1)
dev.off()

jpeg(file='SE_OR_PH.jpg', width = 600, height = 700, units = "px")
p2 <- ggplot(sub_dat, aes(x=Season, y=ph)) +
  geom_boxplot(outlier.shape = NA) +
  xlab("Seasons") +
  ylab("pH")+
  ggtitle("Southeast Oyster Reef Seasonal pH", subtitle = "2007-2016") +
  theme_bw()

print(p2)
dev.off()


#DO plot overtime series 2007-2016










#pH plot overtime series 2007-2016












#USED TO MERGE IN METADATA
#Enter the name of the sitecode again here (used for pulling meta data)
#Sitecode <- rep("nocecwq   ",nrow(wq_dat))
#wq_dat <- cbind(wq_dat, Sitecode)

#station_meta_data<-read.csv("C:/Users/sabla/Documents/Research/SecondDownload_Current/sampling_stations_edited.csv")
#station_meta_data<-read.csv("/Users/samuelblackman/Desktop/Research/sampling_stations_edited.csv")
#wq_dat <- merge(wq_dat,station_meta_data, by=c("Sitecode"), all=TRUE)

#Pull in meta data that I extrapolated from the meta docs and attach it to the dataset
#extra_metadata<-read.csv("C:/Users/sabla/Documents/Research/Southeast_NERRS_WQ_EstuaryTypes_finished.csv", fileEncoding="UTF-8-BOM")
#extra_metadata<-read.csv("/Users/samuelblackman/Desktop/Research/Southeast_NERRS_WQ_EstuaryTypes_finished.csv")
#wq_dat <- merge(wq_dat,extra_metadata, by=c("Sitecode"), all=TRUE)

# #wq_dat <- swmpr(wq_dat, sitename)
# #sub_dat2 <- subset(sub_dat, subset = c('2007-01-01 00:00', '2017-12-31 23:45'), select= c('do_mgl','ph'))
# 
# agg_dat <- aggreswmp(sub_dat_2, by=Season, params=c('do_mgl','ph'),aggs_out = T)
# to_plot <- melt(agg_dat, id.var = 'datetimestamp')
# 
# p <- ggplot(to_plot, aes(x=Season, y=value)) + 
#   geom_boxplot() + 
#   facet_wrap(~variable, ncol = 1, scales = 'free_y') + 
#   theme_bw() + 
#   theme(axis.text.x = element_text(size=5)) +
#   xlab("Seasons") +
#   ylab("pH and DO (mg/L)")

# sub_dat <- subset(wq_dat,subset = c('2002-04-01 00:00', '2018-01-01 00:00'), select= c('do_mgl','ph','chlfluor'))
# agg_dat <- aggreswmp(sub_dat, by='quarters', params=c('chlfluor'),aggs_out = T)
# to_plot <- melt(agg_dat, id.var = 'datetimestamp')
# 
# p <- ggplot(to_plot, aes(x=factor(datetimestamp), y=value)) + 
#   geom_boxplot() + 
#   facet_wrap(~variable, ncol = 1, scales = 'free_y') + 
#   theme_bw() + 
#   theme(axis.text.x = element_text(size=5)) +
#   xlab("Seasons(Quarterly)") +
#   ylab("Chlorophyll (ug/L)")

#Learning SWMPr
#------------------------------------------------------------------------------------------------
# help.search(package = 'SWMPr', "organize")
# help.search(package = 'SWMPr', "retrieve")
# 
# swmp1 <- import_local("C:/Users/sabla/Documents/Research/SecondDownload_Current/SouthEast", "nocecwq", trace = FALSE)
# cleaned_swmp1 <- qaqc(swmp1)
# dat <- subset(cleaned_swmp1, select = 'do_mgl', subset = c('2006-1-01 00:00', '2017-01-01 00:00'))
# 
# #dat <- subset(cleaned_swmp1, select = 'do_mgl', subset = c('2013-07-01 00:00', '2013-07-31 00:00'))
# 
# plot(dat)
# plot(do_mgl ~ datetimestamp, dat)
# plot(dat, type = 'n')
# lines(dat, col = 'red')
# 
# 
# # whole dataset
# summary(dat)
# 
# # individual variables
# summary(dat$do_mgl)
# 
# # mean, range, var, etc.
# # note use of na.rm
# mean(dat$do_mgl, na.rm = T)
# range(dat$do_mgl, na.rm = T)
# var(dat$do_mgl, na.rm = T)
# sd(dat$do_mgl, na.rm = T)
# min(dat$do_mgl, na.rm = T)
# max(dat$do_mgl, na.rm = T)
# 
# # how many missing values?
# sum(is.na(dat$do_mgl))

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
#setwd("/Users/samuelblackman/Desktop/Research")
#SOUTHEAST DATA
#path <- "C:/Users/sabla/Documents/Research/SecondDownload_Current/SouthEast"

#GULF DATA
#path2 <- "C:/Users/sabla/Documents/Research/SecondDownload_Current/GulfOfMexico"
path2 <- "/Users/samuelblackman/Desktop/Research/GulfOfMexico"
path <- "/Users/samuelblackman/Desktop/Research/SouthEast"

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

#plot(do_mgl ~ datetimestamp, sub_dat_2)

jpeg(file='OW_DO_timeseries.jpg', width = 600, height = 700, units = "px")
p5 <- ggplot(sub_dat_2, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("DO (mg/L)")+
  ylim(c(0,25)) +
  ggtitle("Open Water DO over 2007-2016") +
  theme_bw()

print(p5)
dev.off()

#plot(ph ~ datetimestamp, sub_dat_2)

jpeg(file='OW_PH_timeseries.jpg', width = 600, height = 700, units = "px")
p6 <- ggplot(sub_dat_2, aes(x=datetimestamp, y=ph)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("pH")+
  ylim(c(2,12)) +
  ggtitle("Open water over pH 2007-2016") +
  theme_bw()

print(p6)
dev.off()

sub_dat <- subset(wq_dat, wq_dat$Estuary_Type == "Marsh Creek")
sub_dat_2 <- sub_dat[sub_dat$datetimestamp>='2007-01-01 00:00' & sub_dat$datetimestamp<='2016-12-31 23:45',]

sub_dat_2 <- swmpr(sub_dat_2, "Southeast_Seasonsal_1")

#plot(do_mgl ~ datetimestamp, sub_dat_2)

jpeg(file='MC_DO_timeseries.jpg', width = 600, height = 700, units = "px")
p7 <- ggplot(sub_dat_2, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("DO (mg/L)")+
  ylim(c(0,25)) +
  ggtitle("Marsh Creek DO over 2007-2016") +
  theme_bw()

print(p7)
dev.off()

#plot(ph ~ datetimestamp, sub_dat_2)

jpeg(file='MC_PH_timeseries.jpg', width = 600, height = 700, units = "px")
p8 <- ggplot(sub_dat_2, aes(x=datetimestamp, y=ph)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("pH")+
  ylim(c(2,12)) +
  ggtitle("Marsh Creek over pH 2007-2016") +
  theme_bw()

print(p8)
dev.off()

#----------------------------------------------------------------------------------------
#Comparing individual sites:

path <- "C:/Users/sabla/Documents/Research/SecondDownload_Current/SouthEast"

sitename = 'sapdcwq'
sitename2='sapldwq'

data_collected <- import_local(path, sitename, trace = FALSE)
wq_dat <- qaqc(data_collected)
Sitecode <- rep(sitename,nrow(wq_dat))
dc <- cbind(wq_dat, Sitecode)

data_collected <- import_local(path, sitename2, trace = FALSE)
wq_dat <- qaqc(data_collected)
Sitecode <- rep(sitename2,nrow(wq_dat))
ld <- cbind(wq_dat, Sitecode)

setwd("C:/Users/sabla/Documents/Research/Plots_for_Statistical_Analysis")

sub_dat <- dc[dc$datetimestamp>='2007-01-01 00:00' & dc$datetimestamp<='2016-12-31 23:45',]

jpeg(file='sapdcwq_MC_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("sapdcwq MC DO over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='sapdcwq_MC_pH_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=ph)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("pH")+
  ylim(c(2,12)) +
  ggtitle("sapdcwq MC pH over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='sapdcwq_MC_Depth_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=depth)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("Depth (m)")+
  ylim(c(0,6)) +
  ggtitle("sapdcwq MC Depth over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

sub_dat <- ld[ld$datetimestamp>='2007-01-01 00:00' & ld$datetimestamp<='2016-12-31 23:45',]

jpeg(file='sapldwq_OW_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("sapldwq OW DO over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='sapldwq_OW_pH_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=ph)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("pH")+
  ylim(c(2,12)) +
  ggtitle("sapldwq OW pH over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='sapldwq_OW_Depth_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=depth)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("Depth (m)")+
  ylim(c(0,6)) +
  ggtitle("sapldwq OW Depth over 2007-2016") +
  theme_bw()

print(set1)
dev.off()
#--------------------------------------------------------------------------------------------------
path <- "C:/Users/sabla/Documents/Research/SecondDownload_Current/GulfOfMexico"

sitename = 'gndbhwq'
sitename2='gndpcwq'

data_collected <- import_local(path, sitename, trace = FALSE)
wq_dat <- qaqc(data_collected)
Sitecode <- rep(sitename,nrow(wq_dat))
bh <- cbind(wq_dat, Sitecode)

data_collected <- import_local(path, sitename2, trace = FALSE)
wq_dat <- qaqc(data_collected)
Sitecode <- rep(sitename2,nrow(wq_dat))
pc <- cbind(wq_dat, Sitecode)


setwd("C:/Users/sabla/Documents/Research/Plots_for_Statistical_Analysis")

sub_dat <- bh[bh$datetimestamp>='2007-01-01 00:00' & bh$datetimestamp<='2016-12-31 23:45',]

jpeg(file='gndbhwq_MC_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("gndbhwq MC DO over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='gndbhwq_MC_pH_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=ph)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("pH")+
  ylim(c(2,12)) +
  ggtitle("gndbhwq MC pH over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='gndbhwq_MC_Depth_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=depth)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("Depth (m)")+
  ylim(c(0,6)) +
  ggtitle("gndbhwq MC Depth over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

sub_dat <- pc[pc$datetimestamp>='2007-01-01 00:00' & pc$datetimestamp<='2016-12-31 23:45',]

jpeg(file='gndpcwq_OW_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("gndpcwq OW DO over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='gndpcwq_OW_pH_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=ph)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("pH")+
  ylim(c(2,12)) +
  ggtitle("gndpcwq OW pH over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='gndpcwq_OW_Depth_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=depth)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("Depth (m)")+
  ylim(c(0,6)) +
  ggtitle("gndpcwq OW Depth over 2007-2016") +
  theme_bw()

print(set1)
dev.off()
#----------------------------------------------------------------------------------------------------------

path <- "C:/Users/sabla/Documents/Research/SecondDownload_Current/SouthEast"

sitename ='acefcwq'
sitename2='acemcwq'
sitename3='acebbwq'
sitename4='acespwq'

data_collected <- import_local(path, sitename, trace = FALSE)
wq_dat <- qaqc(data_collected)
Sitecode <- rep(sitename,nrow(wq_dat))
fc <- cbind(wq_dat, Sitecode)

data_collected <- import_local(path, sitename2, trace = FALSE)
wq_dat <- qaqc(data_collected)
Sitecode <- rep(sitename2,nrow(wq_dat))
mc <- cbind(wq_dat, Sitecode)

data_collected <- import_local(path, sitename3, trace = FALSE)
wq_dat <- qaqc(data_collected)
Sitecode <- rep(sitename3,nrow(wq_dat))
bb <- cbind(wq_dat, Sitecode)

data_collected <- import_local(path, sitename4, trace = FALSE)
wq_dat <- qaqc(data_collected)
Sitecode <- rep(sitename4,nrow(wq_dat))
sp <- cbind(wq_dat, Sitecode)


setwd("C:/Users/sabla/Documents/Research/Plots_for_Statistical_Analysis")

sub_dat <- fc[fc$datetimestamp>='2007-01-01 00:00' & fc$datetimestamp<='2016-12-31 23:45',]

jpeg(file='acefcwq_MC_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("acefcwq MC DO over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='acefcwq_MC_pH_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=ph)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("pH")+
  ylim(c(2,12)) +
  ggtitle("acefcwq MC pH over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='acefcwq_MC_Depth_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=depth)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("Depth (m)")+
  ylim(c(0,6)) +
  ggtitle("acefcwq MC Depth over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

sub_dat <- mc[mc$datetimestamp>='2007-01-01 00:00' & mc$datetimestamp<='2016-12-31 23:45',]

jpeg(file='acemcwq_OW_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("acemcwq OW DO over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='acemcwq_OW_pH_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=ph)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("pH")+
  ylim(c(2,12)) +
  ggtitle("acemcwq OW pH over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='acemcwq_OW_Depth_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=depth)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("Depth (m)")+
  ylim(c(0,6)) +
  ggtitle("acemcwq OW Depth over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

sub_dat <- bb[bb$datetimestamp>='2007-01-01 00:00' & bb$datetimestamp<='2015-01-07 11:45',]

jpeg(file='acebbwq_OW_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("acebbwq OW DO over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='acebbwq_OW_pH_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=ph)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("pH")+
  ylim(c(2,12)) +
  ggtitle("acebbwq OW pH over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='acebbwq_OW_Depth_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=depth)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("Depth (m)")+
  ylim(c(0,6)) +
  ggtitle("acebbwq OW Depth over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

sub_dat <- sp[sp$datetimestamp>='2007-01-01 00:00' & sp$datetimestamp<='2016-12-31 23:45',]

jpeg(file='acespwq_MC_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("acespwq MC DO over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='acespwq_MC_pH_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=ph)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("pH")+
  ylim(c(2,12)) +
  ggtitle("acespwq MC pH over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='acespwq_MC_Depth_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=depth)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("Depth (m)")+
  ylim(c(0,6)) +
  ggtitle("acespwq MC Depth over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

#-------------------------------------------------------------------------------------------
path <- "C:/Users/sabla/Documents/Research/SecondDownload_Current/SouthEast"

sitename = 'niwcbwq'
sitename2='niwdcwq'
sitename3= 'niwolwq'

data_collected <- import_local(path, sitename, trace = FALSE)
wq_dat <- qaqc(data_collected)
Sitecode <- rep(sitename,nrow(wq_dat))
cb <- cbind(wq_dat, Sitecode)

data_collected <- import_local(path, sitename2, trace = FALSE)
wq_dat <- qaqc(data_collected)
Sitecode <- rep(sitename2,nrow(wq_dat))
dc <- cbind(wq_dat, Sitecode)

data_collected <- import_local(path, sitename3, trace = FALSE)
wq_dat <- qaqc(data_collected)
Sitecode <- rep(sitename3,nrow(wq_dat))
ol <- cbind(wq_dat, Sitecode)

setwd("C:/Users/sabla/Documents/Research/Plots_for_Statistical_Analysis")

sub_dat <- cb[cb$datetimestamp>='2007-01-01 00:00' & cb$datetimestamp<='2016-12-31 23:45',]

jpeg(file='niwcbwq_OW_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("niwcbwq OW DO over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='niwcbwq_OW_pH_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=ph)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("pH")+
  ylim(c(2,12)) +
  ggtitle("niwcbwq OW pH over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='niwcbwq_OW_Depth_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=depth)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("Depth (m)")+
  ylim(c(0,6)) +
  ggtitle("niwcbwq OW Depth over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

sub_dat <- dc[dc$datetimestamp>='2007-01-01 00:00' & dc$datetimestamp<='2016-12-31 23:45',]

jpeg(file='niwdcwq_OW_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("niwdcwq OW DO over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='niwdcwq_OW_pH_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=ph)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("pH")+
  ylim(c(2,12)) +
  ggtitle("niwdcwq OW pH over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='niwdcwq_OW_Depth_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=depth)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("Depth (m)")+
  ylim(c(0,6)) +
  ggtitle("niwdcwq OW Depth over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

sub_dat <- ol[ol$datetimestamp>='2016-05-11 12:45' & ol$datetimestamp<='2018-02-09 1:30',]

jpeg(file='niwolwq_MC_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2016-2018") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("niwolwq MC DO over 2016-2018") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='niwolwq_MC_pH_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=ph)) +
  geom_point() +
  xlab("2016-2018") +
  ylab("pH")+
  ylim(c(2,12)) +
  ggtitle("niwolwq MC pH over 2016-2018") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='niwolwq_MC_Depth_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=depth)) +
  geom_point() +
  xlab("2016-2018") +
  ylab("Depth (m)")+
  ylim(c(0,6)) +
  ggtitle("niwolwq MC Depth over 2016-2018") +
  theme_bw()

print(set1)
dev.off()








#Across reserves comparisons
#--------------------------------------------------------------------
path <- "C:/Users/sabla/Documents/Research/SecondDownload_Current/SouthEast"

sitename = 'niwtawq'
sitename2='gtmsswq'
sitename3= 'niwwswq'

data_collected <- import_local(path, sitename, trace = FALSE)
wq_dat <- qaqc(data_collected)
Sitecode <- rep(sitename,nrow(wq_dat))
ta <- cbind(wq_dat, Sitecode)

data_collected <- import_local(path, sitename2, trace = FALSE)
wq_dat <- qaqc(data_collected)
Sitecode <- rep(sitename2,nrow(wq_dat))
ss <- cbind(wq_dat, Sitecode)

data_collected <- import_local(path, sitename3, trace = FALSE)
wq_dat <- qaqc(data_collected)
Sitecode <- rep(sitename3,nrow(wq_dat))
ws <- cbind(wq_dat, Sitecode)

setwd("C:/Users/sabla/Documents/Research/Plots_for_Statistical_Analysis")

sub_dat <- ta[ta$datetimestamp>='2007-01-01 00:00' & ta$datetimestamp<='2016-12-31 23:45',]

jpeg(file='niwtawq_MC_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("niwtawq MC DO over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='niwtawq_MC_pH_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=ph)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("pH")+
  ylim(c(2,12)) +
  ggtitle("niwtawq MC pH over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='niwtawq_MC_Depth_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=depth)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("Depth (m)")+
  ylim(c(0,6)) +
  ggtitle("niwtawq MC Depth over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

sub_dat <- ss[ss$datetimestamp>='2007-01-01 00:00' & ss$datetimestamp<='2016-12-31 23:45',]

jpeg(file='gtmsswq_OW_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("gtmsswq OW DO over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='gtmsswq_OW_pH_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=ph)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("pH")+
  ylim(c(2,12)) +
  ggtitle("gtmsswq OW pH over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='gtmsswq_OW_Depth_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=depth)) +
  geom_point() +
  xlab("2007-2016") +
  ylab("Depth (m)")+
  ylim(c(0,6)) +
  ggtitle("gtmsswq OW Depth over 2007-2016") +
  theme_bw()

print(set1)
dev.off()

sub_dat <- ws[ws$datetimestamp>='2016-05-11 12:45' & ws$datetimestamp<='2018-02-09 1:30',]

jpeg(file='niwwswq_OW_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2016-2018") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("niwwswq OW DO over 2016-2018") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='niwwswq_OW_pH_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=ph)) +
  geom_point() +
  xlab("2016-2018") +
  ylab("pH")+
  ylim(c(2,12)) +
  ggtitle("niwwswq OW pH over 2016-2018") +
  theme_bw()

print(set1)
dev.off()

jpeg(file='niwwswq_OW_Depth_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=depth)) +
  geom_point() +
  xlab("2016-2018") +
  ylab("Depth (m)")+
  ylim(c(0,6)) +
  ggtitle("niwwswq OW Depth over 2016-2018") +
  theme_bw()

print(set1)
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

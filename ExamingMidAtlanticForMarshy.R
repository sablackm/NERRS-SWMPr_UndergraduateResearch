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


#CBM Site, Potentially more datasets, have to email DNR MD
path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\MidAtlantic"
sitename = 'cbmrrwq'
data_collected <- import_local(path, sitename, trace = FALSE)
rr <- qaqc(data_collected)

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\MidAtlantic"
sitename = 'cbmipwq'
data_collected <- import_local(path, sitename, trace = FALSE)
ip <- qaqc(data_collected)

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\MidAtlantic"
sitename = 'cbmmcwq'
data_collected <- import_local(path, sitename, trace = FALSE)
mc <- qaqc(data_collected)

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\MidAtlantic"
sitename = 'cbmocwq'
data_collected <- import_local(path, sitename, trace = FALSE)
oc <- qaqc(data_collected)


setwd("C:/Users/sabla/Documents/Research/Plots/MidAtlantic")

sub_dat <- rr[rr$datetimestamp>='2007-01-01 00:00' & rr$datetimestamp<='2017-12-31 23:45',]

jpeg(file='cbmrrwq_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2017") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("cbmrrwq DO over 2007-2017") +
  theme_bw()

print(set1)
dev.off()

sub_dat <- ip[ip$datetimestamp>='2007-01-01 00:00' & ip$datetimestamp<='2017-12-31 23:45',]

jpeg(file='cbmipwq_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2017") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("cbmrrwq DO over 2007-2017") +
  theme_bw()

print(set1)
dev.off()

#POTENTIAL MARSHY
sub_dat <- mc[mc$datetimestamp>='2007-01-01 00:00' & mc$datetimestamp<='2017-12-31 23:45',]

jpeg(file='cbmmcwq_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2017") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("cbmmcwq DO over 2007-2017") +
  theme_bw()

print(set1)
dev.off()

sub_dat <- oc[oc$datetimestamp>='2007-01-01 00:00' & oc$datetimestamp<='2017-12-31 23:45',]

jpeg(file='cbmocwq_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2017") +
  ylab("DO (mg/L)")+
  ylim(c(0,25)) +
  ggtitle("cbmocwq DO over 2007-2017") +
  theme_bw()

print(set1)
dev.off()

#----------------------------------------------------------------------------------------------------

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\MidAtlantic"
sitename = 'cbvgiwq'
data_collected <- import_local(path, sitename, trace = FALSE)
gi <- qaqc(data_collected)

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\MidAtlantic"
sitename = 'cbvcbwq'
data_collected <- import_local(path, sitename, trace = FALSE)
cb <- qaqc(data_collected)

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\MidAtlantic"
sitename = 'cbvtcwq'
data_collected <- import_local(path, sitename, trace = FALSE)
tc <- qaqc(data_collected)

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\MidAtlantic"
sitename = 'cbvspwq'
data_collected <- import_local(path, sitename, trace = FALSE)
sp <- qaqc(data_collected)

setwd("C:/Users/sabla/Documents/Research/Plots/MidAtlantic")

sub_dat <- gi[gi$datetimestamp>='2007-01-01 00:00' & gi$datetimestamp<='2017-12-31 23:45',]

jpeg(file='cbvgiwq_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2017") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("cbvgiwq DO over 2007-2017") +
  theme_bw()

print(set1)
dev.off()

sub_dat <- cb[cb$datetimestamp>='2007-01-01 00:00' & cb$datetimestamp<='2017-12-31 23:45',]

jpeg(file='cbvcbwq_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2017") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("cbvcbwq DO over 2007-2017") +
  theme_bw()

print(set1)
dev.off()

sub_dat <- tc[tc$datetimestamp>='2007-01-01 00:00' & tc$datetimestamp<='2017-12-31 23:45',]

jpeg(file='cbvtcwq_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2017") +
  ylab("DO (mg/L)")+
  ylim(c(0,20)) +
  ggtitle("cbvtcwq DO over 2007-2017") +
  theme_bw()

print(set1)
dev.off()

sub_dat <- sp[sp$datetimestamp>='2007-01-01 00:00' & sp$datetimestamp<='2017-12-31 23:45',]

jpeg(file='cbvspwq_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2017") +
  ylab("DO (mg/L)")+
  ylim(c(0,15)) +
  ggtitle("cbvspwq DO over 2007-2017") +
  theme_bw()

print(set1)
dev.off()


#--------------------------------------------------------------------------------------------
#THESE ARE QUITE CURIOUS MARSHY
path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\MidAtlantic"
sitename = 'delblwq'
data_collected <- import_local(path, sitename, trace = FALSE)
bl<- qaqc(data_collected)

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\MidAtlantic"
sitename = 'deldswq'
data_collected <- import_local(path, sitename, trace = FALSE)
ds<- qaqc(data_collected)

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\MidAtlantic"
sitename = 'delllwq'
data_collected <- import_local(path, sitename, trace = FALSE)
ll <- qaqc(data_collected)

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\MidAtlantic"
sitename = 'delslpwq'
data_collected <- import_local(path, sitename, trace = FALSE)
sl <- qaqc(data_collected)

setwd("C:/Users/sabla/Documents/Research/Plots/MidAtlantic")

sub_dat <- bl[bl$datetimestamp>='2007-01-01 00:00' & bl$datetimestamp<='2017-12-31 23:45',]

jpeg(file='delblwq_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2017") +
  ylab("DO (mg/L)")+
  ylim(c(0,20)) +
  ggtitle("delblwq DO over 2007-2017") +
  theme_bw()

print(set1)
dev.off()

#Potential Marshy!!!
sub_dat <- ds[ds$datetimestamp>='2007-01-01 00:00' & ds$datetimestamp<='2017-12-31 23:45',]

jpeg(file='deldswq_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2017") +
  ylab("DO (mg/L)")+
  ylim(c(0,20)) +
  ggtitle("deldswq DO over 2007-2017") +
  theme_bw()

print(set1)
dev.off()

sub_dat <- ll[ll$datetimestamp>='2007-01-01 00:00' & ll$datetimestamp<='2017-12-31 23:45',]

jpeg(file='delllwq_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2017") +
  ylab("DO (mg/L)")+
  ylim(c(0,20)) +
  ggtitle("delllwq DO over 2007-2017") +
  theme_bw()

print(set1)
dev.off()

sub_dat <- sl[sl$datetimestamp>='2007-01-01 00:00' & sl$datetimestamp<='2017-12-31 23:45',]

jpeg(file='delslpwq_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2017") +
  ylab("DO (mg/L)")+
  ylim(c(0,20)) +
  ggtitle("delslpwq DO over 2007-2017") +
  theme_bw()

print(set1)
dev.off()

#-------------------------------------------------------------------------------------------------
path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\MidAtlantic"
sitename = 'jacb6wq'
data_collected <- import_local(path, sitename, trace = FALSE)
b6<- qaqc(data_collected)

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\MidAtlantic"
sitename = 'jacb9wq'
data_collected <- import_local(path, sitename, trace = FALSE)
b9<- qaqc(data_collected)

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\MidAtlantic"
sitename = 'jacnewq'
data_collected <- import_local(path, sitename, trace = FALSE)
ne <- qaqc(data_collected)

path <- "C:\\Users\\sabla\\Documents\\Research\\ThirdDownload_current\\MidAtlantic"
sitename = 'jacbawq'
data_collected <- import_local(path, sitename, trace = FALSE)
ba <- qaqc(data_collected)

setwd("C:/Users/sabla/Documents/Research/Plots/MidAtlantic")

sub_dat <- b6[b6$datetimestamp>='2007-01-01 00:00' & b6$datetimestamp<='2017-12-31 23:45',]

jpeg(file='jacb6wq_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2017") +
  ylab("DO (mg/L)")+
  ylim(c(0,20)) +
  ggtitle("jacb6wq DO over 2007-2017") +
  theme_bw()

print(set1)
dev.off()

#Potential Marshy!!!
sub_dat <- b9[b9$datetimestamp>='2007-01-01 00:00' & b9$datetimestamp<='2017-12-31 23:45',]

jpeg(file='jacb9wq_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2017") +
  ylab("DO (mg/L)")+
  ylim(c(0,20)) +
  ggtitle("jacb9wq DO over 2007-2017") +
  theme_bw()

print(set1)
dev.off()

sub_dat <- ne[ne$datetimestamp>='2007-01-01 00:00' & ne$datetimestamp<='2017-12-31 23:45',]

jpeg(file='jacnewq_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2017") +
  ylab("DO (mg/L)")+
  ylim(c(0,20)) +
  ggtitle("jacnewq DO over 2007-2017") +
  theme_bw()

print(set1)
dev.off()

sub_dat <- ba[ba$datetimestamp>='2007-01-01 00:00' & ba$datetimestamp<='2017-12-31 23:45',]

jpeg(file='jacbawq_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2017") +
  ylab("DO (mg/L)")+
  ylim(c(0,20)) +
  ggtitle("jacbawq DO over 2007-2017") +
  theme_bw()

print(set1)
dev.off()


#-------------More GND
path <- "C:\\Users\\sabla\\Documents\\Research\\SecondDownload_old\\GulfofMexico"
sitename = 'gndblwq'
data_collected <- import_local(path, sitename, trace = FALSE)
bl <- qaqc(data_collected)

path <- "C:\\Users\\sabla\\Documents\\Research\\SecondDownload_old\\GulfofMexico"
sitename = 'gndpcwq'
data_collected <- import_local(path, sitename, trace = FALSE)
pc <- qaqc(data_collected)


sub_dat <- bl[bl$datetimestamp>='2007-01-01 00:00' & bl$datetimestamp<='2017-12-31 23:45',]

jpeg(file='gndblwq_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2017") +
  ylab("DO (mg/L)")+
  ylim(c(0,20)) +
  ggtitle("gndblwq DO over 2007-2017") +
  theme_bw()

print(set1)
dev.off()

sub_dat <- pc[pc$datetimestamp>='2007-01-01 00:00' & pc$datetimestamp<='2017-12-31 23:45',]

jpeg(file='gndpcwq_DO_timeseries.jpg', width = 800, height = 600, units = "px")
set1 <- ggplot(sub_dat, aes(x=datetimestamp, y=do_mgl)) +
  geom_point() +
  xlab("2007-2017") +
  ylab("DO (mg/L)")+
  ylim(c(0,20)) +
  ggtitle("gndpcwq DO over 2007-2017") +
  theme_bw()

print(set1)
dev.off()

missing_pc_do <- round(sum(is.na(sub_dat$do_mgl))/nrow(sub_dat)*100,2)
missing_pc_do

sub_dat <- bl[bl$datetimestamp>='2007-01-01 00:00' & bl$datetimestamp<='2017-12-31 23:45',]

missing_bl_do <- round(sum(is.na(sub_dat$do_mgl))/nrow(sub_dat)*100,2)
missing_bl_do








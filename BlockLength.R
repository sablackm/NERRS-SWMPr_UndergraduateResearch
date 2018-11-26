#Determining Autocorrelation and Block length

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


#Data loading and mild cleaning
setwd("/Users/samuelblackman/Desktop/Research/NERRS/CurrentData")
mm <- read.csv("gndbhwq data.csv")
mc <- read.csv("gtmpcwq data.csv")
ow <- read.csv("niwdcwq data.csv")

mm$datetimestamp <- as.POSIXct(mm$datetimestamp, format="%Y-%m-%d %H:%M:%S")
mc$datetimestamp <- as.POSIXct(mc$datetimestamp, format="%Y-%m-%d %H:%M:%S")
ow$datetimestamp <- as.POSIXct(ow$datetimestamp, format="%Y-%m-%d %H:%M:%S")

mm <- na.omit(mm)
mc <- na.omit(mc)
ow <- na.omit(ow)

#Marshy Marsh Creek Example:

#By Season
mm_wi <- subset(mm, Month == 1 | Month == 2 | Month == 12)
#mm_wi <- mm_wi[mm_wi$datetimestamp>='2007-12-01 00:00' & mm_wi$datetimestamp<='2008-02-28 23:45',]
mm_wi <- mm_wi[,c(-1,-2,-10,-11,-12,-13,-14,-15)]

mm_su <- subset(mm, Month == 6 | Month == 7 | Month == 8)
#mm_su <- mm_su[mm_su$datetimestamp>='2008-06-16 00:00' & mm_su$datetimestamp<='2008-06-16 23:45',]
mm_su <- mm_su[,c(-1,-2,-10,-11,-12,-13,-14,-15)]

mm_acf <- acf(mm_su, lag.max = 100)

#All Seasons
mm_acf_pct <- acf(mm_wi$do_pct, lag.max = 1600)
mm_acf_mgl <- acf(mm_wi$do_mgl, lag.max = 4200)
mm_acf_temp <- acf(mm_wi$temp, lag.max = 2900)
mm_acf_sal <- acf(mm_wi$sal, lag.max = 5500)
mm_acf_depth <- acf(mm_wi$depth, lag.max = 40)
mm_acf_turb <- acf(mm_wi$turb,lag.max = 3200)

mm_acf_pct <- acf(mm_su$do_pct, lag.max = 1500)
mm_acf_mgl <- acf(mm_su$do_mgl, lag.max = 1500)
mm_acf_temp <- acf(mm_su$temp, lag.max = 3100)
mm_acf_sal <- acf(mm_su$sal, lag.max = 11000)# block length longer than season length (?)
mm_acf_depth <- acf(mm_su$depth, lag.max = 40)
mm_acf_turb <- acf(mm_su$turb,lag.max = 2600)

#Single Season

mm_acf_pct <- acf(mm_su$do_pct, lag.max = 280)
mm_acf_mgl <- acf(mm_su$do_mgl, lag.max = 280)
mm_acf_temp <- acf(mm_su$temp, lag.max = 1100)
mm_acf_sal <- acf(mm_su$sal, lag.max = 170)
mm_acf_depth <- acf(mm_su$depth, lag.max = 40)
mm_acf_turb <- acf(mm_su$turb,lag.max = 600)

#Single Day

mm_acf_pct <- acf(mm_su$do_pct, lag.max = 20)
mm_acf_mgl <- acf(mm_su$do_mgl, lag.max = 20)
mm_acf_temp <- acf(mm_su$temp, lag.max = 30)
mm_acf_sal <- acf(mm_su$sal, lag.max = 30)
mm_acf_depth <- acf(mm_su$depth, lag.max = 30)
mm_acf_turb <- acf(mm_su$turb,lag.max = 10)


#Marsh Creek Example:
mc_wi <- subset(mc, Month == 1 | Month == 2 | Month == 12)
mc_wi <- mc_wi[,c(-1,-2,-10,-11,-12,-13,-14,-15)]

mc_su <- subset(mc, Month == 6 | Month == 7 | Month == 8)
mc_su <- mc_su[mc_su$datetimestamp>='2008-06-16 00:00' & mc_su$datetimestamp<='2008-06-16 23:45',]
mc_su <- mc_su[,c(-1,-2,-10,-11,-12,-13,-14,-15)]


mc_acf_pct <- acf(mc_wi$do_pct, lag.max = 1000)
mc_acf_mgl <- acf(mc_wi$do_mgl, lag.max = 1900)
mc_acf_temp <- acf(mc_wi$temp, lag.max = 3200)
mc_acf_sal <- acf(mc_wi$sal, lag.max = 8000)
mc_acf_depth <- acf(mc_wi$depth, lag.max = 225)
mc_acf_turb <- acf(mc_wi$turb,lag.max = 5700)

mc_acf_pct <- acf(mc_su$do_pct, lag.max = 130)
mc_acf_mgl <- acf(mc_su$do_mgl, lag.max = 140)
mc_acf_temp <- acf(mc_su$temp, lag.max = 2700)
mc_acf_sal <- acf(mc_su$sal, lag.max = 7500)# block length longer than season length (?)
mc_acf_depth <- acf(mc_su$depth, lag.max = 25)
mc_acf_turb <- acf(mc_su$turb,lag.max = 4200)




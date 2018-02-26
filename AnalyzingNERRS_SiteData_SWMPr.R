install.packages("SWMPr")
install.packages("tidyverse")
install.packages("dplyr")

rm(list = ls())


library("SWMPr")
library("tidyverse")
library("dplyr")



#Get the data set of your desired site code
path <- "C:/Users/sabla/Documents/Research/SecondDownload_Current/SouthEast"
data_collected <- import_local(path, 'acebbwq')
wq_dat <- qaqc(data_collected)

#Enter the name of the sitecode again here (used for pulling meta data)
Sitecode <- rep("acebbwq",nrow(wq_dat))
Row <- rep(2,nrow(wq_dat))

wq_dat <- cbind(wq_dat, Sitecode)
wq_dat <- cbind(wq_dat, Row)

#Pull in meta data and attach it to the dataset
station_meta_data<-read.csv("C:/Users/sabla/Documents/Research/SecondDownload_Current/sampling_stations_edited.csv")
test <- merge(wq_dat,station_meta_data, by=c("Row"), all=TRUE)
View(test)


#Pull in meta data that I extrapolated from the meta docs and attach it to the dataset
extra_metadata<-read.csv("C:/Users/sabla/Documents/Research/Southeast_NERRS_WQ_EstuaryTypes_finished.csv")
test <- merge(test,extra_metadata, by="Sitecode.x", all=TRUE)
View(test)





southeast_station_metadata<-subset(station_meta_data,station_meta_data$NERR.Site.ID == 'ace',station_meta_data$NERR.Site.ID == 'gtm',station_meta_data$NERR.Site.ID == 'niw',station_meta_data$NERR.Site.ID == 'noc',station_meta_data$NERR.Site.ID == 'sap')
View(southeast_station_metadata)


#Learning SWMPr
#------------------------------------------------------------------------------------------------
# help.search(package = 'SWMPr', "organize")
# help.search(package = 'SWMPr', "retrieve")
# 
# swmp1 <- import_local("C:/Users/sabla/Documents/Research/Undergraduate_Research_Data/SWMPr_Data_For_R", "nocecwq", trace = FALSE)
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

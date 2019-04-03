#Stringing Together and Combining Tide Data


###############################################
# WARNING WARNING WARNING #
# This script is old and is a mix of different approaches used to join tidal data to the main data set.
# The successful apprach was done in TidalMarch_CycleAnalysis_Sam.r script
###############################################

















rm(list = ls())
# remove.packages(c("ggplot2", "plyr","dplyr", "reshape2", "lubridate", "SWMPr", "tidyverse"))
# install.packages('ggplot2', dependencies = TRUE)
# install.packages('plyr', dependencies = TRUE)
# install.packages('dplyr', dependencies = TRUE)
# install.packages('reshape2', dependencies = TRUE)
# install.packages('lubridate', dependencies = TRUE)
# install.packages('SWMPr', dependencies = TRUE)
# install.packages('tidyverse', dependencies = TRUE)
#install.packages("quantmod", dependencies = TRUE)

library("ggplot2")
library("plyr")
library("tidyverse")
library("dplyr")
library("SWMPr")
library("reshape2")
library("lubridate")
library("gridExtra")
library("data.table")
library("quantmod")

#setwd("C:/Users/sabla/Documents/Research/Tidal Data - NAVD - Daulphin Island")
setwd("/Users/samuelblackman/Desktop/Research/NERRS/Tidal Data/For SAP - Fernandina Beach, FL")

#hl_names <- c("CO-OPS__8735180__hl (1).csv", "CO-OPS__8735180__hl (2).csv", "CO-OPS__8735180__hl (3).csv", "CO-OPS__8735180__hl (4).csv", "CO-OPS__8735180__hl (5).csv", "CO-OPS__8735180__hl (6).csv", "CO-OPS__8735180__hl (7).csv", "CO-OPS__8735180__hl (8).csv", "CO-OPS__8735180__hl (9).csv", "CO-OPS__8735180__hl (10).csv")
#hl <- read.csv("CO-OPS__8735180__hl.csv")

hl_names <- c("CO-OPS__8720030__hl_2008_Fernandina_BeachFL.csv", "CO-OPS__8720030__hl_2009_Fernandina_BeachFL.csv", "CO-OPS__8720030__hl_2010_Fernandina_BeachFL.csv", "CO-OPS__8720030__hl_2011_Fernandina_BeachFL.csv", "CO-OPS__8720030__hl_2012_Fernandina_BeachFL.csv", "CO-OPS__8720030__hl_2013_Fernandina_BeachFL.csv", "CO-OPS__8720030__hl_2014_Fernandina_BeachFL.csv", "CO-OPS__8720030__hl_2015_Fernandina_BeachFL.csv", "CO-OPS__8720030__hl_2016_Fernandina_BeachFL.csv", "CO-OPS__8720030__hl_2017_Fernandina_BeachFL.csv")
hl <- read.csv("CO-OPS__8720030__hl_2007_Fernandina_BeachFL.csv")

for(i in 1:length(hl_names)){
  temp <- read.csv(hl_names[i])
  hl <- rbind(hl,temp)
}


# hr_names <- c("CO-OPS__8735180__hr (1).csv", "CO-OPS__8735180__hr (2).csv", "CO-OPS__8735180__hr (3).csv", "CO-OPS__8735180__hr (4).csv", "CO-OPS__8735180__hr (5).csv", "CO-OPS__8735180__hr (6).csv", "CO-OPS__8735180__hr (7).csv", "CO-OPS__8735180__hr (8).csv", "CO-OPS__8735180__hr (9).csv", "CO-OPS__8735180__hr (10).csv")
# hr <- read.csv("CO-OPS__8735180__hr.csv")
# 
# for(i in 1:length(hr_names)){
#   temp <- read.csv(hr_names[i])
#   hr <- rbind(hr,temp)
# }

drops <- c('I', 'L')
hl <- hl[,!(names(hl) %in% drops)]
#hr <- hr[,!(names(hr) %in% drops)]


#Joining to Reserve Data (Tide Tables)

path <- "/Users/samuelblackman/Desktop/Research/NERRS/SouthAtlantic"
sitename = 'sapdcwq'
data_collected <- import_local(path, sitename, trace = FALSE)
bh <- qaqc(data_collected)

bh_test <-bh
#Change Grand Bay into EST
#bh_test$datetimestamp <- force_tz(bh_test$datetimestamp, tzone ="EST")

site_analyzed<- bh_test[bh_test$datetimestamp>='2007-01-01 00:00' & bh_test$datetimestamp<='2017-12-31 23:45',]

# Date.Time <- strftime(site_analyzed$datetimestamp, format = "%Y-%m-%d %H", tz='EST')
# site_analyzed <- cbind(site_analyzed, Date.Time)

TidesAll <- site_analyzed[c(1:length(site_analyzed$datetimestamp)), c(1,7)]
s <- TidesAll
### Cycle Analysis of data subset
## Odd = Flood, Even = Ebb
## Check that data start with a full flood, verify that all peaks/troughs are captured by the smooth
# s<-TidesAll[1702:length(TidesAll[,1]),]	# subset of data to analyze (cleanest data)
#s<-TidesAll[3391:length(TidesAll[,1]),]	# subset of data to analyze (cleanest data)
b<-0.9347746
m<-10
f<-rep(1/(2*m+1),(2*m+1))
smoo<-stats::filter(s$depth,f,sides=2)
max<-(which(diff(sign(diff(smoo)))==-2)+1)
min<-(which(diff(sign(diff(smoo)))==2)+1)
pt<-sort(c(max,min))
Cycles<-rep(NA,length(s[,1]))
TideChange<-rep(NA,length(s[,1]))
Tide.cy<-rep(NA,length(s[,1]))
Tide.dS<-rep(NA,length(s[,1]))
s<-data.frame(s,TideChange,Cycles,Tide.cy,Tide.dS)
s$TideChange[cy]<-"X"
s$Cycles[1:pt[1]]<-1
z<-length(pt)
s<-s[1:pt[z],]
# s$Cycles[pt[z]:length(s$Cycles)]<-z+1
for (k in 2:length(pt)) {
  s$Cycles[pt[k-1]:pt[k]]<-k
}
s$Tide.cy[s$Cycles%%2==0]<-"Ebb"
s$Tide.cy[s$Cycles%%2!=0]<-"Flood"
for (i in (pt[1]+1):pt[length(pt)]) {
  if (s$Cycles[i]!=s$Cycles[i-1]) {
    k<-match(i,pt)
    s$Tide.dS[s$Cycles==s$Cycles[i-1]]<-s$Stage[i]-s$Stage[pt[k-1]]
  }
}
s$Tide.dS[1:pt[1]]<-s$Stage[pt[1]]-s$Stage[1]
s$Tide.dS[pt[z-1]:pt[z]]<-s$Stage[pt[z]]-s$Stage[pt[z-1]]
DateTime<-paste(format(as.POSIXct(strptime(s$Date,"%m/%d/%y",tz="")) ,format = "%m/%d/%y"),format(as.POSIXct(strptime(s$Time,"%H:%M",tz="")) ,format = "%H:%M"),sep=" ")
DateTime<-as.POSIXct(DateTime, format="%m/%d/%y %H:%M")
s<-data.frame(DateTime,s)
































#Sam's Manuel Determination of Tides


# x <- site_analyzed[c(1:length(site_analyzed$datetimestamp)), c(1,7)]
# x <- na.omit(x)
# x$Tide <- NA
# x$Block <- NA
# switch <- 0
# block_num <- 1
# 
# #must determine 14 manually (first max or min +1)
# for(i in 14:length(site_analyzed$datetimestamp)){
#   
#   if(switch == 0){
#     
#     x$Block[i] <- block_num
#   
#     if(x$depth[i+1]<x$depth[i]){
#       x$Tide[i] <- "F"
#       switch <- 1
#       block_num <- block_num+1
#     }
#   }
#   
#   if(switch == 1){
#     
#     x$Block[i] <- block_num
#     
#     if(x$depth[i+1]>x$depth[i]){
#       x$Tide[i] <- "E"
#       switch <- 0
#       block_num <-block_num+1
#     }
#   }
#   
#   
#   
# }
# 
# 
# #hl$Date.Time <- strftime(as.POSIXct(hl$Date.Time, format="%Y-%m-%d %H:%M", tz='EST'), format = "%Y-%m-%d %H", tz = 'EST')
# ##hl$Date.Time[143] <- "2007-03-11 02" #Daylight savings, spring forward
# #hl <- tibble::rowid_to_column(hl, "ID")
# 
# #analyze_tide <- join(site_analyzed, hl, type="left")
# #analyze_tide <- analyze_tide %>% fill(ID, .direction = "up")
# #analyze_tide <- analyze_tide %>% fill(TY, .direction = "up")
# 
# analyze_tide_2 <- analyze_tide %>% 
#   mutate(Tide = ifelse(TY == "L " | TY == "LL", "E", "F"))
#   #%>% mutate(Tide_2 = ifelse(TY == "L " | TY == "LL", "E", "F"))
#     
# #End Case GNDBH: THIS WILL HAVE TO CHANGE BASED ON THE NEW DATA SET LENGTH (MORE 2018 DATA)
# # analyze_tide_2$ID[385721]<- 8289
# # analyze_tide_2$ID[385722]<- 8289
# # analyze_tide_2$ID[385723]<- 8289
# # analyze_tide_2$ID[385724]<- 8289
# # analyze_tide_2$ID[385725]<- 8289
# # analyze_tide_2$ID[385726]<- 8289
# # analyze_tide_2$ID[385727]<- 8289
# # analyze_tide_2$ID[385728]<- 8289
# # 
# # analyze_tide_2$TY[385721]<- "LL"
# # analyze_tide_2$TY[385722]<- "LL"
# # analyze_tide_2$TY[385723]<- "LL"
# # analyze_tide_2$TY[385724]<- "LL"
# # analyze_tide_2$TY[385725]<- "LL"
# # analyze_tide_2$TY[385726]<- "LL"
# # analyze_tide_2$TY[385727]<- "LL"
# # analyze_tide_2$TY[385728]<- "LL"
# # 
# # analyze_tide_2$Tide[385721]<- "E"
# # analyze_tide_2$Tide[385722]<- "E"
# # analyze_tide_2$Tide[385723]<- "E"
# # analyze_tide_2$Tide[385724]<- "E"
# # analyze_tide_2$Tide[385725]<- "E"
# # analyze_tide_2$Tide[385726]<- "E"
# # analyze_tide_2$Tide[385727]<- "E"
# # analyze_tide_2$Tide[385728]<- "E"
# # 
# # analyze_tide_2$Tide_2[385721]<- "E"
# # analyze_tide_2$Tide_2[385722]<- "E"
# # analyze_tide_2$Tide_2[385723]<- "E"
# # analyze_tide_2$Tide_2[385724]<- "E"
# # analyze_tide_2$Tide_2[385725]<- "E"
# # analyze_tide_2$Tide_2[385726]<- "E"
# # analyze_tide_2$Tide_2[385727]<- "E"
# # analyze_tide_2$Tide_2[385728]<- "E"
# 
# #Creating Factor Column for CV individual analysis
# count <- 1
# for(i in 2:length(analyze_tide_2$TY)){
#   
#   if(analyze_tide_2$Tide[i-1]==analyze_tide_2$Tide[i]){
#     
#     analyze_tide_2$Tide[i-1] <- paste(count, analyze_tide_2$Tide[i-1], sep=" ")
#   }
#   
#   else{
#     
#     analyze_tide_2$Tide[i-1] <- paste(count, analyze_tide_2$Tide[i-1], sep=" ")
#     count <- count+1
#   } 
#   
# }
# #END CASE GNDBH
# analyze_tide_2$Tide[length(analyze_tide_2$Tide)] <- "8289 E"
# save_order <- analyze_tide_2$Tide
# 
# analyze_tide_2$Tide <- as.factor(analyze_tide_2$Tide)
# save_order <- unique(save_order)
# analyze_tide_2$Tide <- ordered(analyze_tide_2$Tide, levels <- save_order)
# 
# 
# #Preparing to remove Less than 90% DO and then calculating CV
# error_percent_T <- 1:nlevels(analyze_tide_2$Tide)
# 
# for(i in 1:nlevels(analyze_tide_2$Tide)){
#   
#   temp_tide <-save_order[i]
#   temp <- analyze_tide_2[analyze_tide_2$Tide==temp_tide,]
#   
#   error_percent_T[i] <- round((1-(sum(is.na(temp$do_mgl)))/nrow(temp))*100,2)
#   #print(paste(temp_tide, round((1-(sum(is.na(temp$do_mgl)))/nrow(temp))*100,2), sep=': '))
# }
# 
# to_add <- data.frame(8289,"2017-12-31 24",NA, as.factor("LL"))
# colnames(to_add) <- c("ID", "Date.Time", "Water.Level","TY")
# hl <-rbind(hl,to_add)
# 
# error_tide <- data.frame(hl, error_percent_T)
# error_tide <- error_tide %>% mutate(error_cleaned = ifelse(error_tide$error_percent_T<90, NA, error_tide$error_percent_T))
# 
# drops <- c("Date.Time","Water.Level","TY")
# error_to_add <- error_tide[,(!names(error_tide) %in% drops)]
# 
# #adding time scales(for comparison)
# Sitecode <- rep('gndbhwq',nrow(analyze_tide_2))
# analyze_tide_2 <- cbind(analyze_tide_2, Sitecode)
# 
# diel <- format(as.POSIXct(analyze_tide_2$datetimestamp, format="%Y-%m-%d H:M"), "%Y-%m-%d")
# analyze_tide_2 <-cbind(analyze_tide_2,diel)
# 
# monthly<- format(as.POSIXct(analyze_tide_2$datetimestamp,format="%Y-%m-%d H:M"), "%Y-%m")
# analyze_tide_2<-cbind(analyze_tide_2,monthly)
# 
# #analyze_tide_2 <- arrange(analyze_tide_2, analyze_tide_2$datetimestamp)
# 
# analyze_tide_2 <- analyze_tide_2 %>% 
#   mutate(Month = month(datetimestamp)) %>% 
#   mutate(Season = ifelse(Month == 1 | Month == 2, paste("Winter", year(datetimestamp), sep=" "),ifelse(Month == 12, paste("Winter", (year(datetimestamp)+1), sep=" "), ifelse(Month == 3 | Month == 4 | Month == 5, paste("Spring", year(datetimestamp), sep=" "),ifelse(Month == 6 | Month == 7 | Month == 8, paste("Summer", year(datetimestamp), sep=" "),ifelse(Month == 9 | Month == 10 | Month == 11, paste("Fall", year(datetimestamp), sep=" "),NA))))))
# 
# analyze_tide_2 <-  cbind(analyze_tide_2, do_mgl_T=rep(analyze_tide_2$do_mgl))
# analyze_tide_2 <-  cbind(analyze_tide_2, do_pct_T=rep(analyze_tide_2$do_pct))
# 
# #filling in the error percentages so that the below 90% can be used 
# analyze_tide_3 <- join(analyze_tide_2,error_to_add, by = "ID", type="left")
# 
# #adding na's to the below 90% data so that it is not factored into the cv
# for(i in 1:length(analyze_tide_3$do_mgl)){
#   ifelse(is.na(analyze_tide_3$error_cleaned[i]), analyze_tide_3$do_mgl_T[i]<-NA, analyze_tide_3$do_mgl[i])
#   ifelse(is.na(analyze_tide_3$error_cleaned[i]), analyze_tide_3$do_pct_T[i]<-NA, analyze_tide_3$do_pct[i])
# }
# 
# #cv calculated
# bh_calculations_tide <- analyze_tide_3 %>% group_by(Tide) %>% summarise(tide_STD = sd(do_mgl_T, na.rm=TRUE), tide_AVG = mean(do_mgl_T, na.rm=TRUE)) %>% mutate(tide_CV=(tide_STD/tide_AVG))
# bh_calculations_tide <- tibble::rowid_to_column(bh_calculations_tide, "ID")
# analyze_tide_3 <- merge(analyze_tide_3, bh_calculations_tide, by="ID")
# 
# #Plots----------------------------------------------------------------------------------------
# to_plot <- analyze_tide_3[analyze_tide_3$datetimestamp>='2007-01-01 00:00' & analyze_tide_3$datetimestamp<='2017-12-31 23:45',]
# #to_plot <- arrange(to_plot, to_plot$datetimestamp)
# 
# #ggplot(to_plot, aes(datetimestamp, do_mgl)) +
#  # geom_point()
# 
# #Develop Plot
# p1 <- ggplot(to_plot, aes(x=Tide_2, y=do_pct_T)) +
#   geom_boxplot() +
#   #theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
#   #facet_grid(.~Month) +
#   xlab("Flood/Ebb Tide") +
#   ylab("DO_pct") +
#   ggtitle("gndbhwq DO pct 2007-2017")
# p1
# #---------------------------------------------------------------------------------------------
# 
# 
# 
# 
# 















# path <- "C:\\Users\\sabla\\Documents\\Research\\SecondDownload_old\\Southeast"
# sitename = 'sapdcwq'
# data_collected <- import_local(path, sitename, trace = FALSE)
# dc <- qaqc(data_collected)
# 
# to_plot <- dc[dc$datetimestamp >='2007-01-01 00:00' & dc$datetimestamp <= '2007-12-31 23:45',]
# 
# p1 <- ggplot(to_plot, aes(datetimestamp, do_mgl)) +
#   geom_point() +
#   theme(axis.text.x = element_text(angle = 60, hjust = 1)) #+
# #facet_grid(Season~.)
# p1

# #Filling out the High/Low Tide so that Ebb/Flood can be associated with it
# saved_1 <- "LL"
# saved_2 <- "HH"
# 
# for(i in 1:length(analyze_tide$TY)){
#   
#   if(is.na(analyze_tide$TY[i])){
#     
#     analyze_tide$TY[i] <- saved_1
#   }
#   
#   else if(!is.na(analyze_tide$TY[i]) && is.na(analyze_tide$TY[i+1])){
#     
#     if(analyze_tide$TY[i]=="LL"){
#       saved_1 <- "HH"
#     }
#     else if(analyze_tide$TY[i]=="HH"){
#       saved_1 <- "LL"
#     }
#     else if(analyze_tide$TY[i]=="L "){
#       saved_1 <- "H "
#     }
#     else if(analyze_tide$TY[i]=="H "){
#       saved_1 <- "L "
#     }
#   }
#   
# }
#end case condition
#analyze_tide_2$Tide[length(analyze_tide_2$Tide)] <- "8289 E"
#analyze_tide_2$Tide <- as.factor(analyze_tide_2$Tide)
# #Add the flood/ebb to the hl so that error percentage may be calculated
# hl <- hl %>%
#   mutate(Tide = ifelse(TY =="L " | TY == "LL", "E","F"))
# 
# count <- 1
# 
# for(i in 1:length(hl$Date.Time)){
#   
#   hl$Tide[i] <- paste(count, hl$Tide[i], sep=" ")
#   count <- count+1
#   
# }
# 
# #remove extra columns
# drops <- c("Date.Time","Water.Level","TY")
# analyze_tide_2 <- analyze_tide_2[,(!names(analyze_tide_2) %in% drops)]
#colnames(error_tide) <- c("Date.Time","Water.Level","TY","error_percent_T","error_cleaned_T")
#site_analyzed_clean<- bh_test[bh_test$datetimestamp>='2007-01-01 00:00' & bh_test$datetimestamp<='2017-12-31 23:45',]

# #Reorder the Tide Factors
#  o <- as.character(to_plot$Tide)
#  o <- unique(o)
#  to_plot$Tide <- ordered(to_plot$Tide, levels <- o)
#  s <- as.character(to_plot$Season)
#  s <- unique(s)
#  to_plot$Season <- ordered(to_plot$Season, levels <-s)

#p <- to_plot[to_plot$monthly=="2007-01",]

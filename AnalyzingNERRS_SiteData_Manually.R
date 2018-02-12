#Sam Blackman
#Undergraduate Research under Dr. Nelson
#10/13/17
#Looking at SWMPr North Carolina Water Quality Data downloaded from NERRS Website (10/13/17)
#----------------------------------------------------------------------------------------------------------------------------------------
#Setting up Script
#----------------------------------------------------------------------------------------------------------------------------------------

#Clears Workspace
rm(list = ls())


setwd(
  "C:/Users/sabla/Documents/Research/FirstDownload_old/Undergraduate_Research_Data/SWMPr_NorthCarolinaWQOnly"
)
install.packages("ggpubr")


library(SWMPr)
library(ggplot2)
#library(ggpubr)
#library(ggmap)
library(plyr)
library(dplyr)

#------------------------------------------------------------------------------------------------------------------------------------------
#Functions
#------------------------------------------------------------------------------------------------------------------------------------------


readInFiles <- function(file_list) {
  #Reads in indivdual csv's into an all encomapssing dataframe for a single site
  
  site_data <- data.frame()
  
  #Build Line Break in dataframe according to number of columns (varies between sites ever so slightly)
  #Likely need to make each NA their own column, but also put them under thae same column field as in
  #the imported data, so that it is bound to the correct row
  row_break <- data.frame(cbind(rep(1:10, each = NA)))
  
  
  #concatenates yearly data into one dataframe
  for (i in 1:length(file_list)) {
    temp <- read.csv(file_list[i])
    site_data <- rbind.fill(site_data, temp)
    site_data <- rbind.fill(site_data, row_break)
    
    print("Compiled")
    print(i)
  }
  
  return(site_data)
}

#Gets all water quality spreadsheets given you enter its site name (multiple sites can be selected at once)
getFiles <- function() {
  setwd("C:/Users/sabla/Documents/Research/FirstDownload_old/Undergraduate_Research_Data/SWMPr_Data_For_R")
  
  data <- list.files(path = ".", pattern = ".csv")
  filtered <- grep("wq", data)
  new_data <- data[filtered]
  
  filtered <- grep("2",new_data)#Gets rid of unreliable data from the 90s
  new_data_2 <- new_data[filtered]
  
  to_remove_1 <- grep("2000", new_data_2)#Gets rid of data from the year 2000
  new_data_2 <- setdiff(new_data_2, to_remove_1)
  
  to_remove_2 <- grep("2001", new_data_2)#Gets rid of data from the year 2001
  new_data_2 <- setdiff(new_data_2, to_remove_2)
  
  
  #writeLines("Here is a list of all available water quality site data: \r\n Apalachicola, Florida (apa) \n Ashepoo Combahee Edisto Basin, SC (ace) \n Chesapeake Bay, MD (cbm) \n Chesapeake Bay, VA (cbv) \n Delaware, DE (del) \n Elkhorn Slough, CA (elk) \n Grand Bay, MS (gnd) \n Great Bay, NH (grb) \n Guana Tolomato Mantanzas, FL (gtm) \n Hudson River, NY (hud) \n Jacques Cousteau, NJ (jac) \n Jobos Bay, PR (job) \n Kachemak Bay, AK (kac) \n Lake Superior, WI (lks) \n Mission Aransas, TX (mar) \n Narragansett Bay, RI (nar) \n North Carolina, NC (noc) \n North Inlet-Winyah Bay, SC (niw) \n Old Woman Creek, OH (owc) \n Padilla Bay, WA (pdb) \n Rookery Bay, FL (rkb) \n San Francisco Bay, CA (sfb) \n Sapelo Island, GA (sap) \n South Slough, OR (sos) \n Tijuana River, CA (tjr) \n Waquoit Bay, MA (wqb) \n Weeks Bay, AL (wkb) \n Wells, ME (wel)")
  #response <- readline(prompt = "Enter the site code of the location you would like to examine (separate codes with a space):  ")
  
  response = ("nocecwq")
  
  site_codes <- strsplit(response, " ")
  selected_data <- character(length = 0);

  
  for(i in 1:length(site_codes[[1]])){
    
    filtered <- which(grepl(site_codes[[1]][i], new_data_2))
    selected_data <- c(selected_data, new_data_2[filtered])
    
  }
  
  selected_data_df <- readInFiles(selected_data)
  
  toReturn <- list("Data" = selected_data_df, "Sites" = site_codes)
  return(toReturn)
}


#------------------------------------------------------------------------------------------------------------------------------------------
#Script
#------------------------------------------------------------------------------------------------------------------------------------------

#Cleaning the Data ADD EXTRA CLEANING FOR POSITIVE QAQC VALUES!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
info <- getFiles()

station_data <- info$Data
numSiteCodes <- info$Sites

str(station_data)
str(numSiteCodes)
View(station_data)

swmp1 <-station_data
swmp1 <-mutate(swmp1, DateTimeStamp = as.character(swmp1$DateTimeStamp))
swmp1$DateTimeStamp <- as.Date(swmp1$DateTimeStamp, format = '%m/%d/%Y %H:%M')
swmp1$DateTimeStamp <- as.Date.POSIXct(swmp1$DateTimeStamp, format = '%Y/%m/%d %H:%M')
View(swmp1)

swmp2 <-swmpr(swmp1, 'nocecwq')
View(swmp2)

aggreswmp(swmp2, 'quarters',params = c('DO_mgl'),plot=T)








dat <- subset(swmp1, select = 'DO_mgl', subset = c('2013-07-01 00:00', '2013-07-31 00:00'))

plot(dat)
plot(do_mgl ~ datetimestamp, dat)
plot(dat, type = 'n')
lines(dat, col = 'red')



drops <- c("SpCond", "F_SpCond", "X", "cbind.rep.1.10..each...NA..")
station_data <- station_data[, !names(station_data) %in% drops]

#---------------------------------------------------------Eliminating Negative QAQC Values--------------------------------------------------

data <- station_data

#Get's the headers of the data frame and then selects the headers that contain QAQC information
header_names <- colnames(data)
get_QAQC_headers <- grep('F_', header_names, fixed = TRUE)

data[get_QAQC_headers] <-
  lapply(data[get_QAQC_headers],  as.character)

for (k in 1:length(get_QAQC_headers)) {
  #Gets a list of indices for where there are values that do not meet QAQC standards (anything below zero)
  indices_1 <-
    grep('<-1>', data[, get_QAQC_headers[k]], fixed = TRUE)
  indices_2 <-
    grep('<-2>', data[, get_QAQC_headers[k]], fixed = TRUE)
  indices_3 <-
    grep('<-3>', data[, get_QAQC_headers[k]], fixed = TRUE)
  indices_4 <-
    grep('<-4>', data[, get_QAQC_headers[k]], fixed = TRUE)
  indices_5 <-
    grep('<-5>', data[, get_QAQC_headers[k]], fixed = TRUE)
  
  #Places these vectors into a list
  indices <-
    list(
      neg_one = indices_1,
      neg_two = indices_2,
      neg_three = indices_3,
      neg_four = indices_4,
      neg_five = indices_5
    )
  
  #Searches through the each data frame column an eliminates the invalid data and replaces it was NAs
  for (i in 1:length(indices)) {
    if (length(indices[i]) > 0) {
      data[indices[[i]], get_QAQC_headers[k] - 1] = NA
      
    }
  }
  
  print("Column Cleaned")
  
  station_data <- data
}





#-----------------------------------------------------------------------------------------------------------------------------------------
#Plots
#-----------------------------------------------------------------------------------------------------------------------------------------

testable_data <- station_data

for(i in 1:length(numSiteCodes[[1]])){
  
  currentSite <- paste(numSiteCodes[[1]][i], "   ", sep="")
  
single_site <-
  subset(testable_data, testable_data$StationCode == currentSite)

single_site <-
  mutate(single_site,
         DateTimeStamp = as.Date(single_site$DateTimeStamp, format = '%m/%d/%Y %H:%M'))

setwd("C:/Users/sabla/Documents/Research/Undergraduate_Research_Data/FastPlots")

#DO Values
#pdf(file="1do_nocecwq.pdf", width=8.5,height=11)

toSave <- paste(numSiteCodes[[1]][i], "_do.jpg", sep="")

jpeg(file=toSave, width = 600, height = 700, units = "px")
do_plot <- ggplot(single_site, aes(x = single_site[[3]], y = single_site[[13]])) + 
  geom_point() +
  #scale_color_gradient(low="cyan", high = "darkblue", guide = "colourbar") +
  scale_x_date(name = "Time (minutes, separate by year)", date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(name = "Dissolved Oxygen (mgL)", limits = c(0,25), breaks = c(0,5,10,15,20,25)) +
  ggtitle("Dissolved Oxygen Levels Versus Time") +
  labs(colour = "DO (mgL)")

print(do_plot)
dev.off()

#Temp Values
toSave <- paste(numSiteCodes[[1]][i], "_temp.jpg", sep="")

jpeg(file=toSave, width = 600, height = 700, units = "px")
temp_plot <- ggplot(single_site, aes(x = single_site[[3]], y = single_site[[7]])) + 
  geom_point() +
  #scale_color_gradient(low="cyan", high = "darkblue", guide = "colourbar") +
  scale_x_date(name = "Time (minutes, separate by year)", date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(name = "Temperature (C)", limits = c(0,35), breaks = c(0,5,10,15,20,25,30,35)) +
  ggtitle("Temperature Levels Versus Time") +
  labs(colour = "Temp (C)")

print(temp_plot)
dev.off()

#Salinity Values
toSave <- paste(numSiteCodes[[1]][i], "_sal.jpg", sep="")

jpeg(file=toSave, width = 600, height = 700, units = "px")
sal_plot <- ggplot(single_site, aes(x = single_site[[3]], y = single_site[[9]])) + 
  geom_point() +
  #scale_color_gradient(low="cyan", high = "darkblue", guide = "colourbar") +
  scale_x_date(name = "Time (minutes, separate by year)", date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(name = "Salinity (psu)", limits = c(0,60), breaks = c(0,5,10,15,20,25,30,35,40,45,50,55,60)) +
  ggtitle("Salinity Levels Versus Time") +
  labs(colour = "Sal (psu)")

print(sal_plot)
dev.off()

#pH Values
toSave <- paste(numSiteCodes[[1]][i], "_ph.jpg", sep="")

jpeg(file=toSave, width = 600, height = 700, units = "px")
ph_plot <- ggplot(single_site, aes(x = single_site[[3]], y = single_site[[23]])) + 
  geom_point() +
  #scale_color_gradient(low="cyan", high = "darkblue", guide = "colourbar") +
  scale_x_date(name = "Time (minutes, separate by year)", date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(name = "pH", limits = c(5,10), breaks = c(5,6,7,8,9,10)) +
  ggtitle("pH Levels Versus Time") +
  labs(colour = "pH")

print(ph_plot)
dev.off()

#Turbidity Values
toSave <- paste(numSiteCodes[[1]][i], "_tb.jpg", sep="")

jpeg(file=toSave, width = 600, height = 700, units = "px")
tb_plot <- ggplot(single_site, aes(x = single_site[[3]], y = single_site[[25]])) + 
  geom_point() +
  #scale_color_gradient(low="cyan", high = "darkblue", guide = "colourbar") +
  scale_x_date(name = "Time (minutes, separate by year)", date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(name = "Turbidity (FNU/NTU)", limits = c(-7,3800)) +
  ggtitle("Turbidity Levels Versus Time") +
  labs(colour = "Turbidity (FNU/NTU)")

print(tb_plot)
dev.off()

#Depth Values
toSave <- paste(numSiteCodes[[1]][i], "_dep.jpg", sep="")

jpeg(file=toSave, width = 600, height = 700, units = "px")
dep_plot <- ggplot(single_site, aes(x = single_site[[3]], y = single_site[[15]])) + 
  geom_point() +
  #scale_color_gradient(low="cyan", high = "darkblue", guide = "colourbar") +
  scale_x_date(name = "Time (minutes, separate by year)", date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(name = "Depth (m)", limits = c(-.5,3)) +
  ggtitle("Depth Levels Versus Time") +
  labs(colour = "Depth (m)")

print(dep_plot)
dev.off()

#print(ggarrange(do_plot, temp_plot, sal_plot, ph_plot, labels = c("A", "B", "C", "D"), ncol = 1, nrow = 4))
#dev.off()
}

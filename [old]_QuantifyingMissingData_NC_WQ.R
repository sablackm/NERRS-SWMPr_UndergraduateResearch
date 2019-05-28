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
  "C:/Users/sabla/Documents/Undergraduate_Research_Data/SWMPr_NorthCarolinaWQOnly"
)
install.packages("ggpubr")


library(SWMPr)
library(ggplot2)
library(ggpubr)
library(ggmap)
library(dplyr)
library(plyr)

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
  setwd("C:/Users/sabla/Documents/Undergraduate_Research_Data/SWMPr_Data_For_R")
  
  data <- list.files(path = ".", pattern = ".csv")
  filtered <- which(grepl("wq", data))
  new_data <- data[filtered]
  
  writeLines(
    "Here is a list of all available water quality site data: \r\n Apalachicola, Florida (apa) \n Ashepoo Combahee Edisto Basin, SC (ace) \n Chesapeake Bay, MD (cbm) \n Chesapeake Bay, VA (cbv) \n Delaware, DE (del) \n Elkhorn Slough, CA (elk) \n Grand Bay, MS (gnd) \n Great Bay, NH (grb) \n Guana Tolomato Mantanzas, FL (gtm) \n Hudson River, NY (hud) \n Jacques Cousteau, NJ (jac) \n Jobos Bay, PR (job) \n Kachemak Bay, AK (kac) \n Lake Superior, WI (lks) \n Mission Aransas, TX (mar) \n Narragansett Bay, RI (nar) \n North Carolina, NC (noc) \n North Inlet-Winyah Bay, SC (niw) \n Old Woman Creek, OH (owc) \n Padilla Bay, WA (pdb) \n Rookery Bay, FL (rkb) \n San Francisco Bay, CA (sfb) \n Sapelo Island, GA (sap) \n South Slough, OR (sos) \n Tijuana River, CA (tjr) \n Waquoit Bay, MA (wqb) \n Weeks Bay, AL (wkb) \n Wells, ME (wel)"
  )
  response <-
    readline(prompt = "Enter the site code of the location you would like to examine (separate codes with a space):  ")
  
  site_codes <- strsplit(response, " ")
  selected_data <- character(length = 0)
  
  
  for (i in 1:length(site_codes[[1]])) {
    filtered <- which(grepl(site_codes[[1]][i], new_data))
    selected_data <- c(selected_data, new_data[filtered])
    
  }
  
  selected_data_df <- readInFiles(selected_data)
  
  return(selected_data_df)
}

#------------------------------------------------------------------------------------------------------------------------------------------
#Script
#------------------------------------------------------------------------------------------------------------------------------------------

#Cleaning the Data
station_data <- getFiles()

str(station_data)
View(station_data)

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
single_site <-
  subset(testable_data, testable_data$StationCode == "noclcwq   ")

#str(single_site)

# str(testable_data$cec[3])
# str(testable_data$cec[13])

#print(single_site[[3]])

#single_site$DateTimeStamp <- lapply(single_site$DateTimeStamp, as.POSIXlt(single_site$DateTimeStamp, format ='%m/%d/%Y %H:%M'))

single_site <-
  mutate(single_site,
         DateTimeStamp = as.Date(single_site$DateTimeStamp, format = '%m/%d/%Y %H:%M'))

# View(station_data$cec[3])
# View(testable_data$cec[3])
#
# plot_dates <- mutate(testable_data$cec[3], DateTimeStamp = as.POSIXlt(DateTimeStamp, format ='%m/%d/%Y %H:%M'))
# plot_DO <- mutate(testable_data$cec[13], DateTimeStamp = as.numeric(DO_mgl))

#DO Values
do_plot <- ggplot(single_site, aes(x = single_site[[3]], y = single_site[[13]], color = single_site[[13]])) + 
  geom_point() +
  scale_color_gradient(low="cyan", high = "darkblue", guide = "colourbar") +
  scale_x_date(name = "Time (minutes, separate by year)", date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(name = "Dissolved Oxygen (mgL)", limits = c(0,18), breaks = c(0,1,2,3,4,5,6,7,8,9,10,11,12,13,14,15,16,17,18,19,20)) +
  ggtitle("Dissolved Oxygen Levels Versus Time at Loosin Creek, NC") +
  labs(colour = "DO (mgL)")

#Temp Values
temp_plot <- ggplot(single_site, aes(x = single_site[[3]], y = single_site[[7]], color = single_site[[7]])) + 
  geom_point() +
  scale_color_gradient(low="cyan", high = "darkblue", guide = "colourbar") +
  scale_x_date(name = "Time (minutes, separate by year)", date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(name = "Temperature (C)", limits = c(0,35), breaks = c(0,5,10,15,20,25,30,35)) +
  ggtitle("Temperature Levels Versus Time at Loosin Creek, NC") +
  labs(colour = "Temp (C)")

#Salinity Values
sal_plot <- ggplot(single_site, aes(x = single_site[[3]], y = single_site[[9]], color = single_site[[9]])) + 
  geom_point() +
  scale_color_gradient(low="cyan", high = "darkblue", guide = "colourbar") +
  scale_x_date(name = "Time (minutes, separate by year)", date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(name = "Salinity (psu)", limits = c(0,40), breaks = c(0,5,10,15,20,25,30,35,40)) +
  ggtitle("Salinity Levels Versus Time at Loosin Creek, NC") +
  labs(colour = "Sal (psu)")

#pH Values
ph_plot <- ggplot(single_site, aes(x = single_site[[3]], y = single_site[[23]], color = single_site[[23]])) + 
  geom_point() +
  scale_color_gradient(low="cyan", high = "darkblue", guide = "colourbar") +
  scale_x_date(name = "Time (minutes, separate by year)", date_labels = "%Y", date_breaks = "1 year") +
  scale_y_continuous(name = "pH", limits = c(6,10), breaks = c(6,7,8,9,10)) +
  ggtitle("pH Levels Versus Time at Loosin Creek, NC") +
  labs(colour = "pH")

ggarrange(do_plot, temp_plot, sal_plot, ph_plot, labels = c("A", "B", "C"), ncol = 2, nrow = 2)

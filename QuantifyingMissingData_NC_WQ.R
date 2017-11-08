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


library(SWMPr)
library(ggplot2)
library(dplyr)
library(plyr)

#------------------------------------------------------------------------------------------------------------------------------------------
#Functions
#------------------------------------------------------------------------------------------------------------------------------------------

getFilesManual <- function() {
  #Allows for manual entering of which data sets you would like to clean up/analyze
  
  numFiles <-
    readline(prompt = "Enter the number of data sheets you are uploading: ")
  name_of_files <- vector(mode = "character", length = numFiles)
  
  all_data <- list(length = numFiles)
  
  
  for (i in 1:numFiles) {
    name_of_files[i] <-
      readline(prompt = "Enter Filename of csv (with .csv):  ")
    all_data[[i]] <- read.csv(name_of_files[i])
  }
  
  return(all_data)
}


getFilesAutomatic <- function() {
  #Loads in All csv files from the SWMPr_NorthCarlinaWQOnly folder in my Working Directory
  
  
  #nocecwq site
  data_nocecwq <-
    readInFiles(list.files(path = ".", pattern = "nocecwq"))
  
  #noclcwq site
  data_noclcwq <-
    readInFiles(list.files(path = ".", pattern = "noclcwq"))
  
  #nocrcwq site
  data_nocrcwq <-
    readInFiles(list.files(path = ".", pattern = "nocrcwq"))
  
  #noczbwq site
  data_noczbwq <-
    readInFiles(list.files(path = ".", pattern = "noczbwq"))
  
  str(data_nocecwq)
  
  all_sites <-
    list(
      cec = data_nocecwq,
      clc = data_noclcwq,
      crc = data_nocrcwq,
      czb = data_noczbwq
    )
  return(all_sites)
}

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

#------------------------------------------------------------------------------------------------------------------------------------------
#Script
#------------------------------------------------------------------------------------------------------------------------------------------

#Enter a single dataset to be analyzed
#name_of_file <-
#   readline(prompt = "Enter Filename of csv (with .csv): ")
#
# station_data <- read.csv(name_of_file)
# #head(station_data)

#Cleaning the Data
station_data <- getFilesAutomatic()

str(station_data)

#Removes SpCond and F_SpCond
station_data$cec[9] = NULL
station_data$cec[9] = NULL
station_data$clc[9] = NULL
station_data$clc[9] = NULL
station_data$crc[9] = NULL
station_data$crc[9] = NULL
station_data$czb[9] = NULL
station_data$czb[9] = NULL

#Removes Last 2 columns (used to add NAs between dataframes that are no longer necessary)
station_data$cec[29] = NULL
station_data$cec[29] = NULL
station_data$clc[29] = NULL
station_data$clc[29] = NULL
station_data$crc[29] = NULL
station_data$crc[29] = NULL
station_data$czb[29] = NULL
station_data$czb[29] = NULL

#---------------------------------------------------------Eliminating Negative QAQC Values--------------------------------------------------


#Finding an replacing negative QAQC values
for (j in 1:length(station_data)) {
  
  data <- station_data[[j]]
  
  #Get's the headers of the data frame and then selects the headers that contain QAQC information
  header_names <- colnames(data)
  get_QAQC_headers <- which(grepl('F_', header_names, fixed = TRUE))
  
  
  for (k in 1:length(get_QAQC_headers)) {
    #Gets a list of indices for where there are values that do not meet QAQC standards (anything below zero)
    indices_1 <-
      which(grepl('<-1>', data[[get_QAQC_headers[k]]], fixed = TRUE))
    indices_2 <-
      which(grepl('<-2>', data[[get_QAQC_headers[k]]], fixed = TRUE))
    indices_3 <-
      which(grepl('<-3>', data[[get_QAQC_headers[k]]], fixed = TRUE))
    indices_4 <-
      which(grepl('<-4>', data[[get_QAQC_headers[k]]], fixed = TRUE))
    indices_5 <-
      which(grepl('<-5>', data[[get_QAQC_headers[k]]], fixed = TRUE))
    
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
    
    station_data[[j]] <- data
  }
}
View(station_data$cec)







#-----------------------------------------------------------------------------------------------------------------------------------------
#Plots
#-----------------------------------------------------------------------------------------------------------------------------------------

testable_data <- station_data
single_site <- testable_data$cec

str(single_site)

# str(testable_data$cec[3])
# str(testable_data$cec[13])

print(single_site[[3]])

single_site$DateTimeStamp <- mutate(single_site$DateTimeStamp, DateTimeStamp = as.POSIXlt(DateTimeStamp, format ='%m/%d/%Y %H:%M'))

# View(station_data$cec[3])
# View(testable_data$cec[3])
# 
# plot_dates <- mutate(testable_data$cec[3], DateTimeStamp = as.POSIXlt(DateTimeStamp, format ='%m/%d/%Y %H:%M'))
# plot_DO <- mutate(testable_data$cec[13], DateTimeStamp = as.numeric(DO_mgl))


ggplot(single_site,aes(x = single_site[[3]], y = single_site[[13]])) + geom_point()


nrow(testable_data$cec[3])
nrow(testable_data$cec[13])








# for(i in 1:numYears){
#
#   #Removes SpCond and F_SpCond
#   station_data[i]$length[9] <- NULL
#   station_data[i]$length[9] <- NULL
#
# #How to elminate Factors
#  #station_data <-
#   #mutate(station_data[i]$length, F_DO_mgl = as.character(F_DO_mgl))
#
# }
#
# #How to replace items with NAs
# # for (i in 1:nrow(station_data)) {
# #   if (identical(station_data$F_DO_mgl[i], "<-2> ")) {
# #     station_data$F_DO_mgl[i] <- NA
# #
# #   }
# # }
#
# head(station_data)
#
# #Works for replacing NAs in A single subset
# # DO_quality <- subset(station_data, select = 'F_DO_mgl')
# # head(DO_quality)
# #
# # DO_quality <- mutate(DO_quality, F_DO_mgl = as.character(F_DO_mgl))
# # str(DO_quality)
# # head(DO_quality)
# #
# #
# # for(i in 1:nrow(DO_quality)){
# #
# #   if(identical(DO_quality$F_DO_mgl[i], "<-2> ")){
# #     DO_quality$F_DO_mgl[i] <- NA;
# #     countNAs <- countNAs + 1;
# #   }
# # }
# #
# # head(DO_quality)

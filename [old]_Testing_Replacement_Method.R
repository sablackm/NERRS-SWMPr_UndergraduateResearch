



#Testing replacement methods for QAQC values with NAs

setwd("C:/Users/sabla/Documents/Undergraduate_Research_Data/Testing")


data <- read.csv("acemcnut2014.csv")
str(data)




#Get's the headers of the data frame and then selects the headers that contain QAQC information
header_names <- colnames(data)
get_QAQC_headers <- which(grepl('F_', header_names, fixed = TRUE))

#Finding an replacing negative QAQC values
for (k in 1:length(get_QAQC_headers)) {
  
  #Gets a list of indices for where there are values that do not meet QAQC standards (anything below zero)
  indices_1 <- which(grepl('<-1>', data[[get_QAQC_headers[k]]], fixed = TRUE))
  indices_2 <- which(grepl('<-2>', data[[get_QAQC_headers[k]]], fixed = TRUE))
  indices_3 <- which(grepl('<-3>', data[[get_QAQC_headers[k]]], fixed = TRUE))
  indices_4 <- which(grepl('<-4>', data[[get_QAQC_headers[k]]], fixed = TRUE))
  indices_5 <- which(grepl('<-5>', data[[get_QAQC_headers[k]]], fixed = TRUE))
  
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
}

View(data)

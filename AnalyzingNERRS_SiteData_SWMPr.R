install.packages("SWMPr")
library("SWMPr")

setwd(
  "C:/Users/sabla/Documents/Research/Undergraduate_Research_Data/SWMPr_NorthCarolinaWQOnly"
)
data(nocecwq)
swmp1 <-apadbwq

help.search(package = 'SWMPr', "organize")
help.search(package = 'SWMPr', "retrieve")

swmp1 <- import_local("C:/Users/sabla/Documents/Research/Undergraduate_Research_Data/SWMPr_Data_For_R", "nocecwq", trace = FALSE)
cleaned_swmp1 <- qaqc(swmp1)




dat <- subset(cleaned_swmp1, select = 'do_mgl', subset = c('2013-07-01 00:00', '2013-07-31 00:00'))

plot(dat)
plot(do_mgl ~ datetimestamp, dat)
plot(dat, type = 'n')
lines(dat, col = 'red')

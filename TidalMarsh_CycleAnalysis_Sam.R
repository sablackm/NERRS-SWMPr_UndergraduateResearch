rm(list=ls(all=TRUE))
library(ggplot2)
library(gridExtra)
library(SWMPr)
library(tidyverse)
library(naniar)

#######################
##### Load Data #######
#######################

path <- "/Users/samuelblackman/Desktop/Research/NERRS/SouthAtlantic"
sitename = 'sapdcwq'
data_collected <- import_local(path, sitename, trace = FALSE)
bh <- qaqc(data_collected)

TidesAll <- bh
#-------------------------------------------------------------------------------------------------------------------------------------------------------------

#Only Max - WORKS!
#Will not have to worry about lower bound, if using LoadingData.R
s2 <- TidesAll[c(3114:385728), c(1,7)]
s2 <- na.omit(s2)

m<-10
f<-rep(1/(2*m+1),(2*m+1))
smoo2<-stats::filter(s2$depth,f,sides=2)

sign_diff <- sign(diff(smoo2))
sdf <- data.frame(sign_diff[1:length(sign_diff)])
sdf[sdf == 0] <- NA
sdf <- na.locf(sdf$sign_diff.1.length.sign_diff.., na.rm = FALSE)

#sign_diff2 <- diff(sign(diff(smoo2)))
sign_diff2 <- diff(sdf)
max <- which(sign_diff2==-2)+1

pt<-sort(max)
Cycles<-rep(NA,length(s2[,1]))
s2<-data.frame(s2[1:length(sign_diff2),],Cycles[1:length(sign_diff2)], sign_diff[1:length(sign_diff2)], sign_diff2)
s2$Cycles[1:pt[1]]<-1

for (k in 2:length(pt)) {
  s2$Cycles[pt[k-1]:pt[k]]<-k
}

#Clean up End of Dataset:
toRemove <- c()
for (i in (length(s2$Cycles)-100):length(s2$Cycles)) {
  if(is.na(s2$Cycles[i])){
    toRemove <- c(toRemove, i)
  }
}

s2 <- s2[-toRemove,]


#Viewing Depths Versus Filtered Depths

# plot(smoo2, type= 'l')
# lines(s2$depth, col = 'green')
# plot(s2$depth, type = 'l', col = 'green')
# lines(smoo2)



# y2017 <- TidesAll[TidesAll$datetimestamp>='2017-01-01 00:00:00' & TidesAll$datetimestamp<='2017-12-31 23:45:00',]
# y2017 <- y2017[c(1:length(y2017$datetimestamp)), c(1,7)]
# y2017 <- na.omit(y2017)



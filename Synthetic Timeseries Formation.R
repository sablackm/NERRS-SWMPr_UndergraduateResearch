## Forming synthetic timeseries from the data which has been blocked by tidal cycle.
## Based on Bryans's bootstrap code
##
## MUST RUN TIDALMARSH_CYCLEANALYSIS_SAM.R BEFORE RUNNING THIS SCRIPT
library(dplyr)
library(quantreg)

#Rejoin necessary columns used in analysis (e.g. temp, sal, turb, do_pct, do_mgl, unsmoothed depth)
setwd("/Users/samuelblackman/Desktop/Research/NERRS/CurrentData")
toJoin <- read.csv("gndbhwq data.csv")
toJoin$X <- NULL
toJoin <- toJoin[c(3114:385728),]
toJoin$datetimestamp <- as.POSIXct(toJoin$datetimestamp, format = "%Y-%m-%d %H:%M:%OS")

#s2 is from TidalMarsh_CycleAnalysis_Sam.R
site <- left_join(x = s2, y = toJoin, by="datetimestamp")

#Seeting up Synthetic timeseries creation
n    <- length(site)  # Sample size
rho  <- 0.90  # Autocorrelation
tau  <- 0.95  # Quantile level
beta <- 1     # True slope
N    <- 2  # Number of bootstrap samples

#Compute the estimate
beta_hat <- rq(site$do_pct ~ site$temp + site$sal + site$turb + site$depth.y,tau)$coef #calculates the slopes/intercept for the entire dataset

# Block bootstrap for the standard errors
nb    <- max(unique(site$Cycles)) #number of blocks
#plot(site$do_pct,col=site$Cycles) #plots the blocks next to each other

samps <-matrix(0,5,N)

start_time <- proc.time()

for(i in 1:N){
  samp <- NULL
  
  for(b in 1:nb){
    index  <- sample(1:nb,1) #sampling from one to the maximum number of blocks
    samp <- rbind(samp,site[site$Cycles==index,]) #collects specified tidal cycle from the dataset
    print(b)
  }   
  
  samps <- cbind(samps, rq(site$do_pct ~ site$temp + site$sal + site$turb + site$depth.y, tau)$coef)
}

print(proc.time()-start_time)
boxplot(samps) #boxplot of the slopes (or with 2 removed, slope and intercept)
SE   <- apply(samps,2,sd) #standard error
z    <- beta_hat/SE
pval <- 2*pnorm(-abs(z))

# Summarize the results

output <- cbind(beta_hat,SE,z,pval)
print(output) #save all of this summarized info


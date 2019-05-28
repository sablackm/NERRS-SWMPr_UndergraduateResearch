rm(list=ls())
library(quantreg)

n    <- 1000  # Sample size
rho  <- 0.90  # Autocorrelation
m    <- 50    # Block size
tau  <- 0.95  # Quantile level
beta <- 1     # True slope
N    <- 100   # Number of bootstrap samples

# This is making fake data.  You would skip this and use the real data

#Don't worry about, used for making fake data
 set.seed(0820)
 e    <- rnorm(n)
 Z    <- rep(0,n)
 Z[1] <- e[1]
 for(t in 2:n){
   Z[t] <- rho*Z[t-1] + e[t]
 }
 #notice he does not use a dataframe, but instead two separate variables
 X     <- rnorm(n)
 Y     <- 5 + beta*X + Z

#Compute the estimate

 beta_hat <- rq(Y~X,tau)$coef #calculates the slope/intercept for the entire dataset

# Block bootstrap for the standard errors

 block <- sort(rep(1:n,m))[1:n] # makes block id column
 nb    <- max(block) #number of blocks
 plot(Y,col=block) #plots the blocks next to each other

 samps <-matrix(0,N,2)

 for(i in 1:N){
   y_samp <- NULL
   x_samp <- NULL
   for(b in 1:nb){
     index  <- sample(1:nb,1) #sampling from one to the maximum number of blocks
     y_samp <- c(y_samp,Y[block==index]) #subsettinf (I will only have 1, for dataframe)
     x_samp <- c(x_samp,X[block==index])
   }   
   samps[i,] <- rq(y_samp~x_samp,tau)$coef[2] #elimate the [2] to get the slope and intercept
 }

 boxplot(samps) #boxplot of the slopes (or with 2 removed, slope and intercept)
 SE   <- apply(samps,2,sd) #standard error
 z    <- beta_hat/SE
 pval <- 2*pnorm(-abs(z))

# Summarize the results

 output <- cbind(beta_hat,SE,z,pval)
 print(output) #save all of this summarized info



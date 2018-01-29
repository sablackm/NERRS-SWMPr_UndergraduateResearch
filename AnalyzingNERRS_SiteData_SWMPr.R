install.packages("SWMPr")
library("SWMPr")

help.search(package = 'SWMPr', "organize")
help.search(package = 'SWMPr', "retrieve")

swmp1 <- import_local("C:/Users/sabla/Documents/Research/Undergraduate_Research_Data/SWMPr_Data_For_R", "nocecwq", trace = FALSE)
cleaned_swmp1 <- qaqc(swmp1)
dat <- subset(cleaned_swmp1, select = 'do_mgl', subset = c('2006-1-01 00:00', '2017-01-01 00:00'))
              
#dat <- subset(cleaned_swmp1, select = 'do_mgl', subset = c('2013-07-01 00:00', '2013-07-31 00:00'))

plot(dat)
plot(do_mgl ~ datetimestamp, dat)
plot(dat, type = 'n')
lines(dat, col = 'red')


# whole dataset
summary(dat)

# individual variables
summary(dat$do_mgl)

# mean, range, var, etc.
# note use of na.rm
mean(dat$do_mgl, na.rm = T)
range(dat$do_mgl, na.rm = T)
var(dat$do_mgl, na.rm = T)
sd(dat$do_mgl, na.rm = T)
min(dat$do_mgl, na.rm = T)
max(dat$do_mgl, na.rm = T)

# how many missing values?
sum(is.na(dat$do_mgl))

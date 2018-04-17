#--------------------------------------
#Sam Blackman 4/17/18
#This script is used to calculate Cumulative Distribution Function (CDF) for
#The defined Marshy Marsh Creek Habitiat types (currently only seen in the
#South Atlantic and Gulf of Mexico).
#Also seen in this script is the MIssing Value % seen in the data sets examined.
#--------------------------------------

rm(list = ls())

library("SWMPr")
library("tidyverse")
library("dplyr")
library("ggplot2")
library("reshape2")
library("lubridate")


#Importing Marshy Marsh Creek Data:

path <- "/Users/samuelblackman/Desktop/Research/NERRS/GulfofMexico"
sitename = 'gndbhwq'
data_collected <- import_local(path, sitename, trace = FALSE)
bh <- qaqc(data_collected)

path <- "/Users/samuelblackman/Desktop/Research/NERRS/SouthEast"
sitename = 'sapdcwq'
data_collected <- import_local(path, sitename, trace = FALSE)
dc <- qaqc(data_collected)

path <- "/Users/samuelblackman/Desktop/Research/NERRS/SouthEast"
sitename = 'sapldwq'
data_collected <- import_local(path, sitename, trace = FALSE)
ld <- qaqc(data_collected)


#CDF Plots
g_gnd <- ggplot(bh, aes(x=bh$do_mgl)) + stat_ecdf()
g_gnd

g_sap <- ggplot(dc, aes(x=dc$do_mgl)) + stat_ecdf()
g_sap

g_sap_control <-ggplot(ld, aes(x=ld$do_mgl)) + stat_ecdf()
g_sap_control










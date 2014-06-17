rm(list=ls(all=TRUE))

library(car)
library(gmodels)
library(lme4)
library(lubridate)
library(MASS)
library(nlme)
library(packrat)
library(plyr)
library(reshape)
library(xlsx)
library(contrast)
library(effects)
library(ggplot2)
library(scales)
library(xtable)

source("R//functions.R")

######################
# Process data frame #
######################
extr <- read.csv("Data//extractable.csv", 
                 colClasses=c("ring"="factor",
                              "plot"="factor",
                              "time"="factor",
                              "coverage" = "NULL"))
# remove unnecessary rows
extr <- droplevels(extr[complete.cases(extr), ])

# rename columns
names(extr)[c(2,6:8)] <- c("date","no", "nh", "po")


# organise data frame
extr <- within(extr, {
  # format date
  date <- as.Date(dmy(date))
  
  # add ID for layter analysis
  id <- ring:plot
  
  # add pre and post co2, not last of pre-co2 is used as a 
  # baseline of post-co2
  pre <- ifelse(time %in% c(1, 2), TRUE, FALSE)
  post <- ifelse(time != 1, TRUE, FALSE)
  
  # blocking
  block <- recode(ring, 
                  "c(1,2) = 'A'; c(3,4) = 'B'; c(5,6) = 'C'")
})


# save
save(extr, file = "Output//Data/extractable.RData")


# ##################
# # soil variables #
# ##################
# load("Data/FACE_TDR_ProbeDF.RData")
# 
# # subset soil
# TdrSoil <- subsetD(FACE_TDR_ProbeDF, Sample == "soil")
# 
# # compute mean of soil variable for given period
# SoilPeriodMean <- function(data, rings, plots, Start, End){
#   sDF <- subset(data, Date >= Start & Date >= End & ring == rings & plot == plots)
#   ddply(sDF, .(ring, plot),function(x) colMeans(x[c("Moist", "Temp_Mean", "Temp_Min", "Temp_Max")], na.rm = TRUE))
# }
# 
# IEMSoil <- ddply(iem, .(insertion, sampling, ring, plot), 
#                  function(x) SoilPeriodMean(data = TdrIem, Start = x$insertion, End = x$sampling, rings = x$ring, plot = x$plot))
# 
# # merge
# iem <- merge(iem, IEMSoil, by = c("insertion", "sampling", "ring", "plot"))
# 
# # save
# save(iem, file = "output//data//FACE_IEM.RData")

#######################
# Excel summary table #
#######################
source("R//SummaryExlTable.R")

########
# Figs #
########
source("R/Figs.R")

#########
# Stats #
#########
# parcent change
pchDF <- ddply(extr, .(ring, plot, co2, block, id), PerChange)


scatter.plot(~ no + pcNO + 
              nh + pcNH +
              p + pcP)

boxplot(pcP ~ co2:block, data = subsetD(pchDF, !pre))
boxplot(log(pcNH + 1) ~ co2:block, data = subsetD(pchDF, !pre))

boxplot(no ~ co2:block, data = subsetD(pchDF, !pre))
boxplot(log(pcNO+1) ~ co2:block, data = subsetD(pchDF, !pre))

range(pchDF$pcNO, na.rm = TRUE)
Delt
range(pchDF$pcNH, na.rm = TRUE)

boxplot(po ~ co2:block, data = subsetD(pchDF, !pre))





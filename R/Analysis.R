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
library(visreg)

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

##################
# soil variables #
##################
load("Data/FACE_TDR_ProbeDF.RData")

# subset soil
TdrSoil <- subsetD(FACE_TDR_ProbeDF, Sample == "soil")

# compute 3-month mean of soil variables for each plot
extrSoil <- ddply(extr, .(date, ring, plot), 
                  function(x) SoilPeriodMean(
                    data = TdrSoil, 
                    Start = x$date - 3 * 4 * 7, # (3 months before)
                    End = x$date, 
                    rings = x$ring, 
                    plot = x$plot))

# merge
extr <- merge(extr, extrSoil, by = c("date", "ring", "plot"))

# save
save(extr, file = "Output//Data/extractable.RData")


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
source("R/Stats.R")
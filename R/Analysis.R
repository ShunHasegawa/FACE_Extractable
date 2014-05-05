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


# format date
extr$date <- as.Date(dmy(extr$date))

# add ID for layter analysis
extr$id <- extr$ring:extr$plot

# add pre and post co2, not last of pre-co2 is used as a 
# baseline of post-co2
extr$pre <- ifelse(extr$time %in% c(1, 2), TRUE, FALSE)
extr$post <- ifelse(extr$time != 1, TRUE, FALSE)

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

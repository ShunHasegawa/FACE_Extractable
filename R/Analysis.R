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
names(extr)[6:8] <- c("no", "nh", "po")

# format date
extr$day <- as.Date(dmy(extr$day))

# add ID for layter analysis
extr$id <- extr$ring:extr$plot

# save
save(extr, file = "Output//Data/extractable.RData")



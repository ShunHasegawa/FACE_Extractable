rm(list=ls(all=TRUE))

source("R/pckg.R")

source("R//functions.R")

######################
# Process data frame #
######################
# source("R/ProcessDF.R")
load("Output//Data//extractable.RData")

# create file for HIEv
source("R/uploading_HIEv.R")

# parcent change
extr <- ddply(extr, .(ring, plot, co2, block, id), PerChange)
# column names for % changes look "Delt.1.arithmetic" on console but actually
# they are properly named. use names(pchDF) to check.

# postCO2 data frame for ancova with soil variables
postDF <- subsetD(extr, !pre)
save(postDF, file = "Output//Data/postDF.RData")

# change post co2 for literally for post-co2
extr$post <- !extr$pre

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

# save all objects
save.image(file = "Output/Data/AllObj.RData")

theme_set(theme_bw()) # graphic backgroud is white

######################
# summary data frame #
######################
RngMean <- ddply(extrMlt, .(time, date, co2, ring, variable), Crt_SmryDF) 
TrtMean <- TrtMean <- ddply(RngMean, .(time, date, co2, variable), function(x) Crt_SmryDF(x, val = "Mean"))
save(TrtMean, file = "Output//Data/FACE_Extractable_CO2Mean.RData")

#################################
# plot each nutrient separately #
#################################
vars <- c("Nitrate", "Ammonium", "Phosphate")

RngFg <- dlply(RngMean, .(variable), PltRnghMean)
fls <- paste("Output//Figs/FACE_Extractable_Ring_", vars, sep = "")
l_ply(1:3, function(x) ggsavePP(filename = fls[x], plot = RngFg[[x]], width = 6, height = 3))

TrtFg <- dlply(TrtMean, .(variable), PltCO2Mean)
fls <- paste("Output//Figs/FACE_Extractable_CO2_", vars, sep = "")
l_ply(1:3, function(x) ggsavePP(filename = fls[x], plot = TrtFg[[x]], width = 6, height = 3))

##################################
# plot all nutrient in one graph #
##################################
# labels for facet_grid
ylabs <- list(
  'no' = expression(KCl*-extractable~NO[3]^"-"*-N),
  'nh' = expression(KCl*-extractable~NH[4]^"+"*-N),
  'po' = expression(Bray*-extractable~PO[4]^"3-"*-P))


ylab_label <- function(variable, value){
  return(ylabs[value])
}

pl <- PltCO2Mean(TrtMean) +
  facet_grid(variable~., scales= "free_y", labeller= ylab_label)
ggsavePP(filename = "output//figs/FACE_IEM_CO2Trt", plot = pl, width = 6, height = 6)

########################
# Plot for publication #
########################
# load stat table, note that if you want the most updated one, you need to run
# Stat.R first
load("output//data//FACE_extractable_CO2xTime_Stats.RData")

# ymax value for each variable
ymaxDF <- ddply(TrtMean, .(variable), function(x) max(x$Mean + x$SE, na.rm = TRUE))
# adjust ymax
ymaxDF[3, 2] <- 2.7

# load contrastDF to annotate stat result and combine with max values from
# TrtMean as y position
load("Output//data/FACE_Extractable_ContrastDF.RData")
Antt_CntrstDF <- merge(ContrastDF, 
                       ddply(TrtMean, .(date, variable), summarise, yval = max(Mean + SE)),
                       # this return maximum values
                       by = c("date", "variable"), all.x = TRUE)
Antt_CntrstDF$co2 <- "amb" # co2 column is required as it's used for mapping
Antt_CntrstDF <- subset(Antt_CntrstDF, stars != "") 
  # remove empty rows as they causes trouble when using geom_text

# create a plot
p <- WBFig(data = TrtMean, 
           ylab = expression(Extractable~soil~nutrients~(mg~kg^"-1")),
           facetLab = ylab_label,
           StatRes = Stat_CO2Time, 
           StatY = c(ymaxDF[1, 2] + .3, ymaxDF[2:3, 2])) +
  geom_text(data = Antt_CntrstDF, aes(x = date, y = yval, label = stars), 
            vjust = 0, parse = TRUE)

# adjust ymax
p2 <- p + geom_blank(data = data.frame(date = as.Date(2012-06-13), 
                                       varaible = "po", 
                                       Mean = 2.7, 
                                       co2 = "amb"))
ggsavePP(filename = "Output//Figs/FACE_Manuscript/FACE_Extractable", plot = p2, 
         width = 6.65, height = 6.65)


#######################
# Plot soil variables #
#######################

##########################
## Process raw TDR data ##
##########################
load("Data/FACE_TDR_ProbeDF.RData")

# subset soil tdr
soilTDR <- subsetD(FACE_TDR_ProbeDF, Sample == "soil")

# add co2 and remove unnecessary columns
soilTDR <- within(soilTDR, {
  co2 <- factor(ifelse(ring %in% c(1, 4, 5), "elev", "amb"))
  plot <- factor(plot)
  Temp_Min <- NULL
  Temp_Max <- NULL
  Sample <- NULL
})

soilTDRdf <- melt(soilTDR, id = c("Date", "co2", "ring", "plot"))
soilTDRdf$type <- factor(ifelse(soilTDRdf$variable == "Moist", "Moist", "Temp")) 
# need "type" column for ggplot later

# compute mean
soilTDR_RngMean <- ddply(soilTDRdf, .(Date, co2, ring, variable, type), summarise, value = mean(value, na.rm = TRUE))
soilTDR_co2Mean <- ddply(soilTDR_RngMean, .(Date, co2, variable, type), summarise, value = mean(value, na.rm = TRUE))



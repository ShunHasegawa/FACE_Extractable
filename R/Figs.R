theme_set(theme_bw()) # graphic backgroud is white

######################
# summary data frame #
######################
RngMean <- ddply(extrMlt, .(time, date, co2, ring, variable), Crt_SmryDF) 
TrtMean <- TrtMean <- ddply(RngMean, .(time, date, co2, variable), function(x) Crt_SmryDF(x, val = "Mean"))

#################################
# plot each nutrient separately #
#################################
vars <- c("Nitrate", "Ammonium", "Phosphate")

RngFg <- dlply(RngMean, .(variable), PltMean)
fls <- paste("Output//Figs/FACE_Extractable_Ring_", vars, sep = "")
l_ply(1:3, function(x) ggsavePP(filename = fls[x], plot = RngFg[[x]], width = 6, height = 3))

TrtFg <- dlply(TrtMean, .(variable), PltMean)
fls <- paste("Output//Figs/FACE_Extractable_CO2_", vars, sep = "")
l_ply(1:3, function(x) ggsavePP(filename = fls[x], plot = TrtFg[[x]], width = 6, height = 3))

##################################
# plot all nutrient in one graph #
##################################
# labels for facet_grid
ylabs <- list(
  'no' = expression(NO[3]^"-"-N),
  'nh' = expression(NH[4]^"+"-N),
  'po' = expression(PO[4]^"3-"-P))


ylab_label <- function(variable, value){
  return(ylabs[value])
}

pl <- PltMean(TrtMean) +
  facet_grid(variable~., scales= "free_y", labeller= ylab_label)
ggsavePP(filename = "output//figs/FACE_IEM_CO2Trt", plot = pl, width = 6, height = 6)
























#################################
# plot each nutrient separately #
#################################
vars <- c("Nitrate", "Ammonium", "Phosphate")

RngFg <- dlply(RngMean, .(variable), PltMean)
fls <- paste("Output/Figs/WTC_Extractable_ring_", vars, ".pdf",sep = "")
l_ply(1:3, function(x) ggsave(filename = fls[x], plot = RngFg[[x]], width = 6, height = 3))

TrtFg <- dlply(TrtMean, .(variable), PltMean)
fls <- paste("Output/Figs/WTC_Extractable_CO2_", vars, ".pdf",sep = "")
l_ply(1:3, function(x) ggsave(filename = fls[x], plot = TrtFg[[x]], width = 6, height = 3))

##################################
# plot all nutrient in one graph #
##################################
# labels for facet_grid
ylabs <- list(
  'no' = expression(NO[3]^"-"-N),
  'nh' = expression(NH[4]^"+"-N),
  'po' = expression(PO[4]^"3-"-P))


ylab_label <- function(variable, value){
  return(ylabs[value])
}


pl <- PltMean(TrtMean) +
  facet_grid(variable~., scales= "free_y", labeller= ylab_label)
ggsave(filename = "Output//Figs/WTC_ExtractableNutrient_CO2.pdf", plot = pl, width = 6, height = 6)

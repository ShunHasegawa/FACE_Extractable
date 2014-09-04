#########################################
# Process soil variable data for ANCOVA #
#########################################

# Soil variables (i.g. moisture and temperature) will be used as covariates. I 
# need to deternmine how many days to go back from the sampling dates in order 
# to calculate their means for given period. The number of days to be used will
# be determined by AIC valuse for models with different periods used to obtain
# soil variable averages.

# TDR soil data
load("Data/FACE_TDR_ProbeDF.RData")

# subset soil
TdrSoil <- subsetD(FACE_TDR_ProbeDF, Sample == "soil")

##################################################
# Create mean of soil variable for ginven period #
##################################################

# Using the function, SoilVarPeriMean, create soil variables for given period
# and merge with data. Then run this for different periods and store all the
# resulted data frames in a sigle list

# Actual values
# LstDF_SoilVar <- llply(seq(0, 90, 1), 
#                        function(x) SoilVarPeriMean(data = postDF, period = x, SoilData = TdrSoil), 
#                        .progress = "text")
# names(LstDF_SoilVar) <- seq(0, 90, 1)
# save(LstDF_SoilVar, file =  "Output/Data/LstDF_SoilVar.RData")
load("Output/Data/LstDF_SoilVar.RData")

###########
# Nitrate #
###########
source("R/Stats_NO.R")

############
# Ammonium #
############
source("R/Stats_NH.R")

#############
# Phosphate #
#############
source("R/Stats_PO.R")

#######################
# Summary Stats table #
#######################
# create summary list
StatSmmryLst <- list("Nitrate" = list(AnvF_no, Est_NO),
                     "Ammonium" = list(AnvF_nh, Est_nh),
                     "Phosphate" = list(AnvF_P, Est_P))

# save in a single excel file
wb <- createWorkbook()
l_ply(c("Nitrate", "Ammonium", "Phosphate"), 
      function(x) CrSheetAnvTbl(workbook = wb, 
                                sheetName = x, 
                                smmaryLst = StatSmmryLst))
saveWorkbook(wb, "Output//Table/FACE_Extractable_Ancv.xlsx")

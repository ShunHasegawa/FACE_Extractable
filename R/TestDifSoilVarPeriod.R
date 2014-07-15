
# Soil variables (i.g. moisture and temperature) will be used as covariates. I 
# need to deternmine how many days to go back from the sampling dates in order 
# to calculate their means for given period. The number of days to be used will
# be determined by AIC valuse for models with different periods used to obtain
# soil variable averages.

#########################################
# Process soil variable data for ANCOVA #
#########################################
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

LstDF_SoilVar <- llply(seq(0, 90, 1), 
                       function(x) SoilVarPeriMean(data = postDF, period = x, SoilData = TdrSoil), 
                       .progress = "text")
names(LstDF_SoilVar) <- seq(0, 90, 1)
save(LstDF_SoilVar, file =  "Output/Data/LstDF_SoilVar.RData")


###############################################
# Run lmer for each data frame and return AIC #
###############################################
test <- LmrAicComp(ListDF = ListDF,
                   formula = formula(log(po) ~ co2 * (log(Moist) + Temp_Mean) + 
                     (1|block) + (1|ring) + (1|id)))

test <- LmrAicComp(ListDF = LstDF[1:30],
                   formula = formula(log(po) ~ co2 * (Moist + Temp_Mean) + 
                     (1|block) + (1|ring) + (1|id)))

test <- LmrAicComp(ListDF = LstDF[1:30],
                   formula = formula(log(no) ~ co2 * (log(Moist) + Temp_Mean) + 
                     (1|block) + (1|ring) + (1|id)))
Iml <- test[[1]]
Fml <- test[[2]]
Anova(Fml, test.statistic = "F")
plot(Fml)



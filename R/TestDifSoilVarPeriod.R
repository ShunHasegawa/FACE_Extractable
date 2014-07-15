
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
SoilVarPeriMean <- function(data, period){ 
  # period = number of days to back from sampling date to get average soil vars
  df <- ddply(data, .(date, ring, plot),
              function(x) SoilPeriodMean(
                data = TdrSoil, 
                Start = x$date - period,
                End = x$date, 
                rings = x$ring, 
                plot = x$plot))
  merge(data, df, by = c("date", "ring", "plot"))
}
  
# Using the above function create soil variables for given period and merge with
# data. Then run this for different periods and store all the resulted data
# frames in a sigle list

LstDF_SoilVar <- llply(seq(0, 90, 1), 
                       function(x) SoilVarPeriMean(data = data, period = x), 
                       .progress = "text")
names(LstDF_SoilVar) <- seq(0, 90, 1)

###############################################
# Run lmer for each data frame and return AIC #
###############################################
LmrAicComp <- function(ListDF, formula){
  # lmer test for each data set
  LstLmrs <- llply(ListDF, 
                   function(x) lmer(formula, data = x),
                   .progress = "text")
  names(LstLmrs) <- names(ListDF)
  
  # plot AIC
  aicDF <- ldply(LstLmrs, AIC)
  names(aicDF) <- c("period", "AICs")
  plot(AICs ~ period, data = aicDF, xlab = "N of Days back from sampling")
  
  
  # lmer for the lowest aic
  df <- ListDF[[which(aicDF$AICs == min(aicDF$AICs))]]
  Iml <- lmer(formula, data = df)
  Fml <- stepLmer(Iml)
  return(list("Initial model" = Iml, "Final model" = Fml, "Data" = df))
}


ListDF <- LstDF_SoilVar(data = postDF, period = 30)

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



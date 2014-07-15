# TDR soil data
load("Data/FACE_TDR_ProbeDF.RData")

# subset soil
TdrSoil <- subsetD(FACE_TDR_ProbeDF, Sample == "soil")

head(TdrSoil)

##################################################
# Create mean of soil variable for ginven period #
##################################################
testDF <- subsetD(extr[, c(1:12)], !pre)

SoilVarPeriMean <- function(data, period){ # period =  number of dates to get average soil vars
  ddply(data, .(date, ring, plot),
        function(x) SoilPeriodMean(
          data = TdrSoil, 
          Start = x$date - period,
          End = x$date, 
          rings = x$ring, 
          plot = x$plot))
}
  

# merge
LmrMultPeri <- function(period){
  df <- merge(testDF, SoilVarPeriMean(testDF, period = period), by = c("date", "ring", "plot"))
  m1 <- lmer(log(po) ~ co2 + log(Moist) + Temp_Mean + (1|block) + (1|ring) + (1|id), data = df)
  return(m1)
}


LstLmrs <- llply(seq(0, 110, 1), LmrMultPeri, .progress = "text")
names(LstLmrs) <- seq(0, 110, 1)
aicDF <- ldply(LstLmrs, AIC)
names(aicDF) <- c("period", "AICs")
plot(AICs ~ period, data = aicDF)
aicDF[which(aicDF$AICs == min(aicDF$AICs)),]
df <- merge(testDF, SoilVarPeriMean(testDF, period = 69), by = c("date", "ring", "plot"))
m1 <- lmer(sqrt(nh) ~ co2 * (log(Moist) + Temp_Mean) + (1|block) + (1|ring) + (1|id), data = df)
AIC(m1)
Anova(m1)
m2 <- stepLmer(m1)
Anova(m2)


?llply

names(LstLmrs2)
LstLmrs2

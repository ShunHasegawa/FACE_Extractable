
# Soil variables (i.g. moisture and temperature) will be used as covariates. I 
# need to deternmine how many days to go back from the sampling dates before 
# calculating their means for given period. The number of days to be used will 
# be determined by AIC valuse for models with different periods used to obtain
# soil var averages.

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
  df <- merge(postDF, SoilVarPeriMean(postDF, period = period), by = c("date", "ring", "plot"))
#   m1 <- lmer(log(po) ~ co2 + log(Moist) + Temp_Mean + (1|block) + (1|ring) + (1|id), data = df)
  return(df)
}



test <- llply(seq(0, 5, 1), 
              function(x) merge(postDF, SoilVarPeriMean(postDF, period = x), 
                                by = c("date", "ring", "plot")), 
              .progress = "text")
names(test) <- seq(0, 5, 1)


LstLmrs <- llply(test, 
                 function(x) lmer(log(po) ~ co2 + log(Moist) + Temp_Mean + (1|block) + (1|ring) + (1|id), data = x),
                 .progress = "text")
names(LstLmrs) <- seq(0, 5, 1)

aicDF <- ldply(LstLmrs, AIC)
names(aicDF) <- c("period", "AICs")

plot(AICs ~ period, data = aicDF)

aicDF[which(aicDF$AICs == min(aicDF$AICs)),]


a <- test[[which(aicDF$AICs == min(aicDF$AICs))]]
str(a)
m1 <- lmer(log(po) ~ co2 * (log(Moist) + Temp_Mean) + (1|block) + (1|ring) + (1|id), 
           data = a)

m1 <- lmer(log(po) ~ co2 * Temp_Mean + (1|block) + (1|ring) + (1|id), 
           data = a)

b <- 
m1 <- lmer(log(po) ~ co2 * (log(Moist) + Temp_Mean) + (1|block) + (1|ring) + (1|id), 
           data = a)

plot(log(a$Moist))

Anova(m1)

summary(a)
head(a)
te <- step(m1, reduce.random = FALSE, ddf = "Kenward-Roger")
Anova(te$model)
te <- step(m1, ddf = "Kenward-Roger")

m2 <- stepLmer(m1)

df <- merge(postDF, SoilVarPeriMean(postDF, period = 6), by = c("date", "ring", "plot"))
m1 <- lmer(log(po) ~ co2 * (log(Moist) + Temp_Mean) + (1|block) + (1|ring) + (1|id), data = df)
AIC(m1)
Anova(m1)
m2 <- stepLmer(m1)
Anova(m2)



Iml_ancv <- lmer(log(po) ~ co2 * (log(Moist) + Temp_Mean) + 
                   (1|block) + (1|ring) + (1|id), data = postDF)
?llply

names(LstLmrs2)
LstLmrs2

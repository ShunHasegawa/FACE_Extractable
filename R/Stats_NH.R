## ----Stat_FACE_Extr_Ammonium_PreCO2

range(extr$nh)

###########
# Pre-CO2 #
###########
bxplts(value= "nh", data= subsetD(extr, pre))
# row data seems better

# different random factor strucures
m1 <- lme(nh ~ co2 * time, random = ~1|block/ring/plot, data = subsetD(extr, pre))
RndmComp(m1)$anova
  # model2, 3 are better but use m1 for the time being

# autocorelation
atml <- atcr.cmpr(m1)
atml$models
  # no need for autocorrelation

Iml_pre <- atml[[1]]

# The starting model is:
Iml_pre$call
Anova(Iml_pre)

# model simplification
MdlSmpl(Iml_pre)
  # time:co2, co2 are removed

Fml_pre <- MdlSmpl(Iml_pre)$model.reml

# The final model is:
Fml_pre$call

Anova(Fml_pre)

summary(Fml_pre)

# model diagnosis
plot(Fml_pre)
qqnorm(Fml_pre, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_pre))
qqline(residuals.lm(Fml_pre))

## ----Stat_FACE_Extr_Ammonium_PostCO2

############
# Post-CO2 #
############

bxplts(value= "nh", data= subsetD(extr, post))
  # sqrt seems better

# different random factor strucures
m1 <- lme(sqrt(nh) ~ co2 * time, random = ~1|block/ring/plot, data = subsetD(extr, post))
RndmComp(m1)$anova
  # m5 is better but use m1 for the time being

# autocorelation
atml <- atcr.cmpr(m1)
atml$models
anova(atml[[1]], atml[[5]])
  # model5 is better, but no significant difference so use m1

Iml_post <- atml[[1]]

# The starting model is:
Iml_post$call
Anova(Iml_post)

# model simplification
MdlSmpl(Iml_post)
 #no factor is removed

Fml_post <- MdlSmpl(Iml_post)$model.reml

# The final model is:
Fml_post$call

Anova(Fml_post)

summary(Fml_post)

# plot(allEffects(Fml_post))

# contrast
cntrst<- contrast(Fml_post, 
                  a = list(time = levels(extr$time[extr$post, drop = TRUE]), co2 = "amb"),
                  b = list(time = levels(extr$time[extr$post, drop = TRUE]), co2 = "elev"))
FACE_Extr_PostCO2_NH_CntrstDf <- cntrstTbl(cntrst, data = extr[extr$post, ], digit = 2)

FACE_Extr_PostCO2_NH_CntrstDf

# model diagnosis
plot(Fml_post)
# wedge-shaped
qqnorm(Fml_post, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_post))
qqline(residuals.lm(Fml_post))

## ---- Stat_FACE_Extr_Ammonium_postCO2_withSoilVar
##########
# Ancova #
##########
# Determine how many days to go back from the sampling dates to calculate soil
# variables

# m1 <- LmrAicComp(ListDF = LstDF_SoilVar, 
#                  formula = formula(sqrt(nh) ~ co2 * (log(Moist) + Temp_Mean) + 
#                                      (1|block) + (1|ring) + (1|id)))

# The above code return erorr message as random factors don't explain any 
# variation. Redo manually

ListLmr <- llply(LstDF_SoilVar, function(x) lmer(sqrt(nh) ~ co2 * (Moist + Temp_Mean) + 
                                                   (1|block) + (1|ring) + (1|id), data = x))
aicDF <- ldply(ListLmr, AIC)
aicDF[which(aicDF$V1 == min(aicDF$V1)),]
# 69 days gives the lowest AIC

df <- LstDF_SoilVar[[which(aicDF$V1 == min(aicDF$V1))]]

## check the linearlity against soil variables

# plot against soil varriable
scatterplotMatrix(~ sqrt(nh) + Moist + Temp_Max + Temp_Mean + Temp_Min,
                  diag = "boxplot", df)

# plot for each plot against soil variables
print(xyplot(sqrt(nh) ~ Moist | ring + plot, df, type = c("r", "p")))
print(xyplot(sqrt(nh) ~ Temp_Max | ring + plot, df, type = c("r", "p")))
# looks fine

## Analysis
Iml_ancv <- lmer(sqrt(nh) ~ co2 * (Moist + Temp_Mean) + (1|block) + (1|ring) + (1|id), data = df)
Anova(Iml_ancv)

# model simplification: Note that because no variation is explained by random
# factors, I can't use stepLmer.
m2 <- lmer(sqrt(nh) ~ co2 *Temp_Mean + Moist + (1|block) + (1|ring) + (1|id), data = df)
anova(Iml_ancv, m2)
# remove co2:log(Moist)
Anova(m2)
Anova(m2, test.statistic = "F")

m3 <- lmer(sqrt(nh) ~ co2 + Temp_Mean + Moist + (1|block) + (1|ring) + (1|id), data = df)
anova(m2, m3)
Anova(m3)
Anova(m3, test.statistic = "F")

# co2:Temp_Mean is marginal. Removing this increases AIC. Keep this for time being

Fml_ancv <- m2
Anova(Fml_ancv)
Anova(Fml_ancv, test.statistic = "F")

# main effect
plot(allEffects(Fml_ancv))

# model diagnosis
plot(Fml_ancv)
  # little bit wedged..
qqnorm(resid(Fml_ancv))
qqline(resid(Fml_ancv))

## What if remove the one top outlier
qqval <- qqnorm(resid(Fml_ancv))
qqval$y[which(qqval$y == max(qqval$y))]

newDF <- df
newDF$nh[which(qqval$y == max(qqval$y))] <- NA
m1 <- lmer(sqrt(nh) ~ co2 * (Moist + Temp_Mean) + (1|block) + (1|ring) + (1|id), data = newDF)
m2 <- lmer(sqrt(nh) ~ co2 * Temp_Mean + Moist + (1|block) + (1|ring) + (1|id), data = newDF)
anova(m1, m2)
Anova(m2)
m3 <- lmer(sqrt(nh) ~ co2 + Moist + Temp_Mean + (1|block) + (1|ring) + (1|id), data = newDF)
anova(m2, m3)
Anova(m3)
Anova(m3, test.statistic = "F")

plot(m3)
qqnorm(resid(m3))
qqline(resid(m3))
plot(allEffects(m3))
# This looks better so use this
Iml_ancv <- m1
Fml_ancv <- m3

AnvF_nh <- Anova(Fml_ancv, test.statistic = "F")
AnvF_nh

# 95 % CI for each estimate
ciDF <- CIdf(Fml_ancv)

# calculate actual values
Est.val <- rbind(
  int = ciDF[1, ],
  co2elev = ciDF[2, ] + ciDF[1, 3],
  Moist = ciDF[3, ],
  Temp_Mean = ciDF[4, ]
)

Est.val

# reshape Est.val and make a table
Est_nh <- ANCV_Tbl(Est.val)

##############
## % change ##
##############
df <- LstDF_SoilVar[[84]] # use 3-month mean as this is % change in 3 months
range(df$pcNH)
bxplts(val = "pcNH", ofst = 1, data = df)
# use log

## checkout for linearity against soil variables

# plot against soil varriable
scatterplotMatrix(~ I(log(pcNH + 1)) + Moist + Temp_Max + Temp_Mean + Temp_Min, diag = "boxplot", df)
# temp looks polynomical..

# plot for each plot against soil variables
print(xyplot(log(pcNH + 1) ~ Moist | ring + plot, data = df, type = c("r", "p")))
print(xyplot(log(pcNH + 1) ~ Temp_Mean | ring + plot, df, type = c("r", "p")))

## Analysis
Iml_ancv_pc <- lmer(log(pcNH + 1) ~ co2 * (Moist + Temp_Mean) 
                    + (1|block) + (1|ring) + (1|id), data = df)
Anova(Iml_ancv_pc)
Fml_ancv_pc <- stepLmer(Iml_ancv_pc)
Anova(Fml_ancv_pc)
Anova(Fml_ancv_pc, test.statistic = "F")
plot(allEffects(Fml_ancv_pc))
plot(Fml_ancv_pc)
qqnorm(resid(Fml_ancv_pc))
qqline(resid(Fml_ancv_pc))

## 95 % CI
ciDF <- CIdf(Fml_ancv_pc)
ciDF

## ----Stat_FACE_Extr_Ammonium_PreCO2Smmry
# The starting model is:
Iml_pre$call
Anova(Iml_pre)

# The final model is:
Fml_pre$call
Anova(Fml_pre)

## ----Stat_FACE_Extr_Ammonium_PostCO2Smmry
# The starting model is:
Iml_post$call
Anova(Iml_post)

# The final model is:
Fml_post$call
Anova(Fml_post)

#  Contrast
FACE_Extr_PostCO2_NH_CntrstDf

## ---- Stat_FACE_Extr_Ammonium_postCO2_withSoilVarSmmry

# Initial model
Iml_ancv@call
Anova(Iml_ancv)

# Final model
Fml_ancv@call
Anova(Fml_ancv)

# 95 % CI
Est.val



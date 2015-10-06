## ----Stat_FACE_Extr_Ammonium_PreCO2

range(extr$nh)

###########
# Pre-CO2 #
###########
bxplts(value= "nh", data= subsetD(extr, pre))
# row data seems better

# different random factor strucures
Iml_pre_nh <- lmer(nh ~ co2 * time + (1|block/ring/plot), data = subsetD(extr, pre))

# The starting model is:
Iml_pre_nh@call
Anova(Iml_pre_nh)

# model simplification
Fml_pre_nh <- stepLmer(Iml_pre_nh, alpha.fixed = .1)

# The final model is:
Fml_pre_nh@call

Anova(Fml_pre_nh)

summary(Fml_pre_nh)

# model diagnosis
plot(Fml_pre_nh)
qqnorm(resid(Fml_pre_nh))
qqline(resid(Fml_pre_nh))

## ----Stat_FACE_Extr_Ammonium_PostCO2

############
# Post-CO2 #
############

bxplts(value= "nh", data= subsetD(extr, post))
  # sqrt seems better

# The initial model
Iml_post_nh <- lmer(sqrt(nh) ~ co2 * time + (1|block)+ (1|ring) + (1|id), 
                 data = subsetD(extr, post))
Anova(Iml_post_nh)
Anova(Iml_post_nh, test.statistic = "F")
  # no need to remove anything

# Model simplification
Fml_post_nh <- Iml_post_nh
Anova(Fml_post_nh)
AnvF_NH_post <- Anova(Fml_post_nh, test.statistic = "F") 
AnvF_NH_post

summary(Fml_post_nh)

# plot(allEffects(Fml_post_nh))

# model diagnosis
plot(Fml_post_nh)
qqnorm(residuals(Fml_post_nh))
qqline(residuals(Fml_post_nh))
  # not great

############
# contrast #
############
# Note that contrast doesn't work with lmer so use lme

# LmeMod <- lme(sqrt(nh) ~ co2 * time, random = ~1|block/ring/plot, data =
# subsetD(extr, post))

# This will give you error message for the contrast test saying Non-positive
# definite approximate variance-covariance. so relvel fixed factor and rerun.
newDF <- subsetD(extr, post)
newDF <- within(newDF, {
  co2 <- relevel(co2, "elev")
  time <- relevel(time, "7")})

LmeMod <- lme(sqrt(nh) ~ co2 * time, random = ~1|block/ring/plot, data = newDF)

cntrst<- contrast(LmeMod, 
                  a = list(time = levels(extr$time[extr$post, drop = TRUE]), co2 = "amb"),
                  b = list(time = levels(extr$time[extr$post, drop = TRUE]), co2 = "elev"))
FACE_Extr_PostCO2_NH_CntrstDf <- cntrstTbl(cntrst, data = extr[extr$post, ], variable = "nh")

FACE_Extr_PostCO2_NH_CntrstDf

## ---- Stat_FACE_Extr_Ammonium_postCO2_withSoilVar
##########
# Ancova #
##########
# Determine how many days to go back from the sampling dates to calculate soil
# variables

m1 <- LmrAicComp(ListDF = LstDF_SoilVar, 
                 formula = formula(sqrt(nh) ~ co2 * (log(Moist) + Temp_Mean) + 
                                     (1|block) + (1|ring) + (1|id)))

aicDF <- m1$AICdf
aicDF[which(aicDF$AICs == min(aicDF$AICs)), ]
# 69 days gives the lowest AIC

df <- m1$Data

## check the linearlity against soil variables

# plot against soil varriable
scatterplotMatrix(~ sqrt(nh) + Moist + Temp_Max + Temp_Mean + Temp_Min,
                  diag = "boxplot", df)

# plot for each plot against soil variables
print(xyplot(sqrt(nh) ~ Moist | ring + plot, df, type = c("r", "p")))
print(xyplot(sqrt(nh) ~ Temp_Max | ring + plot, df, type = c("r", "p")))
# looks fine

## Analysis
Iml_ancv_nh <- lmer(sqrt(nh) ~ co2 * (Moist + Temp_Mean) + (1|block) + (1|ring) + (1|id), data = df)
Anova(Iml_ancv_nh)

# model simplification: Note that because no variation is explained by random
# factors, I can't use stepLmer.
m2 <- lmer(sqrt(nh) ~ co2 *Temp_Mean + Moist + (1|block) + (1|ring) + (1|id), data = df)
anova(Iml_ancv_nh, m2)
# remove co2:log(Moist)
Anova(m2)
Anova(m2, test.statistic = "F")

m3 <- lmer(sqrt(nh) ~ co2 + Temp_Mean + Moist + (1|block) + (1|ring) + (1|id), data = df)
anova(m2, m3)
Anova(m3)
Anova(m3, test.statistic = "F")

# co2:Temp_Mean is marginal. Removing this increases AIC. Keep this for time being

Fml_ancv_nh <- m2
Anova(Fml_ancv_nh)
Anova(Fml_ancv_nh, test.statistic = "F")

# main effect
plot(allEffects(Fml_ancv_nh))

# model diagnosis
plot(Fml_ancv_nh)
  # little bit wedged..
qqnorm(resid(Fml_ancv_nh))
qqline(resid(Fml_ancv_nh))

## What if remove the one top outlier
qqval <- qqnorm(resid(Fml_ancv_nh))
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

# co2 is not significnat (P > .1) with F test, so remove this time
m4 <- lmer(sqrt(nh) ~ Moist + Temp_Mean + (1|block) + (1|ring) + (1|id), data = newDF)
anova(m3, m4)
Anova(m4, test.statistic = "F")
plot(m4)
qqnorm(resid(m4))
qqline(resid(m4))
plot(allEffects(m4))

Iml_ancv_nh <- m1
Fml_ancv_nh <- m4

AnvF_nh <- Anova(Fml_ancv_nh, test.statistic = "F")
AnvF_nh

# 95 % CI for each estimate
ciDF <- CIdf(Fml_ancv_nh)

# calculate actual values
Est.val <- ciDF
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
Iml_ancv_nh_pc <- lmer(log(pcNH + 1) ~ co2 * (Moist + Temp_Mean) 
                    + (1|block) + (1|ring) + (1|id), data = df)
Anova(Iml_ancv_nh_pc)
Fml_ancv_nh_pc <- stepLmer(Iml_ancv_nh_pc)
Anova(Fml_ancv_nh_pc)
Anova(Fml_ancv_nh_pc, test.statistic = "F")
plot(allEffects(Fml_ancv_nh_pc))
plot(Fml_ancv_nh_pc)
qqnorm(resid(Fml_ancv_nh_pc))
qqline(resid(Fml_ancv_nh_pc))

## 95 % CI
ciDF <- CIdf(Fml_ancv_nh_pc)
ciDF

## ----Stat_FACE_Extr_Ammonium_PreCO2Smmry
# The starting model is:
Iml_pre_nh@call
Anova(Iml_pre_nh)

# The final model is:
Fml_pre_nh@call
Anova(Fml_pre_nh)

## ----Stat_FACE_Extr_Ammonium_PostCO2Smmry
# The starting model is:
Iml_post_nh@call
Anova(Iml_post_nh)

# The final model is:
Fml_post_nh@call

# Chi-square
Anova(Fml_post_nh)

# F-test
AnvF_NH_post

#  Contrast
FACE_Extr_PostCO2_NH_CntrstDf

## ---- Stat_FACE_Extr_Ammonium_postCO2_withSoilVarSmmry

# Initial model
Iml_ancv_nh@call
Anova(Iml_ancv_nh)

# Final model
Fml_ancv_nh@call

# Chisq
Anova(Fml_ancv_nh)

# F-test
AnvF_nh

# squared r
rsquared.glmm(Fml_ancv_nh)

# 95 % CI
Est_nh
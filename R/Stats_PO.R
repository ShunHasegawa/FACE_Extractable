## ----Stat_FACE_Extr_Phosphate_PreCO2

range(extr$po)

###########
# Pre-CO2 #
###########
bxplts(value= "po", data= subsetD(extr, pre))
  # use row data

# different random factor strucures
m1 <- lme(po ~ co2 * time, random = ~1|block/ring/plot, data = subsetD(extr, pre))
RndmComp(m1)$anova
  # m5 is better, but use m1 for time being

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
  # no factor is removed

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

## ----Stat_FACE_Extr_Phosphate_PostCO2

############
# Post-CO2 #
############

bxplts(value= "po", data= subsetD(extr, post))
  # log seems better

# The initial model
Iml_post <- lmer(log(po) ~ co2 * time + (1|block)+ (1|ring) + (1|id), 
                 data = subsetD(extr, post))
Anova(Iml_post)
Anova(Iml_post, test.statistic = "F")
# no need to remove anything

# Model simplification
Fml_post <- Iml_post
Anova(Fml_post)
AnvF_P_post <- Anova(Fml_post, test.statistic = "F") 
AnvF_P_post

summary(Fml_post)

# plot(allEffects(Fml_post))

# model diagnosis
plot(Fml_post)
qqnorm(residuals(Fml_post))
qqline(residuals(Fml_post))

# contrast
# Note that contrast doesn't work with lmer so use lme
LmeMod <- lme(log(po) ~ co2 * time, random = ~1|block/ring/plot, 
              data = subsetD(extr, post))

cntrst<- contrast(LmeMod, 
                  a = list(time = levels(extr$time[extr$post, drop = TRUE]), co2 = "amb"),
                  b = list(time = levels(extr$time[extr$post, drop = TRUE]), co2 = "elev"))
FACE_Extr_PostCO2_PO_CntrstDf <- cntrstTbl(cntrst, data = extr[extr$post, ], variable = "po")

FACE_Extr_PostCO2_PO_CntrstDf

## ---- Stat_FACE_Extr_Phosphate_postCO2_withSoilVar

##########
# Ancova #
##########

#############
## Raw dta ##
#############

# Determine how many days to go back from the sampling dates to calculate soil
# variables
m1 <- LmrAicComp(ListDF = LstDF_SoilVar, 
                 formula = formula(log(po) ~ co2 * (log(Moist) + Temp_Mean) + 
                                       (1|block) + (1|ring) + (1|id)))
m1$AICdf
# 90 days showed the lowest AIC... but not sure if it makes sense but the result
# would be the same when you use 30 days anyway

## checkout for linearity against soil variables

# plot against soil varriable
scatterplotMatrix(~ log(po) + log(Moist) + Temp_Max + Temp_Mean + Temp_Min,
                  diag = "boxplot", m1$Data)

# plot for each plot against soil variables
print(xyplot(log(po) ~ log(Moist) | ring + plot, m1$Data, type = c("r", "p")))
print(xyplot(log(po) ~ Temp_Mean | ring + plot, m1$Data, type = c("r", "p")))
# looks fine

## Analysis
Iml_ancv <- m1$Initial
# in summary Iml_ancv, it says "some computational error has occurred in 
# lmerTest". There we can't use stepLmer so rewrite lmer for this model manually
# this time till I find a solution. Might be partly becuse data is given within
# the function.
Iml_ancv <- lmer(log(po) ~ co2 * (log(Moist) + Temp_Mean) 
            + (1 | block) + (1 |ring) + (1 | id), data = m1$Data)
Fml_ancv <- stepLmer(Iml_ancv)
Anova(Fml_ancv)
AnvF_po <- Anova(Fml_ancv, test.statistic = "F")
AnvF_po

# main effects
plot(allEffects(Fml_ancv))

# model diagnosis
plot(Fml_ancv)
qqnorm(resid(Fml_ancv))
qqline(resid(Fml_ancv))

## Confidence intervals
# confidence interval for estimated parameters
ciDF <- CIdf(model = Fml_ancv)
Est.val <- ciDF
Est.val

# reshape Est.val and make a table
Est_P <- ANCV_Tbl(Est.val)

##############
## % Change ##
##############
df <- LstDF_SoilVar[[84]] # use 3-month mean as this is % change in 3 months
range(df$pcP)
bxplts(value= "pcP", ofst = .6, data= df)
# use poer(1/3)

## checkout for linearity against soil variables

# plot against soil varriable
scatterplotMatrix(~ I(pcP^(1/3)) + Moist + Temp_Max + Temp_Mean + Temp_Min, diag = "boxplot", df)

# plot for each plot against soil variables
print(xyplot(pcP^(1/3) ~ Moist | ring + plot, data = df, type = c("r", "p")))
print(xyplot(pcP^(1/3) ~ Temp_Mean | ring + plot, df, type = c("r", "p")))
# looks fine

## Analysis
Iml_ancv_pc <- lmer(pcP^(1/3) ~ co2 * (Moist + Temp_Mean) + (1|block) + (1|ring) + (1|id), data = df)

Anova(Iml_ancv_pc)

# Fml_ancv_pc <- stepLmer(Iml_ancv_pc)
# gives error message as random factors don't explain any variation
m2 <- lmer(pcP^(1/3) ~ co2 + Moist + Temp_Mean + (1|block) + (1|ring) + (1|id), data = df)
anova(Iml_ancv_pc, m2)
Anova(m2)

m3 <- lmer(pcP^(1/3) ~ Moist + Temp_Mean + (1|block) + (1|ring) + (1|id), data = df)
m4 <- lmer(pcP^(1/3) ~ co2 + Temp_Mean + (1|block) + (1|ring) + (1|id), data = df)
m5 <- lmer(pcP^(1/3) ~ Temp_Mean + (1|block) + (1|ring) + (1|id), data = df)
anova(m2, m3)
anova(m2, m4)
anova(m2, m5)

Fml_ancv_pc <- m5
Anova(Fml_ancv_pc)
Anova(Fml_ancv_pc, test.statistic = "F")

# main effects
plot(allEffects(Fml_ancv_pc))

# model diagnosis
plot(Fml_ancv_pc)
qqnorm(resid(Fml_ancv_pc))
qqline(resid(Fml_ancv_pc))

## Confidence intervals ##
# confidence interval for estimated parameters
ciDF <- CIdf(model = Fml_ancv_pc)
Est.val <- ciDF
Est.val

# reshape Est.val and make a table
Est_pcP <- ANCV_Tbl(Est.val)

## ----Stat_FACE_Extr_Phosphate_PreCO2Smmry
# The starting model is:
Iml_pre$call
Anova(Iml_pre)

# The final model is:
Fml_pre$call
Anova(Fml_pre)

## ----Stat_FACE_Extr_Phosphate_PostCO2Smmry
# The starting model is:
Iml_post@call
Anova(Iml_post)

# The final model is:
Fml_post@call

# Chi-square
Anova(Fml_post)

# F-test
AnvF_P_post

# contrast
FACE_Extr_PostCO2_PO_CntrstDf

## ---- Stat_FACE_Extr_Phosphate_postCO2_withSoilVarSmmry
# The initial model is
Iml_ancv@call
Anova(Iml_ancv)

# The final model is
Fml_ancv@call

# Chisq
Anova(Fml_ancv)

# F-test
AnvF_po

# squared r
rsquared.glmm(Fml_ancv)

# 95 % CI
Est_P
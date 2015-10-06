## ----Stat_FACE_Extr_Phosphate_PreCO2

range(extr$po)

###########
# Pre-CO2 #
###########
bxplts(value= "po", data= subsetD(extr, pre))
  # use raw data

# different random factor strucures
Iml_pre_po <- lmer(po ~ co2 * time + (1|block/ring/plot), 
                   data = subsetD(extr, pre))

# The starting model is:
Iml_pre_po@call
Anova(Iml_pre_po)
Anova(Iml_pre_po, test.statistic = "F")

# model simplification
Fml_pre_po <- stepLmer(Iml_pre_po, alpha.fixed = .1)

# The final model is:
Fml_pre_po@call

Anova(Fml_pre_po)
Anova(Fml_pre_po, test.statistic = "F")

summary(Fml_pre_po)

# model diagnosis
plot(Fml_pre_po)
qqnorm(residuals(Fml_pre_po))
qqline(residuals(Fml_pre_po))

############
# contrast #
############
newDF <- subsetD(extr, pre)
# newDF <- within(newDF, {
#   co2 <- relevel(co2, "elev")
#   time <- relevel(time, "7")})

LmeMod <- lme(po ~ co2 * time, random = ~1|block/ring/plot, data = newDF)

cntrst<- contrast(LmeMod, 
                  a = list(time = levels(extr$time[extr$pre, drop = TRUE]), co2 = "amb"),
                  b = list(time = levels(extr$time[extr$pre, drop = TRUE]), co2 = "elev"))
cntrst

## ----Stat_FACE_Extr_Phosphate_PostCO2

############
# Post-CO2 #
############

bxplts(value= "po", data= subsetD(extr, post))
  # log seems better

# The initial model
Iml_post_po <- lmer(log(po) ~ co2 * time + (1|block)+ (1|ring) + (1|id), 
                 data = subsetD(extr, post))
Anova(Iml_post_po)
Anova(Iml_post_po, test.statistic = "F")

# Model simplification
Fml_post_po <- stepLmer(Iml_post_po, alpha.fixed = .1)
Anova(Fml_post_po)
AnvF_P_post <- Anova(Fml_post_po, test.statistic = "F") 
AnvF_P_post

summary(Fml_post_po)

# plot(allEffects(Fml_post_po))

# model diagnosis
plot(Fml_post_po)
qqnorm(residuals(Fml_post_po))
qqline(residuals(Fml_post_po))

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
Iml_ancv_po <- m1$Initial
# in summary Iml_ancv_po, it says "some computational error has occurred in 
# lmerTest". There we can't use stepLmer so rewrite lmer for this model manually
# this time till I find a solution. Might be partly becuse data is given within
# the function.
Iml_ancv_po <- lmer(log(po) ~ co2 * (log(Moist) + Temp_Mean) 
            + (1 | block) + (1 |ring) + (1 | id), data = m1$Data)
Fml_ancv_po <- stepLmer(Iml_ancv_po)
Anova(Fml_ancv_po)
AnvF_po <- Anova(Fml_ancv_po, test.statistic = "F")
AnvF_po

# main effects
plot(allEffects(Fml_ancv_po))

# model diagnosis
plot(Fml_ancv_po)
qqnorm(resid(Fml_ancv_po))
qqline(resid(Fml_ancv_po))

## Confidence intervals
# confidence interval for estimated parameters
ciDF <- CIdf(model = Fml_ancv_po)
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
Iml_ancv_po_pc <- lmer(pcP^(1/3) ~ co2 * (Moist + Temp_Mean) + (1|block) + (1|ring) + (1|id), data = df)

Anova(Iml_ancv_po_pc)

# Fml_ancv_po_pc <- stepLmer(Iml_ancv_po_pc)
# gives error message as random factors don't explain any variation
m2 <- lmer(pcP^(1/3) ~ co2 + Moist + Temp_Mean + (1|block) + (1|ring) + (1|id), data = df)
anova(Iml_ancv_po_pc, m2)
Anova(m2)

m3 <- lmer(pcP^(1/3) ~ Moist + Temp_Mean + (1|block) + (1|ring) + (1|id), data = df)
m4 <- lmer(pcP^(1/3) ~ co2 + Temp_Mean + (1|block) + (1|ring) + (1|id), data = df)
m5 <- lmer(pcP^(1/3) ~ Temp_Mean + (1|block) + (1|ring) + (1|id), data = df)
anova(m2, m3)
anova(m2, m4)
anova(m2, m5)

Fml_ancv_po_pc <- m5
Anova(Fml_ancv_po_pc)
Anova(Fml_ancv_po_pc, test.statistic = "F")

# main effects
plot(allEffects(Fml_ancv_po_pc))

# model diagnosis
plot(Fml_ancv_po_pc)
qqnorm(resid(Fml_ancv_po_pc))
qqline(resid(Fml_ancv_po_pc))

## Confidence intervals ##
# confidence interval for estimated parameters
ciDF <- CIdf(model = Fml_ancv_po_pc)
Est.val <- ciDF
Est.val

# reshape Est.val and make a table
Est_pcP <- ANCV_Tbl(Est.val)

## ----Stat_FACE_Extr_Phosphate_PreCO2Smmry
# The starting model is:
Iml_pre_po@call
Anova(Iml_pre_po)

# The final model is:
Fml_pre_po@call
Anova(Fml_pre_po)
Anova(Fml_pre_po, test.statistic = "F")

## ----Stat_FACE_Extr_Phosphate_PostCO2Smmry
# The starting model is:
Iml_post_po@call
Anova(Iml_post_po)

# The final model is:
Fml_post_po@call

# Chi-square
Anova(Fml_post_po)

# F-test
AnvF_P_post

## ---- Stat_FACE_Extr_Phosphate_postCO2_withSoilVarSmmry
# The initial model is
Iml_ancv_po@call
Anova(Iml_ancv_po)

# The final model is
Fml_ancv_po@call

# Chisq
Anova(Fml_ancv_po)

# F-test
AnvF_po

# squared r
# rsquared.glmm(Fml_ancv_po)

# 95 % CI
Est_P

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

# different random factor strucures
m1 <- lme(log(po) ~ co2 * time, random = ~1|block/ring/plot, data = subsetD(extr, post))
RndmComp(m1)$anova
# m3 is better, but use m1 for time being

# autocorelation
atml <- atcr.cmpr(m1)
atml$models
  # no need of autocorrelation

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
FACE_Extr_PostCO2_PO_CntrstDf <- cntrstTbl(cntrst, data = extr[extr$post, ], digit = 2)

FACE_Extr_PostCO2_PO_CntrstDf

# model diagnosis
plot(Fml_post)
qqnorm(Fml_post, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_post))
qqline(residuals.lm(Fml_post))

## ---- Stat_FACE_Extr_Phosphate_postCO2_withSoilVar

##########
# Ancova #
##########

# Determine how many days to go back from the sampling dates to calculate soil
# variables
m1 <- LmrAicComp(ListDF = LstDF_SoilVar, 
                 formula = formula(log(po) ~ co2 * (log(Moist) + Temp_Mean) + 
                                       (1|block) + (1|ring) + (1|id)))
m1$AICdf
# 90 days showed the lowest AIC... but not sure if it makes sense but the result
# would be the same when you use 30 days anyway
Iml_ancv <- m1$Initial
Fml_ancv <- m1$Final
Anova(Fml_ancv)

# plot against soil varriable
scatterplotMatrix(~ log(po) + log(Moist) + Temp_Max + Temp_Mean + Temp_Min,
                  diag = "boxplot", m1$Data)

# plot for each plot against soil variables
print(xyplot(log(po) ~ log(Moist) | ring + plot, m1$Data, type = c("r", "p")))
print(xyplot(log(po) ~ Temp_Mean | ring + plot, m1$Data, type = c("r", "p")))

# main effects
plot(allEffects(Fml_ancv))

# model diagnosis
plot(Fml_ancv)
qqnorm(resid(Fml_ancv))
qqline(resid(Fml_ancv))

## ----Stat_FACE_Extr_Phosphate_PreCO2Smmry
# The starting model is:
Iml_pre$call
Anova(Iml_pre)

# The final model is:
Fml_pre$call
Anova(Fml_pre)

## ----Stat_FACE_Extr_Phosphate_PostCO2Smmry
# The starting model is:
Iml_post$call
Anova(Iml_post)

# The final model is:
Fml_post$call
Anova(Fml_post)

# contrast
FACE_Extr_PostCO2_PO_CntrstDf

## ---- Stat_FACE_Extr_Phosphate_postCO2_withSoilVarSmmry
# The initial model is
Iml_ancv@call
Anova(Iml_ancv)

# The final model is
Fml_ancv@call
Anova(Fml_ancv)
Anova(Fml_ancv, test.statistic = "F")


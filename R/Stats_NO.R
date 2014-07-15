## ----Stat_FACE_Extr_Nitrate_PreCO2

range(extr$no)

###########
# Pre-CO2 #
###########

bxplts(value= "no", data= subsetD(extr, pre))
  # log seems slightly better

# different random factor strucures
m1 <- lme(log(no) ~ co2 * time, random = ~1|ring/plot, data = subsetD(extr, pre))
m2 <- lme(log(no) ~ co2 * time, random = ~1|ring, data = subsetD(extr, pre))
m3 <- lme(log(no) ~ co2 * time, random = ~1|id, data = subsetD(extr, pre))
anova(m1, m2, m3)
  # m1 is better

# autocorelation
atcr.cmpr(m1, rndmFac="ring/plot")$models
  # no need for correlation

Iml_pre <- m1

# The starting model is:
Iml_pre$call
Anova(Iml_pre)

# model simplification
MdlSmpl(Iml_pre)
  # no factor was removed, but time:co2 is not 
  # significant so remove

spml <- update(MdlSmpl(Iml_pre)$model.ml, ~. - time:co2)
MdlSmpl(spml)
  # co2 is removed

Fml_pre <- MdlSmpl(spml)$model.reml

# The final model is:
Fml_pre$call

Anova(Fml_pre)

summary(Fml_pre)

# model diagnosis
plot(Fml_pre)
qqnorm(Fml_pre, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_pre))
qqline(residuals.lm(Fml_pre))

## ----Stat_FACE_Extr_Nitrate_PostCO2

############
# Post-CO2 #
############

bxplts(value= "no", data= subsetD(extr, post))
  # log seems better

# different random factor strucures
m1 <- lme(log(no) ~ co2 * time, random = ~1|ring/plot, data = subsetD(extr, post))
m2 <- lme(log(no) ~ co2 * time, random = ~1|ring, data = subsetD(extr, post))
m3 <- lme(log(no) ~ co2 * time, random = ~1|id, data = subsetD(extr, post))
anova(m1, m2, m3)
  # m1 is better

# autocorelation
atcr.cmpr(m1, rndmFac="ring/plot")$models
  # model 5 looks better

Iml_post <- atcr.cmpr(m2, rndmFac="ring")[[5]]

# The starting model is:
Iml_post$call
Anova(Iml_post)


# model simplification
MdlSmpl(Iml_post)
  # co2xtime, co2 are removed

Fml_post <- MdlSmpl(Iml_post)$model.reml

# The final model is:
Fml_post$call

Anova(Fml_post)

summary(Fml_post)

# plot(allEffects(Fml_post))

# model diagnosis
plot(Fml_post)
qqnorm(Fml_post, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_post))
qqline(residuals.lm(Fml_post))
  #not great

## ---- Stat_FACE_Extr_Nitrate_postCO2_withSoilVar
##########
# Ancova #
##########
# Determine how many days to go back from the sampling dates to calculate soil
# variables
m1 <- LmrAicComp(ListDF = LstDF_SoilVar, 
                 formula = formula(log(no) ~ co2 * (log(Moist) + Temp_Mean) + 
                                     (1|block) + (1|ring) + (1|id)))
aicDF <- m1$AICdf
aicDF[which(aicDF$AICs == min(aicDF$AICs)), ]
# 89 days gives the lowest AIC, not sure if it makes sense but use this for time
# being
df <- LstDF_SoilVar[[which(aicDF$AICs == min(aicDF$AICs))]]

## check linearity agains soil variables

# plot against soil varriable
scatterplotMatrix(~ log(no) + log(Moist) + Temp_Max + Temp_Mean + Temp_Min,
                  diag = "boxplot", df)

# plot for each plot against soil variables
print(xyplot(log(no) ~ log(Moist) | ring + plot, df, type = c("r", "p")))
print(xyplot(log(no) ~ Temp_Mean | ring + plot, df, type = c("r", "p")))
# looks fine

## Analysis
Iml_ancv <- lmer(log(no) ~ co2 * (log(Moist) + Temp_Mean) + 
                   (1|block) + (1|ring) + (1|id), data = df)
Anova(Iml_ancv)
plot(allEffects(Iml_ancv))
plot(Iml_ancv)
qqnorm(resid(Iml_ancv))
qqline(resid(Iml_ancv))



## ----Stat_FACE_Extr_Nitrate_PreCO2Smmry
# The starting model is:
Iml_pre$call
Anova(Iml_pre)

# The final model is:
Fml_pre$call
Anova(Fml_pre)

## ----Stat_FACE_Extr_Nitrate_PostCO2Smmry
# The starting model is:
Iml_post$call
Anova(Iml_post)

# The final model is:
Fml_post$call
Anova(Fml_post)

## ---- Stat_FACE_Extr_Nitrate_postCO2_withSoilVarSmmry
Iml_ancv$call
Anova(Iml_ancv)
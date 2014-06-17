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

# plot against soil varriable
scatterplotMatrix(~ no + log(Moist) + Temp_Max + Temp_Mean + Temp_Min,
                  diag = "boxplot", 
                  subsetD(extr, !pre))

scatterplotMatrix(~ log(no) + log(Moist) + Temp_Max + Temp_Mean + Temp_Min,
                  diag = "boxplot", 
                  subsetD(extr, !pre))
# plot for each plot against soil variables
print(xyplot(log(no) ~ log(Moist) | ring + plot, subsetD(extr, !pre), type = c("r", "p")))

# analysis
# Note Temp_Max and log(Moist) appears to be correlated so shouln't be 
# placed in a multiple regression model
Iml_ancv <- lme(log(no) ~ co2 * log(Moist), 
                random = ~1|block/ring/plot,  
                data = subsetD(extr, !pre))
Anova(Iml_ancv)
# no need to use covariates

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
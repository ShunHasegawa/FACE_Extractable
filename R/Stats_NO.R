## ----Stat_FACE_Extr_Nitrate_PreCO2

range(extr$no)

###########
# Pre-CO2 #
###########

bxplts(value= "no", data= subsetD(extr, pre))
  # log seems slightly better

# different random factor strucures
m1 <- lme(log(no) ~ co2 * time, random = ~1|block/ring/plot, data = subsetD(extr, pre))
RndmComp(m1)$anova
# model3 is the best but use m1 for the time being

# autocorelation
atml <- atcr.cmpr(m1)
atml$models
# no need for correlation

Iml_pre <- atml[[1]]

# The starting model is:
Iml_pre$call
Anova(Iml_pre)

# model simplification
Fml_pre <- MdlSmpl(Iml_pre)$model.reml
Anova(Fml_pre)
  # very merginal significant co2:time interaction

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

# The initial model
Iml_post <- lmer(log(no) ~ co2 * time + (1|block)+ (1|ring) + (1|id), 
                 data = subsetD(extr, post))
Anova(Iml_post)

# Model simplification
Fml_post <- stepLmer(Iml_post)
Anova(Fml_post)
AnvF_NO_post <- Anova(Fml_post, test.statistic = "F") 
AnvF_NO_post

summary(Fml_post)

# plot(allEffects(Fml_post))

# model diagnosis
plot(Fml_post)
qqnorm(residuals(Fml_post))
qqline(residuals(Fml_post))
  #not great

## ---- Stat_FACE_Extr_Nitrate_postCO2_withSoilVar
##########
# Ancova #
##########

##############
## Raw data ##
##############

# Determine how many days to go back from the sampling dates to calculate soil
# variables
m1 <- LmrAicComp(ListDF = LstDF_SoilVar, 
                 formula = formula(log(no) ~ co2 * (log(Moist) + Temp_Mean) + 
                                     (1|block) + (1|ring) + (1|id)))
aicDF <- m1$AICdf
aicDF[which(aicDF$AICs == min(aicDF$AICs)), ]
# 89 days gives the lowest AIC, not sure if it makes sense but use this for time
# being
df <- m1$Data

## check linearity agains soil variables

# plot against soil varriable
scatterplotMatrix(~ log(no) + log(Moist) + Temp_Max + Temp_Mean + Temp_Min,
                  diag = "boxplot", df)

# plot for each plot against soil variables
print(xyplot(log(no) ~ log(Moist) | ring + plot, m1$Data, type = c("r", "p")))
print(xyplot(log(no) ~ Temp_Mean | ring + plot, m1$Data, type = c("r", "p")))
# looks fine

## Analysis
Iml_ancv <- m1$Initial
Anova(Iml_ancv)

Fml_ancv <- stepLmer(Iml_ancv)
Anova(Fml_ancv)
AnvF_no <- Anova(Fml_ancv, test.statistic = "F")
AnvF_no

plot(allEffects(Fml_ancv))
plot(Fml_ancv)
qqnorm(resid(Fml_ancv))
qqline(resid(Fml_ancv))

########################
# Confidence intervals #
########################
# confidence interval for estimated parameters
ciDF <- CIdf(model = Fml_ancv)
Est.val <- ciDF
Est.val

# reshape Est.val and make a table
Est_NO <- ANCV_Tbl(Est.val)

##############
## % change ##
##############
df <- LstDF_SoilVar[[84]] # use 3-month mean as this is % change in 3 months

## checkout for linearity against soil variables

# plot against soil varriable
scatterplotMatrix(~ I(log(no)) + Moist + Temp_Max + Temp_Mean + Temp_Min, diag = "boxplot", df)

# plot for each plot against soil variables
print(xyplot(log(no) ~ Moist | ring + plot, data = df, type = c("r", "p")))
print(xyplot(log(no) ~ Temp_Mean | ring + plot, df, type = c("r", "p")))
# looks fine

## Analysis
Iml_ancv_pc <- lmer(log(no) ~ co2 * (Moist + Temp_Mean) + (1|block) + (1|ring) + (1|id), data = df)

Anova(Iml_ancv_pc)

Fml_ancv_pc <- stepLmer(Iml_ancv_pc)
Anova(Fml_ancv_pc)
Anova(Fml_ancv_pc, test.statistic = "F")

# main effect
plot(allEffects(Fml_ancv_pc))

plot(Fml_ancv_pc)
qqnorm(resid(Fml_ancv_pc))
qqline(resid(Fml_ancv_pc))

# 95% CI for estimated parameters
ciDF <- CIdf(Fml_ancv_pc)
ciDF

###########
# Summary #
###########

## ----Stat_FACE_Extr_Nitrate_PreCO2Smmry
# The starting model is:
Iml_pre$call
Anova(Iml_pre)

# The final model is:
Fml_pre$call
Anova(Fml_pre)

## ----Stat_FACE_Extr_Nitrate_PostCO2Smmry
# The starting model is:
Iml_post@call
Anova(Iml_post)

# The final model is:
Fml_post@call

# Chi-square
Anova(Fml_post)

# F test
AnvF_NO_post

## ---- Stat_FACE_Extr_Nitrate_postCO2_withSoilVarSmmry

# The initial model is:
Iml_ancv@call
Anova(Iml_ancv)

# The final model is:
Fml_ancv@call

# Chisq
Anova(Fml_ancv)

# F-test
AnvF_no

# squared r
rsquared.glmm(Fml_ancv)

# 95% CI for estimated parameter
Est_NO

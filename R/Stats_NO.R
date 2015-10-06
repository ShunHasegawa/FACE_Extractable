## ----Stat_FACE_Extr_Nitrate_PreCO2

range(extr$no)

###########
# Pre-CO2 #
###########

bxplts(value= "no", data= subsetD(extr, pre))
  # log seems slightly better

# different random factor strucures
Iml_pre_no <- lmer(log(no) ~ co2 * time + (1|block/ring/plot), data = subsetD(extr, pre))

# The starting model is:
Iml_pre_no@call
Anova(Iml_pre_no)
Anova(Iml_pre_no, test.statistic = "F")

# model simplification
Fml_pre_no <- stepLmer(Iml_pre_no, alpha.fixed = .1)
Anova(Fml_pre_no)

# The final model is:
Fml_pre_no@call

Anova(Fml_pre_no)

summary(Fml_pre_no)

# model diagnosis
plot(Fml_pre_no)
qqnorm(residuals(Fml_pre_no))
qqline(residuals(Fml_pre_no))

## ----Stat_FACE_Extr_Nitrate_PostCO2

############
# Post-CO2 #
############

bxplts(value= "no", data= subsetD(extr, post))
  # log seems better

# The initial model
Iml_post_no <- lmer(log(no) ~ co2 * time + (1|block)+ (1|ring) + (1|id), 
                 data = subsetD(extr, post))
Anova(Iml_post_no)

# Model simplification
Fml_post_no <- stepLmer(Iml_post_no, alpha.fixed = .1)
Anova(Fml_post_no)
AnvF_NO_post <- Anova(Fml_post_no, test.statistic = "F") 
AnvF_NO_post

summary(Fml_post_no)

# plot(allEffects(Fml_post_no))

# model diagnosis
plot(Fml_post_no)
qqnorm(residuals(Fml_post_no))
qqline(residuals(Fml_post_no))
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
Iml_ancv_no <- m1$Initial
Anova(Iml_ancv_no)

Fml_ancv_no <- stepLmer(Iml_ancv_no)
Anova(Fml_ancv_no)
AnvF_no <- Anova(Fml_ancv_no, test.statistic = "F")
AnvF_no

plot(allEffects(Fml_ancv_no))
plot(Fml_ancv_no)
qqnorm(resid(Fml_ancv_no))
qqline(resid(Fml_ancv_no))

########################
# Confidence intervals #
########################
# confidence interval for estimated parameters
ciDF <- CIdf(model = Fml_ancv_no)
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
Iml_ancv_no_pc <- lmer(log(no) ~ co2 * (Moist + Temp_Mean) + (1|block) + (1|ring) + (1|id), data = df)

Anova(Iml_ancv_no_pc)

Fml_ancv_no_pc <- stepLmer(Iml_ancv_no_pc)
Anova(Fml_ancv_no_pc)
Anova(Fml_ancv_no_pc, test.statistic = "F")

# main effect
plot(allEffects(Fml_ancv_no_pc))

plot(Fml_ancv_no_pc)
qqnorm(resid(Fml_ancv_no_pc))
qqline(resid(Fml_ancv_no_pc))

# 95% CI for estimated parameters
ciDF <- CIdf(Fml_ancv_no_pc)
ciDF

###########
# Summary #
###########

## ----Stat_FACE_Extr_Nitrate_PreCO2Smmry
# The starting model is:
Iml_pre_no@call
Anova(Iml_pre_no)

# The final model is:
Fml_pre_no@call
Anova(Fml_pre_no)

## ----Stat_FACE_Extr_Nitrate_PostCO2Smmry
# The starting model is:
Iml_post_no@call
Anova(Iml_post_no)

# The final model is:
Fml_post_no@call

# Chi-square
Anova(Fml_post_no)

# F test
AnvF_NO_post

## ---- Stat_FACE_Extr_Nitrate_postCO2_withSoilVarSmmry

# The initial model is:
Iml_ancv_no@call
Anova(Iml_ancv_no)

# The final model is:
Fml_ancv_no@call

# Chisq
Anova(Fml_ancv_no)

# F-test
AnvF_no

# squared r
rsquared.glmm(Fml_ancv_no)

# 95% CI for estimated parameter
Est_NO

## ----Stat_FACE_Extr_Phosphate_PreCO2

range(extr$po)

###########
# Pre-CO2 #
###########
bxplts(value= "po", data= subsetD(extr, pre))
  # use row data

# different random factor strucures
m1 <- lme(po ~ co2 * time, random = ~1|ring/plot, data = subsetD(extr, pre))
m2 <- lme(po ~ co2 * time, random = ~1|ring, data = subsetD(extr, pre))
m3 <- lme(po ~ co2 * time, random = ~1|id, data = subsetD(extr, pre))
anova(m1, m2, m3)
  # m2 is better

# autocorelation
atcr.cmpr(m2, rndmFac="ring")$models
  # no need for autocorrelation

Iml_pre <- m2

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

## ---- StatPhosphatePostCO2 ---- 

############
# Post-CO2 #
############

bxplts(value= "po", data= subsetD(extr, post))
  # log seems better

# different random factor strucures
m1 <- lme(log(po) ~ co2 * time, random = ~1|ring/plot, data = subsetD(extr, post))
m2 <- lme(log(po) ~ co2 * time, random = ~1|ring, data = subsetD(extr, post))
m3 <- lme(log(po) ~ co2 * time, random = ~1|id, data = subsetD(extr, post))
anova(m1, m2, m3)
  # m1 is better

# autocorelation
atcr.cmpr(m1, rndmFac="ring/plot")$models
  # no need of autocorrelation

Iml_post <- m1

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
# wedge-shaped
qqnorm(Fml_post, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_post))
qqline(residuals.lm(Fml_post))

## ---- StatPhosphatePreCO2Smmry ---- 
# The starting model is:
Iml_pre$call
xtable(Anova(Iml_pre), floating = FALSE)

# The final model is:
Fml_pre$call
xtable(Anova(Fml_pre), floating = FALSE)

## ---- StatPhosphatePostCO2Smmry ---- 
# The starting model is:
Iml_post$call
xtable(Anova(Iml_post), floating = FALSE)

# The final model is:
Fml_post$call
xtable(Anova(Fml_post), floating = FALSE)

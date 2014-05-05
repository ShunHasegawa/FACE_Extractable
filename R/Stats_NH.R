## ----Stat_FACE_Extr_Ammonium_PreCO2

range(extr$nh)

###########
# Pre-CO2 #
###########
bxplts(value= "nh", data= subsetD(extr, pre))
# row data seems better

# different random factor strucures
m1 <- lme(nh ~ co2 * time, random = ~1|ring/plot, data = subsetD(extr, pre))
m2 <- lme(nh ~ co2 * time, random = ~1|ring, data = subsetD(extr, pre))
m3 <- lme(nh ~ co2 * time, random = ~1|id, data = subsetD(extr, pre))
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
m1 <- lme(sqrt(nh) ~ co2 * time, random = ~1|ring/plot, data = subsetD(extr, post))
m2 <- lme(sqrt(nh) ~ co2 * time, random = ~1|ring, data = subsetD(extr, post))
m3 <- lme(sqrt(nh) ~ co2 * time, random = ~1|id, data = subsetD(extr, post))
anova(m1, m2, m3)
  # m3 is better

# autocorelation
atcr.cmpr(m3, rndmFac="id")$models
  # model 5 looks better

Iml_post <- atcr.cmpr(m2, rndmFac="ring")[[5]]

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

## ----Stat_FACE_Extr_Ammonium_PreCO2Smmry
# The starting model is:
Iml_pre$call
xtable(Anova(Iml_pre), floating = FALSE)

# The final model is:
Fml_pre$call
xtable(Anova(Fml_pre), floating = FALSE)

## ----Stat_FACE_Extr_Ammonium_PostCO2Smmry
# The starting model is:
Iml_post$call
xtable(Anova(Iml_post), floating = FALSE)

# The final model is:
Fml_post$call
xtable(Anova(Fml_post), floating = FALSE)

#  Contrast
print(xtable(FACE_Extr_PostCO2_NH_CntrstDf, floating = FALSE), 
      include.rownames = FALSE)
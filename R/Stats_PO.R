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

## ----Stat_FACE_Extr_Phosphate_PostCO2

############
# Post-CO2 #
############

bxplts(value= "po", data= subsetD(extr, post))
  # log seems better

# different random factor strucures
m1 <- lme(po^(-0.1818) ~ co2 * time, random = ~1|ring/plot, data = subsetD(extr, post))
m2 <- lme(po^(-0.1818) ~ co2 * time, random = ~1|ring, data = subsetD(extr, post))
m3 <- lme(po^(-0.1818) ~ co2 * time, random = ~1|id, data = subsetD(extr, post))
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
qqnorm(Fml_post, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_post))
qqline(residuals.lm(Fml_post))


##########
# Ancova #
##########
# plot against soil varriable
scatterplotMatrix(~ po + log(Moist) + Temp_Max + Temp_Mean + Temp_Min,
                  diag = "boxplot", 
                  subsetD(extr, !pre))

scatterplotMatrix(~ log(po) + log(Moist) + Temp_Max + Temp_Mean + Temp_Min,
                  diag = "boxplot", 
                  subsetD(extr, !pre))

# moisture seems to have a positive effect

# plot for each plot against soil variables
print(xyplot(log(po) ~ log(Moist) | ring + plot, subsetD(extr, !pre), type = c("r", "p")))

# analysis
# Note Temp_Max and log(Moist) appears to be correlated so shouln't be 
# placed in a multiple regression model
Iml_ancv <- lme(log(po) ~ co2 * log(Moist), 
                random = ~1|block/ring/plot,  
                data = subsetD(extr, !pre))
Anova(Iml_ancv)
Fml_ancv <- MdlSmpl(Iml_ancv)$model.reml
Anova(Fml_ancv)
summary(Fml_ancv)

# main effects
plot(allEffects(Fml_ancv))

## plot predicted value
PltPr_Moist(Fml_ancv, trans = exp)


## plot predicted value for each block
# data frame for predicted values from the final model

# data fram for explanatory variables
expDF <- with(extr, expand.grid(ring = unique(ring), 
                               plot = unique(plot),
                               Moist = seq(min(Moist), max(Moist), length.out= 100)))

expDF <- within(expDF, {
  block = recode(ring, "c(1,2) = 'A'; c(3,4) = 'B'; c(5,6) = 'C'")
  co2 = factor(ifelse(ring %in% c(1, 4, 5), "elev", "amb"))
  id = ring:plot
})

# adjust the moisture range according to the actural range 
# for each block
BlkMoist <- function(variable, data){
  a <- range(subset(extr, !pre & block == variable)$Moist)
  df <- subset(data, 
               block == variable & 
               Moist <= a[2] & 
               Moist >= a[1])
  return(df)
}

expDF <- ldply(list("A", "B", "C"), 
               function(x) BlkMoist(variable = x, data = expDF))

# predicted values from the model above
PredDF <- cbind(expDF, predict(Fml_ancv, 
                               level = 0:3, 
                               newdata = expDF))
theme_set(theme_bw())
p <- ggplot(PredDF, aes(x = Moist, y = exp(predict.block), col = co2))
p + geom_line() +
  geom_point(aes(x = Moist, y = po, col = co2), data = subsetD(extr, !pre)) + 
  scale_color_manual("co2", values = c("blue", "red")) +
  facet_grid(.~block)

# model diagnosis
plot(Fml_ancv)
qqnorm(Fml_ancv, ~ resid(.)|id)
qqnorm(residuals.lm(Fml_ancv))
qqline(residuals.lm(Fml_ancv))

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


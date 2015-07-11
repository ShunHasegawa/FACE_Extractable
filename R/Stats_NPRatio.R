## ----Stat_FACE_Extr_NPRatio_PostCO2

#############
# N:P ratio #
#############
# add NP ratio
extr$NP <- with(extr, (no + nh)/po)

# parcent change
extr$SumN <- with(extr, no + nh) # total of nitrate and ammonium
extr <- ddply(extr, .(ring, plot, co2, block, id), 
              function(x) {
                d  <- x[order(x$date), ]
                df <- within(d, {
                  pcN = exp(Delt(SumN))
                  pcP = exp(Delt(po))
                  pcNP = pcN/pcP
                })
                return(df)
              })

## ---- Stat_FACE_Extr_Analyse_NP
######
# NP #
######

bxplts(value= "NP", data= subsetD(extr, post))
# log seems better

# The initial model
Iml_post_NP <- lmer(log(NP) ~ co2 * time + (1|block)+ (1|ring) + (1|id), 
                 data = subsetD(extr, post))
Anova(Iml_post_NP, test.statistic = "F")
 # significant co2 x time interaction

## ---- Stat_FACE_Extr_Analyse_NP_plot
# model diagnosis
plot(Iml_post_NP)
qqnorm(residuals(Iml_post_NP))
qqline(residuals(Iml_post_NP))
  # one obvious outlier

## ---- Stat_FACE_Extr_Analyse_NP_plot2
# Remove the outlier and re-run
a <- which(qqnorm(residuals(Iml_post_NP), plot.it = FALSE)$y == min(qqnorm(residuals(Iml_post_NP), plot.it = FALSE)$y))
Iml_post_NP2 <- lmer(log(NP) ~ co2 * time + (1|block)+ (1|ring) + (1|id), 
                    data = subsetD(extr, post), subset = -a)
plot(Iml_post_NP2)
qqnorm(residuals(Iml_post_NP2))
qqline(residuals(Iml_post_NP2))
# improved
Anova(Iml_post_NP2, test.statistic = "F")
# consistent results  as above

## ---- Stat_FACE_Extr_Analyse_NP_plot3
plot(allEffects(Iml_post_NP2))

## ---- Stat_FACE_Extr_Analyse_NP_Figure

#geometric mean
gm_mean = function(x, na.rm=TRUE){
  exp(sum(log(x[x > 0]), na.rm=na.rm) / length(x))
}
NP_ring <- ddply(extr, .(date, block, co2, ring), summarise, Rgeo = gm_mean(NP))
NP_co2 <- ddply(NP_ring, .(date, co2), summarise, Rgeo = gm_mean(Rgeo))
plot(log(Rgeo) ~ date, data = NP_co2, pch = 19, cex = 2, col = co2)

# ratio for each block
NP_co2_Raio_block <- ddply(NP_ring, .(date, block), 
                           summarise, Rgeo = Rgeo[co2 == "elev"]/Rgeo[co2 == "amb"])

# geometric mean for co2 treatment
NP_co2_Raio <- ddply(NP_co2_Raio_block, .(date), summarise, Rgeo = gm_mean(Rgeo))
# confidence interval
geoCI <- function(x) exp(ci(log(x))[c(2, 3, 4)])
Rio_CI <- ddply(NP_co2_Raio_block, .(date), function(x) {
  data.frame(lci = geoCI(x$Rgeo)[1], uci = geoCI(x$Rgeo)[2], SE = geoCI(x$Rgeo)[3])
})


dd <- merge(NP_co2_Raio, Rio_CI)
plot(log(Rgeo) ~ date, data = dd, pch = 19, cex = 2, ylim = c(-.6, .45))
with(dd, arrows(date, log(Rgeo) - log(SE), date, log(Rgeo) + log(SE), length = .1, angle = 90, code = 3))
abline(h = 0, lty = 2, col = "red")
abline(v = as.numeric(as.Date("2012-9-18")), lty = 2)
preMean <- mean(log(dd$Rgeo[dd$date < as.Date("2012-9-18")]))
abline(h = preMean, lty = 4, lwd = 2)


load("Output/Data/extr_tmp.RData")
head(extr)
temp_M <- ddply(extr, .(date), summarise, Temp_Mean = mean(Temp_Mean, na.rm = TRUE))
head(postDF)
ddt <- merge(dd, temp_M, by = "date")
plot(log(Rgeo) ~ Temp_Mean, data = ddt)
abline(coef(mml <- lm(log(Rgeo) ~ Temp_Mean , data = ddt)))



## ---- Stat_FACE_Extr_Analyse_pcNP
##############################
# NP ratio of Percent change #
##############################
bxplts(value= "pcNP", data= subsetD(extr, post))

# The initial model is
Iml_post_pcNP <- lmer(log(pcNP) ~ co2 * time + (1|block) + (1|ring)  + (1|id),
                      data = subsetD(extr, post))
Anova(Iml_post_pcNP, test.statistic = "F")

## ---- Stat_FACE_Extr_Analyse_pcNP_plot
# model diagnosis
plot(Iml_post_pcNP)
qqnorm(resid(Iml_post_pcNP))
qqline(resid(Iml_post_pcNP))
# one obvious outlier so remove and re-run

## ---- Stat_FACE_Extr_Analyse_pcNP_plot2
a <- which(qqnorm(resid(Iml_post_pcNP), plot.it = FALSE)$y == max(qqnorm(resid(Iml_post_pcNP), plot.it = FALSE)$y))
Iml_post_pcNP2 <- lmer(log(pcNP) ~ co2 * time + (1|block) + (1|ring)  + (1|id),
                      data = subsetD(extr, post), subset = -a)
plot(Iml_post_pcNP2)
qqnorm(resid(Iml_post_pcNP2))
qqline(resid(Iml_post_pcNP2))
# improved
Anova(Iml_post_pcNP2, test.statistic = "F")

## ---- Stat_FACE_Extr_Analyse_pcNP_plot3
plot(allEffects(Iml_post_pcNP2))


## ---- Stat_FACE_Extr_NPRatio_ANCOVA
#########
# ANCOV #
#########

############
# NP ratio #
############
postDF <- subsetD(extr, !pre)

scatterplotMatrix(~ NP + Moist + Temp_Mean, data = postDF, diag = "boxplot")
xyplot(NP ~ Moist|ring, group = id, postDF, type = c("r", "p"))
xyplot(NP ~ Temp_Mean|ring, group  = id, postDF, type = c("r", "p"))

Iml_ancv_NP <- lmer(NP ~ co2 * (Moist + Temp_Mean) + (1|block) + (1|ring) + (1|id), 
                    data = postDF)
Anova(Iml_ancv_NP, test.statistic = "F")
Fml__ancv_NP <- stepLmer(Iml_ancv_NP)
Anova(Fml__ancv_NP, test.statistic = "F")
plot(allEffects(Fml__ancv_NP))

## ---- Stat_FACE_Extr_NPRatio_ANCOVA_plot
plot(Fml__ancv_NP)
qqnorm(resid(Fml__ancv_NP))
qqline(resid(Fml__ancv_NP))

## ---- Stat_FACE_Extr_pcNP_ANCOVA
############
# % change #
############
scatterplotMatrix(~ pcNP + Moist + Temp_Mean, data = postDF, diag = "boxplot")
# one obvious outlier
a <- which(postDF$pcNP == max(postDF$pcNP))
scatterplotMatrix(~ pcNP + Moist + Temp_Mean, data = postDF[-a, ], diag = "boxplot")
scatterplotMatrix(~ log(pcNP) + Moist + Temp_Mean, data = postDF[-a, ], diag = "boxplot")
xyplot(log(pcNP) ~ Moist|ring, group = id, postDF[-a, ], type = c("r", "p"))
xyplot(log(pcNP) ~ Temp_Mean|ring, group  = id, postDF[-a, ], type = c("r", "p"))

Iml_ancv_pcNP <- lmer(log(pcNP) ~ co2 * (Moist + Temp_Mean) + (1|block) + (1|ring) + (1|id), 
                      data = postDF, subset = -a)
Anova(Iml_ancv_pcNP, test.statistic = "F")
Fml__ancv_pcNP <- stepLmer(Iml_ancv_pcNP)
Anova(Fml__ancv_pcNP, test.statistic = "F")
plot(allEffects(Fml__ancv_pcNP))

## ---- Stat_FACE_Extr_pcNP_ANCOVA_plot
plot(Fml__ancv_pcNP)
qqnorm(resid(Fml__ancv_pcNP))
qqline(resid(Fml__ancv_pcNP))

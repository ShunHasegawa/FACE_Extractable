###############################################
# Run lmer for each data frame and return AIC #
###############################################
test <- LmrAicComp(ListDF = ListDF,
                   formula = formula(log(po) ~ co2 * (log(Moist) + Temp_Mean) + 
                     (1|block) + (1|ring) + (1|id)))

test <- LmrAicComp(ListDF = LstDF[1:30],
                   formula = formula(log(po) ~ co2 * (Moist + Temp_Mean) + 
                     (1|block) + (1|ring) + (1|id)))

test <- LmrAicComp(ListDF = LstDF[1:30],
                   formula = formula(log(no) ~ co2 * (log(Moist) + Temp_Mean) + 
                     (1|block) + (1|ring) + (1|id)))
Iml <- test[[1]]
Fml <- test[[2]]
Anova(Fml, test.statistic = "F")
plot(Fml)



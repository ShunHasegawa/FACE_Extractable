require(plyr)
require(lme4)

source("rsquaredglmm.R")
load("ListDF.RData") # load list of data frames

#########
# Try 1 #
#########

# Create list of models
LstLmrs <- llply(ListDF, 
                 function(x) lmer(log(po) ~ co2 * (log(Moist) + Temp_Mean) + 
                                    (1|block) + (1|ring) + (1|id), data = x))

# compute square r for each model
R2DF <- rsquared.glmm(LstLmrs)
  # This give an error message sayiig that Error: 'data' not found, and some
  # variables missing from formula environment.

LstLmrs[[1]]
  # Data: x. This is probably causing the above problem 

#########
# Try 2 #
#########

# Here I run lmer for each data frame in the list and then try to obtain r^2
# wihtin the function

R2Fun <- function(data) {
  ml <- lmer(log(po) ~ co2 * (log(Moist) + Temp_Mean) + 
               (1|block) + (1|ring) + (1|id), data = data)
  r2 <- rsquared.glmm(ml)
  return(r2)
}

LstLmrs <- llply(ListDF, R2Fun)
  # It gives the different error message: Error in list2env(data) : first 
  # argument must be a named list. Probably environment is the problem (?) as if
  # I simpliy run this individually thi works fine as below. 
  # environment(rsquared.glmm) is set at the global environment. I tried to set 
  # environment by myself as shown https://github.com/lme4/lme4/issues/177, but 
  # it doesn't work..

model <- lmer(log(po) ~ co2 * (log(Moist) + Temp_Mean) + 
                (1|block) + (1|ring) + (1|id), data = ListDF[[1]])
r2 <- rsquared.glmm(model)
r2

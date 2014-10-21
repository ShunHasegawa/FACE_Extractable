require(plyr)
source("AskRemko/rsquaredglmm.R")

load(ListDF) # load list of data frames

# Here I run lmer for each data frame in the list and try to obtain r^2 for each model
LstLmrs <- llply(ListDF, 
                 function(x) {
                   df <- x
                   ml <- lmer(log(po) ~ co2 * (log(Moist) + Temp_Mean) + 
                                (1|block) + (1|ring) + (1|id), data = df)
                   r2 <- rsquared.glmm(ml)
                   return(r2)
                   },
                 .progress = "text")

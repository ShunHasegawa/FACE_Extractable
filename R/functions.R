#######################
#model simplification #
#######################
MdlSmpl <- function(model){
  mod2 <- update(model, method = "ML") #change method from REML to ML
  stai <- stepAIC(mod2, trace = FALSE) #model simplification by AIC
  dr <- drop1(stai, test="Chisq") #test if removing a factor even more significantly lowers model
  model <- update(stai, method="REML")
  ifelse(all(dr[[4]] < 0.05, na.rm=TRUE), anr <- Anova(model), anr<-NA)  # type II
  #dr[[4]]<0.05-->unable to remove any more factors so finlize the results by changsing the method back to REML
  return(list(step.aic = stai$anova, drop1 = dr, anova.reml = anr, model.reml = model, model.ml = stai))
}

##########################
# Create a summary table #
##########################
CreateTable <- function(dataset, fac, ...){
  a <- dataset[c("date", fac, "value")] #extract required columns
  colnames(a) <- c("date","variable","value") #change column names for cast
  means <- cast(a, date~variable, mean, na.rm = TRUE) 
  ses <- cast(a,date~variable,function(x) ci(x,na.rm=TRUE)[4])
  colnames(ses)[2:ncol(ses)] <- paste(colnames(ses)[2:ncol(ses)],"SE",sep=".")
  samples <- cast(a,date~variable,function(x) sum(!is.na(x))) #sample size
  colnames(samples)[2:ncol(samples)] <- paste(colnames(samples)[2:ncol(samples)],"N",sep=".")
  mer <- Reduce(function(...) merge(..., by = "date"), list(means, ses, samples)) #merge datasets
  mer <- mer[,c(1, order(names(mer)[-grep("date|N", names(mer))])+1, grep("N", names(mer)))] #re-order columns
  mer$date <- as.character(mer$date) # date is turned into character for knitr output 
  return(format(mer, ...))
}

# creates excel worksheets
crSheet <- function(sheetname, dataset){
  #create sheet
  sheet <- createSheet(wb, sheetName = sheetname)
  
  #add data to the sheet
  addDataFrame(dataset, sheet, showNA = TRUE, row.names = FALSE, startRow = 2)
  
  #title of the sheet
  addDataFrame(t(c(sheetname, "unit=mg DrySoil kg^(-1)")), sheet, startRow = 1, row.names = FALSE, col.names = FALSE)
}

############################
# make a summary dataframe #
############################
Crt_SmryDF <- function(data, val = "value"){
  x <- data[ ,val]
  Mean <- mean(x, na.rm = TRUE)
  SE <- ci(x, na.rm = TRUE)[[4]]
  N  <- sum(!is.na(x))
  data.frame(Mean, SE, N)
}

####################
# plot mean and se #
####################
PltMean <- function(data, ...){
  
  vars <- c(substitute(NO[3]^"-"-N), 
            substitute(NH[4]^"+"-N),
            substitute(PO[4]^"3-"-P))
  # subsitute returens argument as it is without calculation (similar to expression())
  
  yvars <- lapply(vars, function(x) bquote(Soil-extractable~.(x)))
  # bquote allows one to call an object and return expression
  
  ylabs <- lapply(yvars, function(x) {
    c(expression(), 
      bquote(atop(paste(.(x)), paste((mg~DW_kg^"-1")))))         
  })
  # atop: put the 1st argument on top of the 2nd
  
  # create ylab according to variable
  ntrs <- c("no", "nh", "po")
  
  # when plotting multiple variables at the same time
  if(length(unique(data$variable)) > 1) 
    ylab <- expression(Soil-extractable~nutrient~(mg~DW_kg^"-1")) else {
    # only one variable
    for (i in 1:3){
      if(unique(data$variable) == ntrs[i]) ylab  <- ylabs[[i]]
    }
  }
  
  p <- ggplot(data, aes_string(x = "date", y = "Mean", ...))
  
  p2 <- p + geom_line(size = 1) + 
    geom_errorbar(aes_string(ymin = "Mean - SE", ymax = "Mean + SE", ...), width = 5) + 
    labs(x = "Time", y = ylab) +
    geom_vline(xintercept = as.numeric(as.Date("2012-09-18")), 
               linetype = "dashed", 
               col = "black") +
    scale_x_date(breaks= date_breaks("2 month"),
                 labels = date_format("%b-%y"),
                 limits = as.Date(c("2012-7-1", "2014-4-2"))) +
    theme(axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1))
}

##################
# Plot ring mean #
##################
PltRnghMean <- function(data){
  # change factor level names for labelling
  p <- PltMean(data, col = "ring", linetype = "co2") +
    scale_color_manual(values = palette(), "Ring", 
                       labels = paste("Ring", c(1:6), sep = "_")) +
    scale_linetype_manual(values = c("dashed", "solid"),
                          expression(CO[2]~trt),
                          labels = c("Ambient", expression(eCO[2])))
  
    return(p)
}

######################
# Plot temp trt mean #
######################
PltCO2Mean <- function(data){
  p <- PltMean(data, col = "co2") +
    scale_color_manual(values = c("blue", "red"), 
                       expression(CO[2]~trt),
                       labels = c("Ambient", expression(eCO[2])))
  
  # add asterisk on NH graphs at co3 treatments
  if(!any(unique(data$variable) == "nh")) p else{
    newDF <- subset(data, time %in% c(3, 7) & variable == "nh") # the times and variable where "*" is placed
    ant_pos <- ddply(newDF, .(date, variable), summarise, Mean = max(Mean + SE)) #y position of "*"
    ant_pos$lab <- "*"
    ant_pos$temp <- factor("amb", levels=c("amb", "elve")) 
    # the original data frame uses "temp", so it needs to have "temp" as well in ggplot2
    # but it doesn't really do anything    
    p +  geom_text(data = ant_pos, aes(x =date, y = Mean, label= lab), col = "black", vjust = 0)
  }
}

##############################
# Save ggplot in PDF and PNG #
##############################
ggsavePP <- function(filename, plot, width, height){
  ggsave(filename = paste(filename, ".pdf", sep = ""), 
         plot = plot, 
         width = width, 
         height = height)
  
  ggsave(filename = paste(filename, ".png", sep = ""), 
         plot = plot, 
         width = width, 
         height = height, 
         dpi = 600)
}

#############################################
# compare different auto-correlation models #
#############################################
atcr.cmpr <- function(model){
  model2 <- update(model,corr=corCompSymm(form = model$call$random))
  model3 <- update(model,correlation=corARMA(q = 2))
  model4 <- update(model,correlation=corAR1()) 
  model5 <- update(model,correlation=corARMA(q = 1))
  a <- anova(model, model2, model3, model4, model5)
  rownames(a) <- c("NULL", "corCompSymm", "corARMA(q=2)", "corAR1()", "corARMA(q=1)")
  models <- list(model, model2, model3, model4, model5, 'models' = a)
  return(models)
}

##############################################
# Compare different random factor structures #
##############################################
RndmComp <- function(model){
  m2 <- update(model, random = ~ 1|block/ring)
  m3 <- update(model, random = ~ 1|block/id)
  m4 <- update(model, random = ~ 1|ring/plot)
  m5 <- update(model, random = ~ 1|ring)
  m6 <- update(model, random = ~ 1|id)
  ms <- list(model, m2, m3, m4, m5, m6)
  a <- anova(model, m2, m3, m4, m5, m6)
  rownames(a) <- sapply(ms, function(x) as.character(x$call$random[2]))
  ms[[length(ms) + 1]] <- a
  names(ms)[length(ms)] <- 'anova'
  return(ms)
}

###########################
# step deletion with lmer #
###########################
stepLmer <- function(model, red.rndm = FALSE, ddf = "Kenward-Roger", ...){
  require(lmerTest)
  update(step(model, reduce.random = red.rndm, ddf = ddf,...)$model, 
         contrasts = NULL)
}
# use "Kenward-Roger" for approximation for denominator degrees of freedom. This
# is the same as the default DF given by Anova(model, test.statistic = "F). The
# default of step gives me a warning message for IEM-NO3 for some reasons (not
# sure why.. so changed it.)

########################################################
# confidence interval for estimated parameters by lmer #
########################################################
CIdf <- function(model, method = "boot"){
  CIs <- confint(model, method = method)
  CIs <- CIs[-grep("sd|sigma", row.names(CIs)), ] 
  # take out estimates for fixed factors
  coefs <- summary(model)$coefficients
  ciDF <- cbind(CIs, Estimate = coefs[, "Estimate"])
  return(ciDF)
}  


###########################################
# produce box plots with transformed data #
###########################################
# log OR sqrt OR power(1/3) OR inverse OR box-cox
bxplts <- function(value, ofst = 0, data, ...){
  data$y <- data[[value]] + ofst #ofst is added to make y >0
  a <- boxcox(y ~ co2 * time, data = data)
  par(mfrow = c(2, 3))
  boxplot(y ~ co2*time, data, main = "row")
  boxplot(log(y) ~ co2*time, main = "log", data)
  boxplot(sqrt(y) ~ co2*time, main = "sqrt", data)
  boxplot(y^(1/3) ~ co2*time, main = "power(1/3)", data)
  boxplot(1/y ~ co2*time, main = "inverse", data)
  BCmax <- a$x[a$y == max(a$y)]
  texcol <- ifelse(BCmax < 0, "red", "black") 
  boxplot(y^(BCmax) ~ co2*time, 
          main = "", sep = "=", 
          data = data)
  title(main = paste("Box Cox", round(BCmax, 4)), 
        col.main = texcol)
  par(mfrow = c(1,1))
}

# multiple box-cox power plot for different constant values
bxcxplts <- function(value, data, sval, fval){
  data$yval <- data[[value]]
  ranges <- seq(sval, fval, (fval - sval)/9)
  
  # store parameters given from box-cox plot
  par(mfrow = c(5, 2))
  BCmax <- vector()
  for (i in 1:10){
    data$y <- data$yval + ranges[i]
    a <- boxcox(y ~ co2 * time, data = data)
    BCmax[i] <- a$x[a$y == max(a$y)]
  }
  
  # plot box plot with poer given from box-box for 
  # each contstant value
  par(mfrow = c(5, 2))
  par(omi = c(0, 0, 0, 0), mai = c(0.4, 0.4, 0.4, 0))
  sapply(1:10, function(x) {
    boxplot((yval + ranges[x]) ^ BCmax[x] ~ co2 * time, 
            main = "", data = data)
    texcol <- ifelse(BCmax[x] < 0, "red", "black") 
    title(main = paste("constant=", round(ranges[x], 4), 
                       ", boxcox=", round(BCmax[x], 4)),
          col.main = texcol)
  })
  par(mfrow = c(1,1))
}


####################################
# create table of contrast results #
####################################
cntrstTbl <- function(cntrstRes, data, ...){
  d <- unique(data$date)
  ds <- format(d, format = "%b-%Y")
  
  Df <- data.frame(
    date = ds,
    contrast  =  cntrst$Contrast,
    SE = cntrst$SE,
    t = cntrst$testStat,
    df = cntrst$df,
    P.value = cntrst$Pvalue)
  return(format(Df, ...))
}


###############
# Print table #
###############
printTbl <- function(tbl, caption, label, ...){
  print(xtable(tbl,
               caption = caption, 
               label = label, 
               align = rep("l", ncol(tbl) + 1)),
        caption.placement = "top", 
        include.rownames = FALSE,
        table.placement = "H", ...)
}

printRngTbl <- function(tbl, caption, label, ...){
  printTbl(tbl[, 1:13], 
           caption = caption,
           label = label,
           ...)
  printTbl(tbl[, c(1, 14:19)], 
           caption = NULL,
           label = NULL,
           ...)
}

##############################
# subset data and droplevels #
##############################
subsetD <- function(data,...){
  droplevels(subset(data, ...))
}

##################
# percent change #
##################
PerChange <- function(data){
  d  <- data[order(data$date), ]
  df <- within(d, {
    pcNO = Delt(d$no, type = "arithmetic")
    pcNH = Delt(d$nh, type = "arithmetic")
    pcP = Delt(d$po, type = "arithmetic")
  })
  return(df)
}

##################################################
# compute mean of soil variable for given period #
##################################################
SoilPeriodMean <- function(data, rings, plots, Start, End){
  sDF <- subset(data, Date >= Start & Date <= End & ring == rings & plot == plots)
  colMeans(sDF[c("Moist", "Temp_Mean", "Temp_Min", "Temp_Max")], na.rm = TRUE)
}

#########################
# plot predicted values #
#########################
# with vesreg function
Visreg_Moist <- function(model, ..., orginalData){
  visreg(model, 
         xvar = "Moist",
         by = "co2", 
         level = 1, # take random factor into accound
         overlay = TRUE, 
         print.cond=TRUE, 
         line.par = list(col = c("blue", "red")),
         points.par = list(col = c("blue", "red")), 
         ...)
  
  times <- unique(orginalData$time[!orginalData$pre])
  timePos <- seq(1, 3, length.out = length(times))
  
  for (i in 1:length(times)){
    lines(x = range(orginalData$Moist[orginalData$time == times[i]]), y = rep(timePos[i], 2), lwd = 2)
    text(x = mean(range(orginalData$Moist[orginalData$time == times[i]])), y = timePos[i], 
         labels = paste("Time =", times[i]), pos = 3)
  }
  legend("topright", lty =1, leg = "Moist range", bty = "n")
}

####################################
# data frame with predicted values #
####################################

# function to adjust the moisture range according to the 
# actural range for each block
BlkMoist <- function(variable, data){
  a <- range(subset(extr, !pre & block == variable)$Moist)
  df <- subset(data, 
               block == variable & 
                 Moist <= a[2] & 
                 Moist >= a[1])
  return(df)
}

PredVal <- function(data, model){
  # data frame for predicted values from the final model
  
  # data fram for explanatory variables
  expDF <- with(data, expand.grid(
    ring = unique(ring), 
    plot = unique(plot),
    Moist = seq(min(Moist), 
    max(Moist), 
    length.out= 100)))
  
  expDF <- within(expDF, {
    block = recode(ring, "c(1,2) = 'A'; c(3,4) = 'B'; c(5,6) = 'C'")
    co2 = factor(ifelse(ring %in% c(1, 4, 5), "elev", "amb"))
    id = ring:plot
  })
  
  expDF <- ldply(list("A", "B", "C"), 
                 function(x) BlkMoist(variable = x, data = expDF))
  
  # predicted values from the model above
  PredDF <- cbind(expDF, predict(model, 
                                 level = 0:3, 
                                 newdata = expDF))
  return(PredDF)
}

###############################################
# Plot soil variable for each incubation time #
###############################################
PltSoilVar <- function(data, var, tdrData, backdates = 3 * 4 * 7, linealpha = .5){
  # backdates: How many days back the soil variable means are calculated (3*4*7
  # = 3 months)
  df <- ddply(data, c("date", var),
              function(x) colMeans(x[c("Moist", "Temp_Mean", "Temp_Min", "Temp_Max")],
                                   na.rm = TRUE))
  df <- within(df, {
    strtDay <- date - backdates
    middleDay <- as.Date(rowMeans(cbind(as.numeric(date), as.numeric(strtDay)), na.rm = TRUE), origin)
  })
  
  SoilVarMlt <- melt(df, id = c(var, "date", "strtDay", "middleDay"))
  SoilVarMlt <- within(SoilVarMlt, {
    type <- factor(ifelse(variable != "Moist", "Temp", "Moist"))
  })
  
  p <- ggplot(SoilVarMlt, aes_string(x = "middleDay", y = "value", shape = "variable", col = var))
  pl <- p + geom_point() +
    facet_grid(type ~., scale = "free_y") +
    labs(x = "Time", y = NULL) +
    geom_vline(xintercept = as.numeric(as.Date("2012-09-18")), linetype = "dashed", col = "black") +
    scale_x_date(breaks= date_breaks("2 month"),
                 labels = date_format("%b-%y"),
                 limits = as.Date(c("2012-7-1", "2014-4-2"))) +
    theme(axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1)) +
    geom_line(aes_string(x = "Date", y = "value", group = var), data = tdrData, alpha = linealpha) +
    geom_vline(xintercept = c(unique(as.numeric(SoilVarMlt$strtDay)), max(as.numeric(SoilVarMlt$date))),
               col = "gray30", size = .5,linetype = "dotted")
  pl
}


##############################################
## Plot raw data and incubation-period mean ##
##############################################

PltSoilVar_Period <- function(data, lab){
  ## co2 ##
  pl <- PltSoilVar(data = data, var = "co2", tdrData = soilTDR_co2Mean, linealpha = .5) +
    scale_color_manual(values = c("blue", "red"), expression(CO[2]~trt), 
                       labels = c("Ambient", expression(eCO[2]))) +
    ggtitle(paste(lab, "Mean soil moisture and temperature"))
  ggsavePP(filename = paste("output//figs/FACE_Extractable_SoilVarMonth_CO2", lab, sep = "_"), 
           plot = pl, width = 6, height = 4)
  
  ## ring ##
  pl <- PltSoilVar(data = data, var = "ring", tdrData = soilTDR_RngMean, linealpha = .3) +
    scale_color_manual(values = palette(), "Ring", labels = paste("Ring", c(1:6), sep = "_")) +
    ggtitle(paste(lab, "Mean soil moisture and temperature"))
  
  ggsavePP(filename = paste("output//figs/FACE_Extractable_SoilVarMonth_Ring", lab, sep = "_"),
           plot = pl, width = 6, height = 4)
  
  ############################
  # Plot Moist against  Temp #
  ############################
  
  p <- ggplot(postDF, aes(x = Temp_Mean, y = log(Moist), col = ring))
  p2 <- p + geom_point(alpha = .5) 
  
  pl  <- p2 + facet_wrap( ~ ring)
  ggsavePP(file = paste("output/figs/FACE_Extractable_SoilVar_Ring", lab, sep = "_"), 
           plot = pl, width = 6, height = 6)
  
  ggsavePP(file = paste("output/figs/FACE_Extractable_SoilVar", lab, sep = "_"), plot = p2, width = 6, height = 6)
}


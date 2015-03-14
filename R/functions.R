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
  
  # CO2 effects
  if(fac == "co2"){
    # Response ratio (e/a-1) cluculated for each block
    blockR <- ddply(dataset, .(date, block), summarise, R = value[co2 == "elev"]/value[co2 == "amb"]-1)
    # remove Inf
    blockR$R[is.infinite(blockR$R)] <- NA
    # mean of Ratio for each date
    blockRMean <- ddply(blockR, .(date), summarise, Ratio = mean(R, na.rm = TRUE))
    
    # merge datasets
    mer <- Reduce(function(...) merge(..., by = "date"), list(means, ses, samples, blockRMean)) 
  } else { # no need calculate Ratio for Ring summary table
    mer <- Reduce(function(...) merge(..., by = "date"), list(means, ses, samples)) 
  }
  
  # re-order columns
  mer <- mer[,c(1, 
                order(names(mer)[-grep("date|N|Ratio", names(mer))])+1, 
                grep("N|Ratio", names(mer)))]
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

#######################
# Fig for publication #
#######################
# define graphic background
science_theme <- theme(panel.border = element_rect(color = "black"),
                       panel.grid.major = element_blank(),
                       panel.grid.minor = element_blank(),
                       axis.text.x  = element_text(angle=45, vjust= 1, hjust = 1),
                       axis.ticks.length = unit(-.2, "lines"),
                       axis.ticks.margin = unit(.5, "lines"),
                       legend.position = c(.4, .93), 
                       legend.title = element_blank(),
                       legend.key.width = unit(2, "lines"),
                       legend.key = element_blank())

# white-black figure
WBFig <- function(data, ylab, facetLab = ylab_label, figTheme = science_theme,
                  StatRes, StatY){
  # StatRes is stats tables to put on the figs; StatY is y coordinate for that tables
    
  # df for sub labels
  subLabDF <- with(data, 
                   data.frame(xv = as.Date("2012-6-21"),
                              ddply(data, .(variable), summarise, yv = max(Mean + SE)),
                              labels = paste("(", letters[1:length(levels(variable))], ")",
                                             sep = ""),
                              co2 = "amb"))
    # co2 is required as group = co2 is used in the main plot mapping
  
  # df for stat table
  ## compute ylength for each variable
  ylengthDF <- ddply(data, 
                  .(variable), 
                  function(x) 
                    data.frame(ylength = max(x$Mean +x$SE, na.rm = TRUE) -
                                 min(x$Mean - x$SE, na.rm = TRUE)))
    # ylength is given as the difference between max and min
  
  ## create df
  statDF <- StatPositionDF(StatRes = StatRes, 
                           variable = levels(ylengthDF$variable), 
                           ytop = StatY,
                           ylength = ylengthDF$ylength)
    
  # create a plot
  p <- ggplot(data, aes(x = date, y = Mean, group = co2))
  
  p2 <- p + geom_line(aes(linetype = co2),
                      position = position_dodge(20)) + 
    geom_errorbar(aes(ymin = Mean - SE, ymax = Mean + SE), 
                  width = 15, size = .3,
                  position = position_dodge(20)) + 
    geom_point(aes(shape = co2, fill = co2), position = position_dodge(20)) +
    labs(x = "Month", y = ylab) +
    geom_vline(xintercept = as.numeric(as.Date("2012-09-18")), 
               linetype = "dashed", col = "black") +
    scale_x_date(breaks= date_breaks("3 month"),
                 labels = date_format("%b-%y"),
                 limits = as.Date(c("2012-6-15", "2014-4-2"))) +
    scale_shape_manual(values = c(24, 21), labels = c("Ambient", expression(eCO[2]))) +
    scale_fill_manual(values = c("black", "white"), 
                      labels = c("Ambient", expression(eCO[2]))) +
    scale_linetype_manual(values = c("solid", "dashed"), 
                          labels = c("Ambient", expression(eCO[2]))) +
    geom_text(aes(x = xv, y = yv * .95, label = labels),
              fontface = "bold",
              hjust = 1,
              data = subLabDF) +
    facet_grid(variable~., scales= "free_y", labeller= facetLab) +
    figTheme +
    geom_text(data = subset(statDF, predictor != ""), 
              aes(x = as.Date("2014-1-20"), y = yval, label = predictor),
              size = 2, hjust = 1, parse = TRUE) +
    # unless remove [" "] with predictor != "", labels will be messed up due to
    # this empty level
    geom_text(data = statDF, 
              aes(x = as.Date("2014-2-20"), y = yval, label = p), 
              size = 2, parse = TRUE)
  return(p2)
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
  update(step(model, reduce.random = red.rndm, ddf = ddf,...)$model, 
         contrasts = NULL)
}
# require(lmerTest)
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
  boxplot(y ~ co2*time, data, main = "raw")
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
cntrstTbl <- function(cntrstRes, data, variable, ...){
  d <- unique(data[, c("date", "time")])
  Df <- data.frame(
    d,
    contrast  =  cntrstRes$Contrast,
    SE = cntrstRes$SE,
    t = cntrstRes$testStat,
    df = cntrstRes$df,
    P.value = cntrstRes$Pvalue,
    FormatPval(cntrstRes$Pvalue),
    variable = variable)
  return(Df)
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

# Apply the above function to data frame and merge
SoilVarPeriMean <- function(data, period, SoilData){ 
  # period = number of days to back from sampling date to get average soil vars
  df <- ddply(data, .(date, ring, plot),
              function(x) SoilPeriodMean(
                data = SoilData, 
                Start = x$date - period,
                End = x$date, 
                rings = x$ring, 
                plot = x$plot))
  merge(data, df, by = c("date", "ring", "plot"))
}

###############################################
# Run lmer for each data frame and return AIC #
###############################################
LmrAicComp <- function(ListDF, formula){
  # lmer test for each data set
  LstLmrs <- llply(ListDF, 
                   function(x) lmer(formula, data = x),
                   .progress = "text")
  names(LstLmrs) <- names(ListDF)
  
  # plot AIC
  aicDF <- ldply(LstLmrs, AIC)
  names(aicDF) <- c("period", "AICs")
  plot(AICs ~ period, data = aicDF, xlab = "N of Days back from sampling")
  
  
  # lmer for the lowest aic
  df <- ListDF[[which(aicDF$AICs == min(aicDF$AICs))]]
  Iml <- lmer(formula, data = df)
  return(list("Initial" = Iml, "Data" = df, "AICdf" = aicDF))
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

#############################################
# Reshape estimated value and creat a table #
#############################################
ANCV_Tbl <- function(df, digits = 2, nsmall = 2){
  Est.val <- within(data.frame(df), {
    pred <- row.names(Est.val)
    co2 <- factor(ifelse(grepl("elev", pred), "elev", "amb"))
    predictor <- factor(ifelse(grepl("Temp", pred), "Temp",
                               ifelse(grepl("Moist", pred), "Moist", 
                                      "co2")))
  })
  names(Est.val)[c(1,2)] <- c("bCI", "tCI") 
  
  Est.val.mlt <- melt(Est.val, id = c("co2", "pred", "predictor"))
  Est.val.Cst <- cast(Est.val.mlt, predictor + co2~ variable)
  # format disimal numbers for each predictor
  formatDF <- ddply(Est.val.Cst, .(predictor), 
                    function(x) format(x, digits = digits, nsmall = nsmall))
  # concatenate CI and estimated values
  formatDF$val <- apply(formatDF, 1, function(x) 
    paste(x["Estimate"], "(", x["bCI"], ", ",x["tCI"], ")", sep = ""))
  
  tbl <- cast(formatDF, predictor ~ co2, value =  "val")
  return(tbl)
}

#######################################
# Excel sheet for Summary Stats table #
#######################################

CrSheetAnvTbl <- function(workbook, sheetName, smmaryLst){
  sheet <- createSheet(workbook, sheetName = sheetName)
  addDataFrame(data.frame("ANOVA_F"), sheet, col.names = FALSE, row.names = FALSE)
  addDataFrame(smmaryLst[[sheetName]][[1]], sheet, showNA = FALSE, 
               row.names = TRUE, characterNA="NA",
               startRow = 2)
  addDataFrame(data.frame("Coef"), sheet, 
               col.names = FALSE, row.names = FALSE,
               startRow = 10)
  addDataFrame(smmaryLst[[sheetName]][[2]], sheet, showNA = FALSE, 
               row.names = FALSE, characterNA="NA", startRow = 11)
}

###################################
# Compute square R value for GLMM #
###################################
source("R/rsquaredglmm.R")

################################
# Return star based on P value #
################################
FormatPval <- function(Pval) {
  stars <- ifelse(Pval > .05, "",
                  ifelse(Pval > .01, "'*'",
                         ifelse(Pval > .001, "'**'",
                                c("'***'"))))
  
  p <- as.character(ifelse(Pval > .1, round(Pval, 3),
                           ifelse(Pval < .001, "bold('<0.001')", 
                                  # shown with bold font. Note that inside of
                                  # bold needs to be in ''
                                  paste("bold(", round(Pval, 3), ")", sep = "'"))))
  return(data.frame(stars, p))
} 

########################################
# Create summary stat table from anova #
########################################
StatTable <- function(x, variable) { # x is anova result
  df <- data.frame(predictor = c(row.names(x)),
                   rbind(FormatPval(x$Pr)))
  
  # add a row for column name of the table in the fig 
  df <- rbind(df, data.frame(predictor = "", 
                             stars = "italic('P')", 
                             p = "italic('P')"))
  
  result <- merge(df, data.frame(predictor = c("co2", "time", "co2:time")), all = TRUE)
  
  # replace NA with ns
  result <- within(result, {
    p <- ifelse(is.na(p), "ns", as.character(p)) 
    # ifelse tries to return factor, so use as.character
    stars <- ifelse(is.na(stars), "ns", as.character(stars))
  })
  
  # relabel for plotting
  result$predictor <- factor(result$predictor, 
                             labels = c("", "CO[2]", "Time", "CO[2]*~x~Time"), 
                             levels = c("", "co2", "time", "co2:time"))
  result$variable <- variable
  result <- result[order(result$predictor), ]
  return(result)
}

############################################
# Create df to add a stat table to figures #
############################################
StatPositionDF <- function(StatRes, variable, ytop, ylength, gap = .07){
  d <- data.frame(variable, ytop, gap = gap * ylength) 
  # ytop is y coordinate for the top (i.e. CO2) of the table for each fig 
  # (variable), ylength is the difference of max and min value of the plot (i.e.
  # max(mean+SE) - min(mean-SE)). 0.1 * ylength is used to determine the gap between each row
  # of the table
  
  predictor <- levels(StatRes$predictor)
  
  # create df which contains variable, predictor and y coordinates for the other
  # predictors (i.e. Time, CO2xTime) which is ylength*0.1 (= gap) lower than one above
  d2 <- ddply(d, .(variable),
              function(x){
                data.frame(predictor, 
                           ldply(1:length(predictor), function(z) x$ytop - z * x$gap))
              })
  names(d2)[3] <- "yval"
  
  # mege every thing
  d3 <- merge(d2, StatRes, by = c("variable", "predictor"))
  d3$co2 <- "amb" # co2 column is required for ggplot
  return(d3)
}
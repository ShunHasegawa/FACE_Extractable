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

atcr.cmpr <- function(model, rndmFac){
  if(rndmFac == "ring/plot"){
    model2 <- update(model,corr=corCompSymm(form=~1|ring/plot)) 
  } else {
    if(rndmFac == "ring"){
      model2 <- update(model,corr=corCompSymm(form=~1|ring))
    } else {
      model2 <- update(model,corr=corCompSymm(form=~1|id))
    }
  }
  
  model3 <- update(model,correlation=corARMA(q=2))
  model4 <- update(model,correlation=corAR1()) 
  model5 <- update(model,correlation=corARMA(q=1))
  a <- anova(model,model2,model3,model4,model5)
  rownames(a) <- c("NULL", "corCompSymm", "corARMA(q=2)", "corAR1()", "corARMA(q=1)")
  models <- list(model, model2, model3, model4, model5, 'models' = a)
  return(models)
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
  sDF <- subset(data, Date >= Start & Date >= End & ring == rings & plot == plots)
  ddply(sDF, .(ring, plot),function(x) 
    colMeans(x[c("Moist", "Temp_Mean", "Temp_Min", "Temp_Max")], na.rm = TRUE))
}

#########################
# plot predicted values #
#########################
PltPr_Moist <- function(model, ...){
  visreg(model, 
         xvar = "Moist",
         by = "co2", 
         level = 1, # take random factor into accound
         overlay = TRUE, 
         print.cond=TRUE, 
         line.par = list(col = c("blue", "red")),
         points.par = list(col = c("blue", "red")), 
         ...)
  timePos <- seq(1, 3, length.out = 6)
  times <- c(3:8)
  for (i in 1:6){
    lines(x = range(extr$Moist[extr$time == times[i]]), y = rep(timePos[i], 2), lwd = 2)
    text(x = mean(range(extr$Moist[extr$time == times[i]])), y = timePos[i], 
         labels = paste("Time =", times[i]), pos = 3)
  }
  legend("topright", lty =1, leg = "Moist range", bty = "n")
}
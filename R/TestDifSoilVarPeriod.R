# TDR soil data
head(TdrSoil)

# compute 3-month mean of soil variables for each plot

##################################################
# Create mean of soil variable for ginven period #
##################################################
extrSoil <- ddply(extr, .(date, ring, plot, period),
                  # period =  number of dates to get average soil vars
                  function(x) SoilPeriodMean(
                    data = TdrSoil, 
                    Start = x$date - period,
                    End = x$date, 
                    rings = x$ring, 
                    plot = x$plot))

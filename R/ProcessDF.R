extr <- read.csv("Data//extractable.csv", 
                 colClasses=c("ring"="factor",
                              "plot"="factor",
                              "time"="factor",
                              "coverage" = "NULL"))
# remove unnecessary rows
extr <- droplevels(extr[complete.cases(extr), ])

# rename columns
names(extr)[c(2,6:8)] <- c("date","no", "nh", "po")


# organise data frame
extr <- within(extr, {
  # format date
  date <- as.Date(dmy(date))
  
  # add ID for layter analysis
  id <- ring:plot
  
  # add pre and post co2, not last of pre-co2 is used as a
  # baseline of post-co2
  pre <- ifelse(time %in% c(1, 2), TRUE, FALSE)
  post <- ifelse(time != 1, TRUE, FALSE)
  
  # blocking
  block <- recode(ring, 
                  "c(1,2) = 'A'; c(3,4) = 'B'; c(5,6) = 'C'")
})

# save
save(extr, file = "Output//Data/extractable.RData")

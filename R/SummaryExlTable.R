# melt dataset
extrMlt <- melt(extr, id = names(extr)[which(!(names(extr) %in% c("no", "nh", "po")))])

# Ring summary table & mean
RngSmmryTbl <- dlply(extrMlt, .(variable), function(x) CreateTable(x, fac = "ring", digit = 1, nsmall = 2))
RngMean <- ddply(extrMlt, .(time, date, co2, ring, variable), summarise, value = mean(value, na.rm = TRUE)) 

# treat summary table $ mean
TrtSmmryTbl <- dlply(RngMean, .(variable), function(x) CreateTable(x, fac = "co2",  digit = 1, nsmall =2))

## create xcel workbook ##
wb <- createWorkbook()

# worksheet for rowdata
sheet <- createSheet(wb,sheetName="row_data")
addDataFrame(extr, sheet, showNA=TRUE, row.names=FALSE, characterNA="NA")

# worksheets for ring summary
shnames <- paste("Ring_mean.",c("Nitrate", "Ammonium","Phosphate", sep=""))
l_ply(1:3, function(x) crSheet(sheetname = shnames[x], dataset = RngSmmryTbl[[x]]))

# worksheets for temp trt summary
shnames <- paste("Temp_mean.", c("Nitrate", "Ammonium","Phosphate"), sep = "")
l_ply(1:3, function(x) crSheet(sheetname = shnames[x], dataset = TrtSmmryTbl[[x]]))

#save file
saveWorkbook(wb,"Output/Table/FACE_Extractable.xlsx")

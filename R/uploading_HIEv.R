# Reformat data frame for HIEv and creat file name
  hiev_dd <- create_hievdata(data        = extr, 
                             variable    = c("no", "nh", "po"), 
                             newvariable = c("nitrate", "ammonium", "phosphate"))
  
  hiev_file <- create_hivedata_filename(hivedata                 = hiev_dd, 
                                        PROJECT                  = "projectcode", 
                                        VARIABLE_COLLECTION_CODE = "SOILEXTRACTABLENUTRIENTS",
                                        DATA_PROCESSING          = "L3")
  
  write.csv(hiev_dd, file = paste0("Output/HIEv/", hiev_file, ".csv"))
  
# Decription
  cat("Concentrations of extractable inorganic nitrogen (nitrate and ammonium) 
  and phosphorus (phosphate) in soil from quarterly soil sampling. The results are 
  published in Hasegawa et al. 2016 (DOI: 10.1111/gcb.13147).", 
      file = "Output/HIEv/Description.txt")
  
# Metadata
  names(hiev_dd)
  
  plot_v     <- data.frame(Column      = "plot", 
                           Unit        = "", 
                           Description = "Permanent soil plot")
  
  variable_v <- data.frame(Column      = c("nitrate", "ammonium", "phosphate"), 
                           Unit        = "mg/kg", 
                           Description = paste(c("Nitrate-N", "Ammonium-N", "Phosphate-P"), 
                                               "concentrations in kg dry soil"))
  
  metadd <- create_metadata(rbind.fill(list(plot_v, variable_v)))
  
  write.csv(metadd, 
           file      = "Output/HIEv/FACE_MD_SOILEXTRACTABLENUTRIENTS.csv", 
           na        = "",
           row.names = FALSE)

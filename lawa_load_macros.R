# ------------------------------
# BATCH LOADER FOR COUNCIL DATA
# ------------------------------

message("Load Macroinvertebrate data from Councils")
message("-- A folder for todays date will be created and the imported files will be stashed there.")

# Encapsulating mkdir commands in the try() in order to suppress error messages on failure
# Failure's can mean
#               1. Directories already exist
#               1. R:/ drive not mapped to \\file\herman\R\OA\08\02

try(shell(paste('mkdir "R:/2018/Water Quality/1.Imported/"',format(Sys.Date(),"%Y-%m-%d"),sep=""), translate=TRUE),silent = TRUE)
try(shell(paste('mkdir "R:/2018/Water Quality/4.Analysis/"',format(Sys.Date(),"%Y-%m-%d"),sep=""), translate=TRUE),silent = TRUE)


## ----------------------------------------------------------------------------,
## Import Lake data to the "1.Imported" folder for 2018

## import destination will be in folder with todays date (created above)
importDestination <- paste("//file/herman/R/OA/08/02/2018/Water Quality/1.Imported/",format(Sys.Date(),"%Y-%m-%d"),"/",sep="")

# #Northland
 source("//file/herman/r/oa/08/02/2018/Water Quality/R/Macroinvert/loadNRC.R")
 
# #Auckland
 source("//file/herman/r/oa/08/02/2018/Water Quality/R/Macroinvert/loadAC.R")
# 
# #Waikato
 source("//file/herman/r/oa/08/02/2018/Water Quality/R/Macroinvert/loadWRC.R")
# 
# #Bay of Plenty
source("//file/herman/r/oa/08/02/2018/Water Quality/R/Macroinvert/loadBOP.R")

# #Gisborne
source("//file/herman/r/oa/08/02/2018/Water Quality/R/Macroinvert/loadGDC.R")

#Taranaki
source("//file/herman/r/oa/08/02/2018/Water Quality/R/Macroinvert/loadTRC.R")

#Hawkes Bay
source("//file/herman/r/oa/08/02/2018/Water Quality/R/Macroinvert/loadHBRC.R")

#Horizons
source("//file/herman/r/oa/08/02/2018/Water Quality/R/Macroinvert/loadHRC.R")

#Greater Wellington
source("//file/herman/r/oa/08/02/2018/Water Quality/R/Macroinvert/loadGW.R")

#Nelson
source("//file/herman/r/oa/08/02/2018/Water Quality/R/Macroinvert/loadNCC.R")

#Tasman
source("//file/herman/r/oa/08/02/2018/Water Quality/R/Macroinvert/loadTDC.R")

#Marlborough
source("//file/herman/r/oa/08/02/2018/Water Quality/R/Macroinvert/loadMDC.R")

#Canterbury
source("//file/herman/r/oa/08/02/2018/Water Quality/R/Macroinvert/loadECAN.R")

#Otago
source("//file/herman/r/oa/08/02/2018/Water Quality/R/Macroinvert/loadORC.R")

#Southland
source("//file/herman/r/oa/08/02/2018/Water Quality/R/Macroinvert/loadES.R")

#West Coast
source("//file/herman/r/oa/08/02/2018/Water Quality/R/Macroinvert/loadWCRC.R")

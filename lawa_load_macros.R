# ------------------------------
# BATCH LOADER FOR COUNCIL DATA
# ------------------------------

message("Load Macroinvertebrate data from Councils")
message("-- A folder for todays date will be created and the imported files will be stashed there.")

# Encapsulating mkdir commands in the try() in order to suppress error messages on failure
# Failure's can mean
#               1. Directories already exist
#               1. R:/ drive not mapped to \\file\herman\R\OA\08\02

try(shell(paste('mkdir "H:/ericg/16666LAWA/2018/MacroInvertebrates/1.Imported/"',format(Sys.Date(),"%Y-%m-%d"),sep=""), translate=TRUE),silent = TRUE)
# try(shell(paste('mkdir "H:/ericg/16666LAWA/2018/MacroInvertebrates/4.Analysis/"',format(Sys.Date(),"%Y-%m-%d"),sep=""), translate=TRUE),silent = TRUE)


## ----------------------------------------------------------------------------,
## Import Lake data to the "1.Imported" folder for 2018

## import destination will be in folder with todays date (created above)
importDestination <- paste("H:/ericg/16666LAWA/2018/MacroInvertebrates/1.Imported/",format(Sys.Date(),"%Y-%m-%d"),"/",sep="")

 
# ACuckland X
# source("H:/ericg/16666LAWA/2018/MacroInvertebrates/ACMacro.R")
 
# BOPRCay of Plenty X
# source("H:/ericg/16666LAWA/2018/MacroInvertebrates/BOPMacro.R")

#ECanterbury
source("H:/ericg/16666LAWA/2018/MacroInvertebrates/ECANmacro.R")

#ESouthland
source("H:/ericg/16666LAWA/2018/MacroInvertebrates/ESmacro.R")

#GDCisborne
source("H:/ericg/16666LAWA/2018/MacroInvertebrates/GDCMacro.R")

#GWRCreater Wellington
source("H:/ericg/16666LAWA/2018/MacroInvertebrates/GWRCMacro.R")

#HBRCawkes Bay
source("H:/ericg/16666LAWA/2018/MacroInvertebrates/HBRCMacro.R")

#HRCorizons 
source("H:/ericg/16666LAWA/2018/MacroInvertebrates/HRCMacro.R")

#MDCarlborough
source("H:/ericg/16666LAWA/2018/MacroInvertebrates/MDCMacro.R")

#NCCelson
source("H:/ericg/16666LAWA/2018/MacroInvertebrates/NCCMacro.R")

#NRCorthland
# source("H:/ericg/16666LAWA/2018/MacroInvertebrates/nrcMacro.R")

#ORCtago
source("H:/ericg/16666LAWA/2018/MacroInvertebrates/ORCMacro.R")

# TRCaranaki 
source("H:/ericg/16666LAWA/2018/MacroInvertebrates/TRCMacro.R")

#TDCasman
source("H:/ericg/16666LAWA/2018/MacroInvertebrates/TDCMacro.R")

# WCRCest Coast 
source("H:/ericg/16666LAWA/2018/MacroInvertebrates/WCRCMacro.R")

# WRCaikato 
 source("H:/ericg/16666LAWA/2018/MacroInvertebrates/WRCMacro.R")
 

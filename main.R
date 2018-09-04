# ======================
ANALYSIS<-"Main routine"
# Set working directory
od<-getwd()
rm(list = ls())
setwd("H:/ericg/16666LAWA/2018/MacroInvertebrates")
source("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/lawa_state_functions.R")


message("Macro data pull")
print(Sys.time())
timeAtStart<- Sys.time()

## DATA Import
## Site Tables / Location data
source("lawa_dataPrep_WFS.R")

## Water Quality data
source("lawa_load_macros.R")

## Preparing data for Export
source("rbindMACRO_files.R")

message("Process finished")
print(Sys.time())
print(Sys.time()-timeAtStart)
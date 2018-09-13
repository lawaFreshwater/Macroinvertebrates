rm(list=ls())

StartYear10 <- 2008
StartYear5 <- 2013
EndYear <- 2017
source("h:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/scripts/WQualityStateTrend/lawa_state_functions.R")
source("h:/ericg/16666LAWA/2018/LAWAFunctionsEG.R")

siteTable <- read.csv(file = "h:/ericg/16666LAWA/2018/MacroInvertebrates/1.Imported/LAWA_Site_Table_Macro.csv",stringsAsFactors = F)


try(dir.create(paste0("H:/ericg/16666LAWA/2018/MacroInvertebrates/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"))))

#Load the latest made (might need to check it works nicely across month folders) 
if(!exists('macroData')){
  acmacroData=tail(dir(path = "H:/ericg/16666LAWA/2018/MacroInvertebrates/1.Imported",pattern = "MacrosWithMetadata.csv",recursive = T,full.names = T),1)
  macroData=read.csv(acmacroData,stringsAsFactors = F)
  rm(acmacroData)
}

#Output the raw data for ITE
write.csv(macroData,paste0("H:/ericg/16666LAWA/2018/MacroInvertebrates/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),
                           "/RiverMACRO_GraphDataForITE",format(Sys.time(),'%Hh%Mm-%d-%b-%Y'),".csv"),row.names=F)

macroData$month=lubridate::month(lubridate::dmy(macroData$Date))
macroData$Year=lubridate::year(lubridate::dmy(macroData$Date))
macroData=macroData[which(macroData$Year>=StartYear5 & macroData$Year<=EndYear),]

#43969 to 15632


macroData$SWQLanduse[macroData$SWQLanduse%in%c("Exotic","Native","Forest","Natural","forestry","Native forest","reference")]="Forest"
macroData$SWQLanduse[macroData$SWQLanduse%in%c("Unstated","")]=NA
macroData$SWQLanduse[macroData$SWQLanduse%in%c("rural")]="Rural"
macroData$SWQLanduse[macroData$SWQLanduse%in%c("urban")]="Urban"

macroData$SWQAltitude[macroData$SWQAltitude%in%c("Unstated","")]=NA

table(macroData$SWQLanduse)
table(macroData$SWQAltitude)
sum(is.na(macroData$SWQLanduse))
sum(is.na(macroData$SWQAltitude))
unique(macroData$LawaSiteID[is.na(macroData$SWQLanduse)])

unique(cbind(macroData$SiteID[!macroData$SiteID==macroData$SiteID.y],macroData$SiteID.y[!macroData$SiteID==macroData$SiteID.y]))
unique(cbind(macroData$CouncilSiteID[!macroData$CouncilSiteID==macroData$CouncilSiteID.y],macroData$CouncilSiteID.y[!macroData$CouncilSiteID==macroData$CouncilSiteID.y]))
apply(unique(cbind(macroData$Lat[!macroData$Lat==macroData$Lat.y],macroData$Lat.y[!macroData$Lat==macroData$Lat.y])),1,diff)%>%summary





macroParam <- c("PercentageEPTTaxa", "TaxaRichness", "MCI") 

suppressWarnings(rm(macroData_A,macroData_med,macroData_n,lawaMacroData))
for(i in 1:length(macroParam)){
  
  macroData_A = macroData[tolower(macroData$parameter)==tolower(macroParam[i]),]
  # ADD tracking of n, to see how many values the medians were calculated from
  macroData_med <- summaryBy(formula=Value~SiteName+parameter+Year,
                             id=~LawaSiteID+SiteID+CouncilSiteID+Agency+Region+Macro+SWQLanduse+SWQAltitude,
                             data=macroData_A, 
                             FUN=quantile, prob=c(0.5), type=5, na.rm=TRUE, keep.name=TRUE)
  macroData_med$LAWAID=macroData_med$LawaSiteID
  macroData_med$LanduseGroup=macroData_med$SWQLanduse
  macroData_med$AltitudeGroup=macroData_med$SWQAltitude
  macroData_med$Catchment='all'
  macroData_med$Frequency='all'
  
  macroData_n <- summaryBy(formula=Value~SiteName+parameter+Year,
                           id=~LawaSiteID+SiteID+CouncilSiteID+Agency+Region+Macro+SWQLanduse+SWQAltitude,
                           data=macroData_A,
                           FUN=length,keep.names=T)
  macroData_med$n=macroData_n$Value

  rm(macroData_n)
  rm(macroData_A)
  gc()
  # Building dataframe to save at the end of this step 
  if(i==1){
    lawaMacroData <- macroData_med
  } else {
    lawaMacroData <- rbind(lawaMacroData,macroData_med)
  }   
  
  if(0){
    # =======================================================
    # Macro State Analysis
    # =======================================================
    
    # All data for the current parameter is passed through to the StateAnalysis
    # Function.
    #   The output of this function is a data.frame with site medians, 
    # with the grouping variables of landuse, altitude, catchment and local
    # local authority name. This data.frame forms the basis for calculating
    # State for each site, based on the median of the last sampled values
    #   This step also excludes those sites that meets the following exclusion
    # criteria:
    #
    # Exclusion criteria
    #   - less than 30 samples for monthly samples
    #   - less than 80 percent of samples for bimonthly/quarterly
    
    cat("LAWA Macro State Analysis\t",macroParam[i],'\n')
    cat("LAWA Macro State Analysis\nCalculating reference quartiles\n")
    
    state <- c("Site","Catchment","Region","NZ")
    level <- c("LandUseAltitude","LandUse","Altitude","None")
    
    sa11 <- StateAnalysis(df = macroData_med,type = state[1],level = level[1])
    
    sa21 <- StateAnalysis(macroData_med,state[2],level[1])
    sa22 <- StateAnalysis(macroData_med,state[2],level[2])
    sa23 <- StateAnalysis(macroData_med,state[2],level[3])
    sa24 <- StateAnalysis(macroData_med,state[2],level[4])
    
    sa31 <- StateAnalysis(macroData_med,state[3],level[1])
    sa32 <- StateAnalysis(macroData_med,state[3],level[2])
    sa33 <- StateAnalysis(macroData_med,state[3],level[3])
    sa34 <- StateAnalysis(macroData_med,state[3],level[4])
    
    sa41 <- StateAnalysis(macroData_med,state[4],level[1])
    sa42 <- StateAnalysis(macroData_med,state[4],level[2])
    sa43 <- StateAnalysis(macroData_med,state[4],level[3])
    sa44 <- StateAnalysis(macroData_med,state[4],level[4])
    
    cat("LAWA Macro State Analysis\n","Binding ",macroParam[i]," data together for measurement\n")
    
    if(i==1){
      sa <- rbind(sa11,sa21,sa22,sa23,sa24,sa31,sa32,sa33,sa34,sa41,sa42,sa43,sa44)
    } else {
      sa <- rbind(sa,sa11,sa21,sa22,sa23,sa24,sa31,sa32,sa33,sa34,sa41,sa42,sa43,sa44)
    }
    
    rm(sa11,sa21,sa22,sa23,sa24,sa31,sa32,sa33,sa34,sa41,sa42,sa43,sa44)
  }
}
# rm(state)

# Housekeeping
# - Saving the lawaMacroData table  USED in NOF calculations
save(lawaMacroData,file=paste("h:/ericg/16666LAWA/2018/MacroInvertebrates/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/lawaMacroData",StartYear10,"-",EndYear,".RData",sep=""))
# load(file=paste("h:/ericg/16666LAWA/2018/MacroInvertebrates/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/lawaMacroData",StartYear10,"-",EndYear,".RData",sep=""),verbose = T)


# State Analysis output contains quantiles for each parameter by site.
# - Rename data.frame headings
# names(sa) <-  c("AltitudeGroup","LanduseGroup","Region","Catchment","SiteName","LAWAID","Parameter",
#                 "Q0","Q25","Q50","Q75","Q100","N","Scope")

# filter sa to remove any LAWAIDS that are NA
# write.csv(sa,file=paste("h:/ericg/16666LAWA/2018/MacroInvertebrates/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/sa",StartYear,"-",EndYear,".csv",sep=""),row.names = F)

# lmd10 <- lawaMacroData%>%
#   filter(!is.na(LawaSiteID))%>%
#   dplyr::group_by(LawaSiteID,parameter)%>%
#   dplyr::summarise(Median=quantile(Value,prob=0.5,type=5,na.rm=T),n=n())%>%ungroup
# sum(lmd10$cnt>=8)/dim(lmd10)[1]

lawaMacroState5yr <- lawaMacroData%>%
  filter(!is.na(LawaSiteID))%>%
  filter(Year>=StartYear5)%>%
  dplyr::group_by(LawaSiteID,parameter)%>%
  dplyr::summarise(Median=quantile(Value,prob=0.5,type=5,na.rm=T),n=n())%>%ungroup
sum(lawaMacroState5yr$n>=3)/dim(lawaMacroState5yr)[1]

# lmd10 <- lmd10%>%filter(cnt>=8)
lawaMacroState5yr <- lawaMacroState5yr%>%filter(n>=3)

# lmd10$period=10
lawaMacroState5yr$period=5

# macroState=rbind(lmd10,lawaMacroState5yr)
# names(macroState)[3:4] <- c("Median","n")

write.csv(lawaMacroState5yr,file=paste0('h:/ericg/16666LAWA/2018/MacroInvertebrates/4.Analysis/',format(Sys.Date(),"%Y-%m-%d"),
                                        '/RiverMACRO_STATE_ForITE',
                           format(Sys.time(),'%Hh%Mm-%d-%b-%Y'),'.csv'),row.names = F)
lawaMacroState5yr = read.csv(tail(dir(path='h:/ericg/16666LAWA/2018/MacroInvertebrates/4.Analysis/',
                                      pattern='RiverMACRO_STATE_ForITE',
                                      recursive = T,full.names = T),1),stringsAsFactors = F)
stop('Stop here!')

# 
# cat("LAWA Macro State Analysis\nAssigning State Scores\n")
# # ' //   In assigning state scores, the routine needs to process each combination of altitude
# # ' // and landuse and compare to the National levels for the same combinations.
# # ' //   These combinations are:
# 
# # ' //   National data set - no factors
# # ' //       Each site (all altitude and landuses) compared to overall National medians
# 
# # ' //   Single factor comparisons
# # ' //       Each upland site (all landuses) compared to upland National medians
# # ' //       Each lowland site (all landuses) compared to lowland National medians
# # ' //       Each rural site (all altitudes) compared to rural National medians
# # ' //       Each forest site (all altitudes) compared to forest National medians
# # ' //       Each urban site (all altitudes) compared to urban National medians
# 
# # ' //   Multiple factor comparisons
# # ' //      For each Altitude
# # ' //        Each rural site compared to rural National medians
# # ' //        Each forest site compared to forest National medians
# # ' //        Each urban site compared to urban National medians
# 
# # ' //      For each LandUse
# # ' //        Each upland site compared to upland National medians
# # ' //        Each lowland site compared to lowland National medians
# 
# 
# scope <- c("Site","Catchment","Region") 
# i=1
# # for(i in 1:3){
# 
# ss1 <-   StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = tolower(""),landuse = tolower(""),wqparam = macroParam,comparison=1)
# ss21 <-  StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = tolower("Upland"),landuse = tolower(""),macroParam,comparison=2)
# ss22 <-  StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = tolower("Lowland"),landuse = tolower(""),macroParam,comparison=2)
# ss31 <-  StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = tolower(""),landuse = tolower("Rural"),macroParam,comparison=3)
# ss32 <-  StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = tolower(""),landuse = tolower("Forest"),macroParam,comparison=3)
# ss33 <-  StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = tolower(""),landuse = tolower("Urban"),macroParam,comparison=3)
# ss411 <- StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = tolower("Upland"),landuse = tolower("Rural"),macroParam,comparison=4)
# ss412 <- StateScore(df = sa,scopeIn = tolower(scope[i]),altitude = tolower("Upland"),landuse = "forest",macroParam,comparison=4)
# 
# # The following line will fail if there are no sites with Upland Urban classification
# # Need to put a test into the StateScore function to return an empty dataframe
# 
# # RE-ENABLE THIS ONCE BOPRC data available
# if(0){
#   ss413 <- StateScore(df = sa,scopeIn=scope[i],altitude = "upland",landuse = "urban",macroParam,comparison=4)
#   ss421 <- StateScore(sa,scope[i],"lowland","rural",macroParam,comparison=4)
#   ss422 <- StateScore(sa,scope[i],"lowland","forest",macroParam,comparison=4)
#   ss423 <- StateScore(sa,scope[i],"lowland","urban",macroParam,comparison=4)
# }
# 
# if(i==1){
#   ss <- rbind(ss1,ss21,ss22,ss31,ss32,ss33,ss411,ss412,ss421,ss422,ss423)
# } else{
#   ss <- rbind(ss,ss1,ss21,ss22,ss31,ss32,ss33,ss411,ss412,ss421,ss422,ss423)
# }
# rm(ss1,ss21,ss22,ss31,ss32,ss33,ss411,ss412,ss421,ss422,ss423)
# # }
# 
# 
# write.csv(ss,file=paste("h:/ericg/16666LAWA/2018/MacroInvertebrates/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/state",StartYear,"-",EndYear,".csv",sep=""),row.names = F)
# 
# 
# 
# cat("LAWA Macro State Analysis\nCompleted assigning State Scores\n")
# 
# ss_csv <- read.csv(file=paste("h:/ericg/16666LAWA/2018/MacroInvertebrates/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/state",StartYear,"-",EndYear,".csv",sep=""),header=TRUE,sep=",",quote = "\"")
# 
# ss.1 <- subset(ss_csv,Scope=="Region")
# ss.1$Location <- ss.1$Region
# ss.2 <- subset(ss_csv,Scope=="Catchment")
# ss.2$Location <- ss.2$Catchment
# ss.3 <- subset(ss_csv,Scope=="Site")
# ss.3$Location <- ss.3$LAWAID
# 
# ss.4 <- rbind.data.frame(ss.1,ss.2,ss.3)
# # unique(ss.4$Location)
# 
# ss.5 <- ss.4[c(18,8,2,3,11,17,4,15,16)-1]  # Location, Parameter, Altitude, Landuse, Q50, LAWAState, Region, Scope, StateGroup
# 
# 
# 
# write.csv(ss.5,file=paste("h:/ericg/16666LAWA/2018/MacroInvertebrates/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/LAWA_STATE_FINAL_",StartYear,"-",EndYear,".csv",sep=""),row.names = F)
# # lawaMacroData_without_niwa <- subset(lawaMacroData,Agency!="NIWA")
# # lawaMacroData_q_without_niwa <- subset(lawaMacroData_q,Agency!="NIWA")
# 
# # write.csv(lawaMacroData_without_niwa,"h:/ericg/16666LAWA/2018/MacroInvertebrates/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/LAWA_DATA.csv",row.names = F)
# # write.csv(lawaMacroData_without_niwa,"h:/ericg/16666LAWA/2018/MacroInvertebrates/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/LAWA_RAW_DATA.csv",row.names = F)
# 
# 

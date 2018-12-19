#load NIWA
rm(list=ls())
library(tidyverse)
#NIWA data has lats and longs which we can match to the lawa ids,
# to get a lawasiteID assigned to it.  Most of those lawasiteIDs should be in the siteTable
# But who cares anyway, just keep the lawaIDs trackign across to the end. 

riverSiteTable=read.csv(file="h:/ericg/16666LAWA/2018/WaterQuality/1.Imported/LAWA_Site_Table_River.csv",stringsAsFactors = F)
# riverSiteTable$CouncilSiteIDlc=tolower(riverSiteTable$CouncilSiteID)
extraMacroTable=riverSiteTable#[riverSiteTable$CouncilSiteIDlc%in%NIWAMCIl$SiteNamelc,]
rm(riverSiteTable)
extraMacroTable=rbind(extraMacroTable,data.frame(CouncilSiteID="SQ10067",LawaSiteID="ECAN-10004",SiteID="Hakataramea River u/s SH82 bridge",
                                                 SWQuality=FALSE,SWQAltitude="Upland",SWQLanduse='Rural',SWQFrequencyAll=NA,
                                                 SWQFrequencyLast5=NA,Region="Canterbury",Agency='niwa',Lat=-44.72610032651481,
                                                 Long=170.4903439007408,accessDate='19-Sep-2018'))
extraMacroTable=rbind(extraMacroTable,data.frame(CouncilSiteID="103248",LawaSiteID="NRC-10008",SiteID="Waipapa @ Forest Ranger",
                                                 SWQuality=FALSE,SWQAltitude="",SWQLanduse='',SWQFrequencyAll=NA,
                                                 SWQFrequencyLast5=NA,Region="Northland",Agency='niwa',Lat=-35.2763146207668,
                                                 Long= 173.684049395477,accessDate='19-Sep-2018'))
extraMacroTable$Macro=F
extraMacroTable <- extraMacroTable%>%select("SiteID","CouncilSiteID","LawaSiteID","Macro","Region","Agency","SWQLanduse","SWQAltitude","Lat","Long")
write.csv(extraMacroTable,file='h:/ericg/16666LAWA/2018/MacroInvertebrates/1.Imported/ExtraMacrotable.csv',row.names=F)






source('k:/R_functions/nzmg2WGS.r')
NIWAmacroSites=readxl::read_xlsx('h:/ericg/16666LAWA/2018/MacroInvertebrates/1.Imported/NIWAInvertebrate.xlsx',sheet = 'site metadata')
lawaIDs=read.csv("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/2018_csv_config_files/LAWAMasterSiteListasatMarch2018.csv",stringsAsFactors = F)
lawaIDs <- lawaIDs%>%dplyr::filter(Module=="Freshwater Quality")
lawaIDs$Long=as.numeric(lawaIDs$Longitude)
lawaIDs$Lat=as.numeric(lawaIDs$Latitude)

latlon=nzmg2wgs(East =  NIWAmacroSites$nzmge,North =  NIWAmacroSites$nzmgn)
NIWAmacroSites$Long=latlon[,2]
NIWAmacroSites$Lat=latlon[,1]
rm(latlon)

minDists=rep(0,dim(NIWAmacroSites)[1])
NIWAmacroSites$LawaSiteID=NA
NIWAmacroSites$lidname=NA
# NIWAmacroSites$Region=NA
for(nms in 1:(dim(NIWAmacroSites)[1])){
  dists=sqrt((NIWAmacroSites$Long[nms]-lawaIDs$Long)^2+(NIWAmacroSites$Lat[nms]-lawaIDs$Lat)^2)
  minDists[nms]=min(dists,na.rm=T)
  NIWAmacroSites$LawaSiteID[nms]=lawaIDs$LawaID[which.min(dists)]
  NIWAmacroSites$lidname[nms]=lawaIDs$SiteName[which.min(dists)]
}
NIWAmacroSites$dist=minDists

# NIWAmacroSites[,c(8,15)]%>%as.data.frame



NIWAMCI=readxl::read_xlsx('h:/ericg/16666LAWA/2018/MacroInvertebrates/1.Imported/NIWAInvertebrate.xlsx',sheet = 'MCI')
NIWAMCI$lawaid=gsub(x = NIWAMCI$lawaid,pattern = '-ND',replacement = '-DN')
NIWAMCI$LawaSiteID = NIWAmacroSites$LawaSiteID[match(NIWAMCI$lawaid,NIWAmacroSites$lawaid)]

NIWAMCI$SiteName = extraMacroTable$CouncilSiteID[match(NIWAMCI$LawaSiteID,extraMacroTable$LawaSiteID)]
# NIWAMCI$SiteName = lawaIDs$SiteName[match(NIWAMCI$LawaSiteID,lawaIDs$LawaID)]
NIWAMCI=NIWAMCI%>%select(-'QMCI',-'ntotal',-'Site.code')
NIWAMCI$agency='niwa'
NIWAMCIl=NIWAMCI%>%gather(key = 'parameter',value = 'Value',c("MCI","ntaxa"))
NIWAMCIl$Date = format((NIWAMCIl$Date),'%d-%b-%Y')
NIWAMCIl$Value=as.numeric(NIWAMCIl$Value)
#Round the year to that of the nearest Jan1 
NIWAMCIl$Year = lubridate::year(round_date(lubridate::dmy(NIWAMCIl$Date),unit = 'year'))
NIWAMCIl$SiteNamelc=tolower(NIWAMCIl$SiteName)
NIWAMCIl$Method=NA

rm(lawaIDs)

write.csv(NIWAMCIl%>%select(SiteName,Date,Value,Method,parameter),
          file=paste0( 'H:/ericg/16666LAWA/2018/MacroInvertebrates/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/','NIWA.csv'),
          row.names=F)





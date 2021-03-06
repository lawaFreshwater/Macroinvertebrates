rm(list=ls())

source("H:/ericg/16666LAWA/2018/LAWAFunctionsEG.R")

siteTable=read.csv("H:/ericg/16666LAWA/2018/MacroInvertebrates/1.Imported/LAWA_Site_Table_Macro.csv",stringsAsFactors=FALSE)

#XML 2 CSV for MACROS ####
agency='gwrc'
lawaset=c("TaxaRichness","MCI","PercentageEPTTaxa")
for(agency in c("ac","ecan","es","gdc","gwrc","hbrc","hrc","mdc","ncc","nrc","orc","tdc","trc","wcrc","wrc")){
  if(agency!='ac'){
    if(agency=='tdc'){
      #Special delivery for Mr Eric Goodwin had the wrong site names, so needs a little unrearranging
      cat("Reading hard-coded CSV delivery file for TDC from 2018-09-04\n")
      forcsv=read.csv("file:///H:/ericg/16666LAWA/2018/MacroInvertebrates/1.Imported/2018-09-04/TDC_in.csv",stringsAsFactors=F)
      # table(unique(tolower(forcsv$SiteName))%in%tolower(siteTable$CouncilSiteID))
      # table(unique(tolower(forcsv$SiteName))%in%tolower(siteTable$SiteID))
      forcsv$SiteName[tolower(forcsv$SiteName)%in%tolower(siteTable$SiteID)] <- 
        siteTable$CouncilSiteID[match(tolower(forcsv$SiteName[tolower(forcsv$SiteName)%in%tolower(siteTable$SiteID)]),
                                      tolower(siteTable$SiteID))]
      write.csv(forcsv,
                file=paste0( 'H:/ericg/16666LAWA/2018/MacroInvertebrates/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/',agency,'.csv'),
                row.names=F)
      rm(forcsv)
    }else{
      forcsv=xml2csvMacro(agency,maxHistory = 30,quiet=T)
      # cat(dim(forcsv),'\t')
      # forcsv=unique(forcsv)
      # cat(dim(forcsv),'\n')
      
      cat(agency,'\t',paste0(unique(forcsv$parameter),collapse=', '),'\n')
      if('SQMCI'%in%unique(forcsv$parameter)){
        forcsv=forcsv[-which(forcsv$parameter%in%c('SQMCI')),]
        cat(agency,'\t',paste0(unique(forcsv$parameter),collapse=', '),'\n')
      }
      forcsv$parameter[grepl(pattern = 'Taxa',x = forcsv$parameter,ignore.case = T)&
                         !grepl('EPT',forcsv$parameter,ignore.case = F)] <- "TaxaRichness"
      forcsv$parameter[grepl(pattern = 'MCI',x = forcsv$parameter,ignore.case = T)] <- "MCI"
      forcsv$parameter[grepl(pattern = 'ate community ind',x = forcsv$parameter,ignore.case = T)] <- "MCI"
      forcsv$parameter[grepl(pattern = '\\% EPT Taxa',x = forcsv$parameter,ignore.case = T)] <- "PercentageEPTTaxa"
      forcsv$parameter[grepl(pattern = 'EPT',x = forcsv$parameter,ignore.case = T)] <- "PercentageEPTTaxa"
      forcsv$parameter[grepl(pattern = 'Rich',x = forcsv$parameter,ignore.case = T)] <- "TaxaRichness"
      excess=unique(forcsv$parameter)[!unique(forcsv$parameter)%in%lawaset]
      if(length(excess)>0){
        forcsv=forcsv[-which(forcsv$parameter%in%excess),]
      }
      rm(excess)
      write.csv(forcsv,
                file=paste0( 'H:/ericg/16666LAWA/2018/MacroInvertebrates/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/',agency,'.csv'),
                row.names=F)
      rm(forcsv)
    }
  }else{
    #Handle AC macros separately, from CSV file delivered by email
    acmacro=read.csv('h:/ericg/16666LAWA/2018/MacroInvertebrates/1.Imported/ACMacros.csv',stringsAsFactors = F,encoding='UTF-8')%>%
      select(Date:Site,Scoring.taxa.HB:MCI.sb,MCI.HB.or.MCI.SB)
    acmacro <- acmacro%>%dplyr::rename(SiteName=Site,TaxaRichness=Total.Richness,PercentageEPTTaxa=X..EPT.Richness)%>%
      select(SiteName,Date,MCI,PercentageEPTTaxa,TaxaRichness)%>%
      gather(parameter,Value,MCI:TaxaRichness)
    cat(agency,'\t',paste0(unique(acmacro$parameter),collapse=', '),'\n')
    nNAbeforeconversion=sum(is.na(acmacro$Value))
    acmacro$Value=as.numeric(gsub(pattern = '%',replacement = '',x = acmacro$Value))
    nNAafterconversion=sum(is.na(acmacro$Value))
    if(nNAafterconversion!=nNAbeforeconversion){
      stop("There may be censored values in AC data, and the script is not yet to set up to convert them.")
    }
    acmacro$Method=NA
    acmacro$Censored=F
    acmacro$centype=F
    write.csv(acmacro,
              file=paste0( 'H:/ericg/16666LAWA/2018/MacroInvertebrates/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/ac.csv'),
              row.names=F)
    rm(acmacro)
  }
}

#Rub a macro audit ####
library(lubridate)
nms=data.frame(agency=NULL,var=NULL,earliest=NULL,latest=NULL,nMeas=NULL,nSite=NULL,meanMeas=NULL,maxMeas=NULL,minMeas=NULL,nNA=NULL)
for(agency in c("ac","boprc","ecan","es","gdc","gwrc","hbrc","hrc","mdc","ncc","nrc","orc","tdc","trc","wcrc","wrc")){
  forcsv=loadLatestCSVmacro(agency)
  newRows=data.frame(agency=rep(agency,length(unique(forcsv$parameter))),
                     var=sort(unique(forcsv$parameter)),
                     earliest=rep("",length(unique(forcsv$parameter))),
                     latest=rep("",length(unique(forcsv$parameter))),
                     nMeas=rep(0,length(unique(forcsv$parameter))),
                     nSite=rep(NA,length(unique(forcsv$parameter))),
                     meanMeas=rep(NA,length(unique(forcsv$parameter))),
                     maxMeas=rep(NA,length(unique(forcsv$parameter))),
                     minMeas=rep(NA,length(unique(forcsv$parameter))),
                     nNA=rep(NA,length(unique(forcsv$parameter))),
                     stringsAsFactors = F)
  if(!is.null(forcsv)){
    for(v in 1:dim(newRows)[1]){
      newRows$earliest[v]=format(min(dmy(forcsv$Date[which(forcsv$parameter==newRows$var[v])])),"%d-%b-%Y")
      newRows$latest[v]=format(max(dmy(forcsv$Date[which(forcsv$parameter==newRows$var[v])])),"%d-%b-%Y")
      newRows$nMeas[v]=sum(forcsv$parameter==newRows$var[v])
      newRows$nSite[v]=length(unique(forcsv$SiteName[which(forcsv$parameter==newRows$var[v] & !is.na(forcsv$Value))]))
      newRows$meanMeas[v]=round(mean(forcsv$Value[forcsv$parameter==newRows$var[v]],na.rm=T),1)
      newRows$maxMeas[v]=round(max(forcsv$Value[forcsv$parameter==newRows$var[v]],na.rm=T),1)
      newRows$minMeas[v]=round(min(forcsv$Value[forcsv$parameter==newRows$var[v]],na.rm=T),1)
      newRows$nNA[v]=sum(is.na(forcsv$Value[forcsv$parameter==newRows$var[v]]))
    }
  }
  nms <- rbind.data.frame(nms,newRows)
}
write.csv(nms,paste0("h:/ericg/16666LAWA/2018/MacroInvertebrates/QA/macroAudit.csv"))

# lawaIDs=read.csv("H:/ericg/16666LAWA/2018/WaterQuality/R/lawa_state/2018_csv_config_files/LAWAMasterSiteListasatMarch2018.csv",stringsAsFactors = F)

#Combine multiple councils into a combo file ####
combo=data.frame(agency=NA,SiteName=NA,Date=NA,Value=NA,Method=NA,parameter=NA)
siteTable=read.csv("H:/ericg/16666LAWA/2018/MacroInvertebrates/1.Imported/LAWA_Site_Table_Macro.csv",stringsAsFactors=FALSE)
for(agency in c("ecan","ac","boprc","es","gdc","gwrc","hbrc","hrc","mdc","ncc","nrc","orc","tdc","trc","wcrc","wrc")){
  if(agency!='boprc'){
  forcsv=loadLatestCSVmacro(agency,quiet=T)
  }else{
    boprcMI=read.csv('h:/ericg/16666LAWA/2018/MacroInvertebrates/1.Imported/2018-09-11/BOPRCDelivery.csv',stringsAsFactors = F)
    names(boprcMI)=c('sid','date','period','Date','SiteName','LawaSiteID','MCI',"TaxaRichness",'PercentageEPTTaxa')
    boprcMI=boprcMI%>%gather(key="parameter",value="Value",MCI:PercentageEPTTaxa)
    boprcMI$Date=paste0('1-1-',boprcMI$Date)
    boprcMI$Date[boprcMI$date!='(blank)'] <- boprcMI$date[boprcMI$date!='(blank)']
    boprcMI$agency='boprc'
    boprcMI$Method=NA
    forcsv <- boprcMI%>%select(names(combo))
  }
  if(!is.null(forcsv)){
    cat(agency,'\n',paste(names(forcsv),collapse='\t'),'\n')
    forcsv$agency=agency
    if(agency=='ac'){
      #BLOODRY TREATIONG AUNKLAND SPECIAL AGAIN@@@@@
      forcsv=forcsv[tolower(forcsv$SiteName)%in%tolower(siteTable$CouncilSiteID),]
    }
    if(agency=='es'){
      toCut=which(forcsv$SiteName=="mataura river 200m d/s mataura bridge"&forcsv$Date=="21-Feb-2017")
      if(length(toCut)>0){
        forcsv=forcsv[-toCut,]
      }
      rm(toCut)
    }
    if(agency=='tdc'){
      forcsv$SiteName[!tolower(forcsv$SiteName)%in%tolower(siteTable$CouncilSiteID)]=siteTable$CouncilSiteID[match(tolower(forcsv$SiteName[!tolower(forcsv$SiteName)%in%tolower(siteTable$CouncilSiteID)]),
                                                                                                                   tolower(siteTable$SiteID))]
    }
    combo=merge(combo,forcsv[,names(forcsv)%in%names(combo)],all=T)
  }  
}
combo=combo[-(dim(combo)[1]),]
write.csv(combo,paste0('h:/ericg/16666LAWA/2018/MacroInvertebrates/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/MacrosCombined.csv'),row.names = F)
# combo=read.csv(paste0('h:/ericg/16666LAWA/2018/MacroInvertebrates/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/MacrosCombined.csv'),stringsAsFactors = F)

names(combo)

#These lines tell us that an hrc site is not in siteTable$CouncilSiteID
table(unique(tolower(combo$SiteName))%in%tolower(siteTable$CouncilSiteID))
unique(combo$agency[!tolower(combo$SiteName)%in%tolower(siteTable$CouncilSiteID)])
unique(combo$SiteName[!tolower(combo$SiteName)%in%tolower(siteTable$CouncilSiteID)])
combo$SiteName[grepl('slackline',combo$SiteName)] <- siteTable$CouncilSiteID[grep('slackline',x = siteTable$SiteID,ignore.case = T)]

table(unique(tolower(combo$SiteName))%in%tolower(siteTable$CouncilSiteID))

#bingo
combo$SiteNamelc=tolower(combo$SiteName)

#Some BOP sites can get LAWASiteIDs by thingy from their thingy
replacementNames=tolower(siteTable$CouncilSiteID)[match(tolower(combo$SiteNamelc[which(combo$agency=='boprc')]),tolower(siteTable$SiteID))]
combo$SiteNamelc[which(combo$agency=='boprc')[!is.na(replacementNames)]]=replacementNames[!is.na(replacementNames)]
rm(replacementNames)

siteTable$CouncilSiteIDlc=tolower(siteTable$CouncilSiteID)
macrosWithMetadata=merge(combo,siteTable,by.x="SiteNamelc",by.y="CouncilSiteIDlc",all.x=T,all.y=F)%>%
  select(CouncilSiteID,SiteName,SiteID,LawaSiteID,Region,Agency,Date,parameter,Value,Method,Lat,Long,SWQLanduse,SWQAltitude) #Drop macro and sitnamelc
macrosWithMetadata$Agency[which(macrosWithMetadata$SiteName%in%boprcMI$SiteName)] <- 'boprc'
macrosWithMetadata$Region[macrosWithMetadata$Agency=='boprc'] <- "Bay of Plenty"
write.csv(macrosWithMetadata,paste0('h:/ericg/16666LAWA/2018/MacroInvertebrates/1.Imported/',format(Sys.Date(),"%Y-%m-%d"),'/MacrosWithMetadata.csv'),row.names = F)


#Load all councils to their own objects ####
for(agency in c("ecan","ac","boprc","es","gdc","gwrc","hbrc","hrc","mdc","ncc","nrc","orc","tdc","trc","wcrc","wrc")){
  forcsv=loadLatestCSVmacro(agency,quiet=T,maxHistory = 100)
  # cat(agency,'\t',paste0(unique(siteTable$Agency[match(tolower(forcsv$SiteName),tolower(siteTable$CouncilSiteID))]),collapse='\t'),'\n')
   eval(parse(text=paste0(agency,'=forcsv')))
}

a$x=b$x[match(a$y,b$y)]
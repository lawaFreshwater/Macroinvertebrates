rm(list=ls())
library(tidyverse)
source("h:/ericg/16666LAWA/LWPTrends/LWPTrends_v1808.R")

EndYear <- 2017
startYear5 <- EndYear - 5+1
startYear10 <- EndYear - 10+1

#Load the latest made (might need to check it works nicely across month folders) 
if(!exists('macroData')){
  acmacroData=tail(dir(path = "H:/ericg/16666LAWA/2018/MacroInvertebrates/1.Imported",pattern = "MacrosWithMetadata.csv",recursive = T,full.names = T),1)
  macroData=read.csv(acmacroData,stringsAsFactors = F)
  rm(acmacroData)
}

macroData$month=lubridate::month(lubridate::dmy(macroData$Date))
macroData$Year=lubridate::year(lubridate::dmy(macroData$Date))
macroData=macroData[which(macroData$Year>=startYear10 & macroData$Year<=EndYear),]


try(dir.create(paste0("h:/ericg/16666LAWA/2018/MacroInvertebrates/4.Analysis/",format(Sys.Date(),"%Y-%m-%d")),recursive = T))




# https://www.lawa.org.nz/learn/factsheets/calculating-water-quality-trends/
# But LWPTrends drops all censored values for SenSlope calcn

suppressWarnings(rm(upara,ucounc,up,pvals,p1,p5,p75,p95,p999))

macroData$myDate <- as.Date(as.character(macroData$Date),"%d-%b-%Y")
macroData <- GetMoreDateInfo(macroData)
macroData$monYear = format(macroData$myDate,"%b-%Y")

macroData$Season <- macroData$Month
SeasonString <- sort(unique(macroData$Season))

# macroData$CenType[macroData$CenType%in%c("Left","L")]='lt'
# macroData$CenType[macroData$CenType%in%c("Right","R")]='gt'
# macroData$CenType[!macroData$CenType%in%c("lt","gt")]='not'
# macroData$NewValues=macroData$Value



source("h:/ericg/16666LAWA/2018/MacroInvertebrates/R/lawa_state/scripts/WQualityStateTrend/lawa_state_functions.R")




#10 year trend ####
macroData$Season=macroData$month
macroData$Censored=F
macroData$CenType="FALSE"
usites=unique(macroData$LawaSiteID)
uMeasures="MCI"
trendTable10=structure(list(LawaSiteID=NA,parameter=NA,
                            # Observations = NA_integer_, KWstat = NA_real_,pvalue = NA_real_,
                            nObs = NA_integer_, S = NA_real_, VarS = NA_real_,D = NA_real_, tau = NA_real_, Z = NA_real_, p = NA_real_, 
                            Median = NA_real_, VarS = NA_real_, AnnualSenSlope = NA_real_, Intercept = NA_real_, Lci = NA_real_, Uci = NA_real_, TrendCategory = NA, 
                            TrendDirection = NA, Probability = NA_real_, Probabilitymax = NA_real_, Probabilitymin = NA_real_, Percent.annual.change = NA_real_), class = "data.frame")
nMax=length(table(macroData$LawaSiteID,macroData$parameter)[table(macroData$LawaSiteID,macroData$parameter)>0])
passCriteria10=data.frame(LawaSiteID=rep('',nMax),param=rep('',nMax),#repFreq=rep('',nMax),
                          nFirstYear=rep(0,nMax),nLastYear=rep(0,nMax),
                          numSamples=rep(0,nMax),numYears=rep(0,nMax),
                          stringsAsFactors = F)
pcpos=1
cat(length(usites),'\n')
for(usite in 1:length(usites)){
  cat('.')
  if(as.integer(usite/20)==(usite/20)){cat(usite,'\n')}
  subDat=macroData%>%filter(LawaSiteID==usites[usite],parameter=="MCI")
    if(dim(subDat)[1]>8){
      SSD_med <- summaryBy(formula=Value~LawaSiteID+Year,
                           id=~myDate+Season+Censored+CenType,
                           data=subDat, 
                           FUN=quantile, prob=c(0.5), type=5, na.rm=TRUE, keep.name=TRUE)
      firstYear=length(which(SSD_med$Year==startYear10))
      lastYear=length(which(SSD_med$Year==EndYear))
      numSamples=dim(SSD_med)[1]
      numYears=length(unique(SSD_med$Year[!is.na(SSD_med$Value)]))
      passCriteria10[pcpos,]=c(usites[usite],"MCI",
                               firstYear,lastYear,numSamples,numYears)
      pcpos=pcpos+1
      rm(subDat)
      #For 10 year we want 9 years available out of 10
      if(numYears>=8){
        cat('+')
        suppressWarnings(rm(st,mk,ss,sk,sss))
          (mk <- MannKendall(x = SSD_med,ValuesToUse = "Value",doPlot=F))
          (ss <- SenSlope(x = SSD_med,ValuesToUse = "Value",doPlot = F))
          newRow=cbind(LawaSiteID=usites[usite],parameter="MCI",mk,ss)
        trendTable10=rbind(trendTable10,newRow)
        rm(newRow)
      }else{
        cat('.')
      }
      rm(SSD_med)
  }
  rm(subDat)
}
rm(usites,uMeasures,usite)
rownames(trendTable10) <- NULL
save(trendTable10,file=paste0("h:/ericg/16666LAWA/2018/MacroInvertebrates/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/Trend10Year.rData"))


load(paste0("h:/ericg/16666LAWA/2018/MacroInvertebrates/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/Trend10Year.rData"))
# combTrend$Probability[combTrend$parameter!="BDISC"]=1-(combTrend$Probability[combTrend$parameter!="BDISC"])



# rm(list=ls())
source("h:/ericg/16666LAWA/LWPTrends/LWPTrends_v1808.R")
if(!exists('macroData')){
  acmacroData=tail(dir(path = "H:/ericg/16666LAWA/2018/MacroInvertebrates/1.Imported",pattern = "AllCouncils.csv",recursive = T,full.names = T),1)
  cat(acmacroData)
  macroData=read.csv(acmacroData,stringsAsFactors = F)
  rm(acmacroData)
  macroData$SWQLanduse[tolower(macroData$SWQLanduse)%in%c("unstated","")] <- NA
  macroData$SWQLanduse[tolower(macroData$SWQLanduse)%in%c("forest","native","exotic","natural")] <- "forest"
  macroData$myDate <- as.Date(as.character(macroData$Date),"%d-%b-%Y")
  macroData <- GetMoreDateInfo(macroData)
  
  macroData$Season <- macroData$Month
  SeasonString <- sort(unique(macroData$Season))
  
  macroData$CenType[macroData$CenType%in%c("Left","L")]='lt'
  macroData$CenType[macroData$CenType%in%c("Right","R")]='gt'
  macroData$CenType[!macroData$CenType%in%c("lt","gt")]='not'
  
  macroData$NewValues=macroData$Value
}


siteTable=read.csv("H:/ericg/16666LAWA/2018/MacroInvertebrates/1.Imported/LAWA_Site_Table_Macro.csv",stringsAsFactors = F)



trendTable10$ConfCat <- cut(trendTable10$Probability, breaks=  c(0, 0.1,0.33,0.66,0.90, 1),
                            labels = c("Very likely improving",
                                       "Likely improving",
                                       "Indeterminate",
                                       "Likely degrading",
                                       "Very likely degrading"))
trendTable10$ConfCat=factor(trendTable10$ConfCat,levels=rev(c("Very likely improving",
                                                              "Likely improving",
                                                              "Indeterminate",
                                                              "Likely degrading",
                                                              "Very likely degrading")))

trendTable10$period=10
trendTable10=trendTable10[,-which(names(trendTable10)=="VarS")[2]]

# trendTable10$Altitude =  siteTable$SWQAltitude[match(trendTable10$LawaSiteID,siteTable$LawaSiteID)]
# trendTable10$Landuse =   siteTable$SWQLanduse[match(trendTable10$LawaSiteID,siteTable$LawaSiteID)]
# trendTable10$Region =    siteTable$Region[match(trendTable10$LawaSiteID,siteTable$LawaSiteID)]
# trendTable10$Frequency = siteTable$SWQFrequencyAll[match(trendTable10$LawaSiteID,siteTable$LawaSiteID)]
trendTable10$TrendScore=as.numeric(trendTable10$ConfCat)-3
trendTable10$TrendScore[is.na(trendTable10$TrendScore)]<-(-99)
trendTable10Export <- trendTable10%>%select(LawaSiteID,parameter,Altitude,Landuse,TrendScore,Region,period)

write.csv(trendTable10Export,paste0("h:/ericg/16666LAWA/2018/MacroInvertebrates/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),
                                    "/MacroMCI_Trend_ForITE",
                         format(Sys.time(),"%Hh%Mm-%d%b%Y"),".csv"),row.names = F)
trendTable10Export=read.csv(tail(dir(path="h:/ericg/16666LAWA/2018/MacroInvertebrates/4.Analysis",
                                     pattern="MacroMCI_Trend",
                                     recursive = T,full.names = T),1),stringsAsFactors = F)
rm(trendTable10Export)

plott=T
usites=unique(trendTable10$LawaSiteID)
uMeasures=unique(trendTable10$parameter)
for(uparam in seq_along(uMeasures)){
  subTrend=trendTable10[which(trendTable10$parameter==uMeasures[uparam]),]
  worstDeg <- which.max(subTrend$Probability) 
  bestImp <- which.min(subTrend$Probability)
  cat(subTrend$Probability[worstDeg],'\t')
  cat(subTrend$Probability[bestImp],'\n')
  if(plott){
    tiff(paste0("h:/ericg/16666LAWA/2018/MacroInvertebrates/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/BestWorst",uMeasures[uparam],".tif"),
         width = 8,height=8,units='in',res=300,compression='lzw',type='cairo')
    
    par(mfrow=c(2,1),mar=c(2,4,1,2))
    theseDeg <- which(macroData$LawaSiteID==subTrend$LawaSiteID[worstDeg] &
                        macroData$parameter==uMeasures[uparam] & dmy(macroData$Date)>dmy("1-1-2008"))
    theseImp <- which(macroData$LawaSiteID==subTrend$LawaSiteID[bestImp] &
                        macroData$parameter==uMeasures[uparam] & dmy(macroData$Date)>dmy("1-1-2008"))
    
      MannKendall(x = macroData[theseDeg,],ValuesToUse = "Value",doPlot=F)
      SenSlope(x = macroData[theseDeg,],ValuesToUse = "Value",doPlot = T)
      MannKendall(x = macroData[theseImp,],ValuesToUse = "Value",doPlot=F)
      SenSlope(x = macroData[theseImp,],ValuesToUse = "Value",doPlot = T)
    if(names(dev.cur())=='tiff'){dev.off()}
    rm(theseDeg,theseImp)
  }
  rm(worstDeg,bestImp)
}


#Make the coloured plot
tb <- plot(factor(trendTable10$parameter),trendTable10$ConfCat,
           col=c("#dd1111FF","#ee4411FF","#bbbbbbFF","#11cc11FF","#008800FF"),main="Ten year trends")
tbp <- apply(X = tb,MARGIN = 1,FUN = function(x)x/sum(x))
mbp <- apply(tbp,MARGIN = 2,FUN=cumsum)
mbp <- rbind(0,mbp)
mbp = (mbp[-1,]+mbp[-6,])/2
# tiff(paste0("h:/ericg/16666LAWA/2018/MacroInvertebrates/4.Analysis/",format(Sys.Date(),"%Y-%m-%d"),"/TenYearTrends.tif"),
#      width = 8,height=8,units='in',res=300,compression='lzw',type='cairo')
par(mfrow=c(1,1),mar=c(5,10,4,2))
barplot(tbp,main="Ten year trends",las=2,
        col=c("#dd1111FF","#ee4411FF","#bbbbbbFF","#11cc11FF","#008800FF"),yaxt='n')
axis(side = 2,at = mbp[,1],labels = colnames(tb),las=2,lty = 0)
  text(0.75,mbp,tb)

if(names(dev.cur())=='tiff'){dev.off()}



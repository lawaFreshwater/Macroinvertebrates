#remove hostory
rm(list = ls())



#Set working directory
od<-getwd()
setwd("//file/herman/R/OA/08/02/2017/Water Quality/R/Macroinvert")

#reqired packages
require(XML)     ### XML library to write hilltop XML
require(dplyr)   ### dply library to manipulate table joins on dataframes
require(RCurl)
require(reshape2)
curdir<-getwd()

tab="\t"

fname <- "BOPMacro.csv"
df <- read.csv(fname,sep=",", stringsAsFactors=FALSE)



molten = melt(df, id.vars = c("Site.name", "Date.and.time", "LAWA.ID", "Processing.Method", 
                              "Collection.Method","Quality.Assurance.Method", "Quality.Assurance.By", "Quality.Code" ), 
              measure.vars = c("MCI", "X._EPT_taxa", "TaxaRichness"))


#df2 <- merge(df,molten, by =c("Site.name", "Date.and.time"))


molten <- molten[c(1,9,2,10,8,5, 3, 4, 6,7)]

write.csv(molten, file = "BOPMacroFormatted.csv")

setwd(od)





ld <- function(url){
  faultChk <- try(download.file(url,destfile="tmpAC",method="wininet",quiet=T))
  if(faultChk==0){
  # pause(1)
  xmlfile <- xmlParse(file = "tmpAC")
  unlink("tmpr")
  error<-as.character(sapply(getNodeSet(doc=xmlfile, path="//Error"), xmlValue))
  exception<-as.character(sapply(getNodeSet(doc=xmlfile, path="//ows:Exception"), xmlValue))
  if(length(error)==0&length(exception)==0){
    return(xmlfile)   # if no error, return xml data
  } else {
    return(NULL)
  }
  }else{
    return(NULL)
  }
}




fname <-"H:/ericg/16666LAWA/2018/MacroInvertebrates/2018_csv_config_files/bopMacro_config.csv"
df <- read.csv(fname,sep=",",stringsAsFactors=FALSE)
Measurements <- subset(df,df$Type=="Measurement")[,1]
Measurements <- as.vector(Measurements)

siteTable=read.csv("H:/ericg/16666LAWA/2018/MacroInvertebrates/1.Imported/LAWA_Site_Table_Macro.csv",stringsAsFactors=FALSE)
sites = unique(siteTable$CouncilSiteID[siteTable$Agency=='boprc'])

for(i in 1:length(sites)){
  cat(i,'out of',length(sites),'\n')
  for(j in 1:length(Measurements)){
    cat('.')
    url <- paste0("http://ec2-52-6-196-14.compute-1.amazonaws.com/sos-bop/service?",
                  "service=SOS&version=2.0.0&request=GetObservation&observedProperty=",Measurements[j],
                  "&featureOfInterest=",sites[i],
                  "&temporalfilter=om:phenomenonTime,2004-01-01/2018-01-01")
    
    url <- gsub("%", "%25", url)
    url <- gsub(" ", "%20", url)
    xmlfile <- ld(url)
    if(!is.null(xmlfile)){
      browser()
    }   
  }
}






rm(list = ls())

## --- Functions ---
# returns string w/o leading or trailing whitespace
trim <- function (x) gsub("^\\s+|\\s+$", "", x)

## Convert datestring to mow seconds (number of seconds since 1-Jan-1940 00:00)
#mowSecs <- function(x){
#  s<-strptime("1940-01-01","%Y-%m-%d")
#  t<-strptime(x,"%Y-%m-%d %H:%M:%S")
#   t<-strptime(x,"%Y-%m-%d %H:%M:%S")
#  x<-(t-s)*86400
#}

require(XML)     ### XML library to write hilltop XML
require(dplyr)   ### dply library to manipulate table joins on dataframes
require(RCurl)


od<-getwd()
setwd("//file/herman/R/OA/08/02/2018/Water Quality/R/Macroinvert")

#function to either create full xml file or return xml file as NULL depending
#on the result from the above funciton
requestData <- function(url){
  #url<-"http://hilltopdev.horizons.govt.nz/data.hts?service=Hilltop"
  #RCurl::getURL(paste(url,"&request=Reset",sep=""))
  #url <- paste(url,request,sep="")
  cat(url,"\n")
  # ret <- htsServiceError(url)
  #if(ret==TRUE){
  xmldata <- ld(url)
  return(xmldata)
  # }else {
  # xmldata <- NULL
  # return(xmldata)
  
  # }
}


#function to create xml file from url. 
ld <- function(url){
  (download.file(url,destfile="tmp",method="wininet"))
  xmlfile <- xmlParse(file = "tmp")
  unlink("tmp")
  return(xmlfile)
}

fname <- "wrcMacro_config.csv"
df <- read.csv(fname,sep=",",stringsAsFactors=FALSE)

sites <- subset(df,df$Type=="Site")[,1]
sites <- as.vector(sites)
Measurements <- subset(df,df$Type=="Measurement")[,1]
Measurements <- as.vector(Measurements)

## Load libraries ------------------------------------------------
require(RODBC)   ### ODBC library for SQL connection
require(dplyr)   ### dply library to manipulate table joins on dataframes
require(XML)     ### XML library to write hilltop XML

for(i in 1:length(sites)){
  for(j in 1:length(Measurements)){
    
    
    url <- paste("http://envdata.waikatoregion.govt.nz:8080/KiWIS/KiWIS?datasource=0&service=SOS&version=2.0&request=GetObservation&featureOfInterest="
                  ,sites[i], "&procedure=Cmd.P&observedProperty=", Measurements[j], "&temporalfilter=om:phenomenonTime,P30Y", sep="")
    xmlfile <- ld(url)
    
    #urlw <- paste("http://envdata.waikatoregion.govt.nz:8080/KiWIS/KiWIS?datasource=0&service=SOS&version=2.0&request=GetObservation&featureOfInterest="
  #                ,sites[i], "&procedure=WARIMP.Sample.Results.P&observedProperty=", Measurements[j], "&temporalfilter=om:phenomenonTime,P10Y", sep="")
    
   # xmlfilew <- ld(urlw)
    
    #urlc <- "http://envdata.waikatoregion.govt.nz:8080/KiWIS/KiWIS?datasource=0&service=SOS&version=2.0&request=GetCapabilities"
    #xmlfilec <- requestData(urlc)
    
    
    
    #Create vector of times
    time <- sapply(getNodeSet(doc=xmlfile, "//wml2:time"), xmlValue)
    #Create vector of  values
    value <- sapply(getNodeSet(doc=xmlfile, "//wml2:value"), xmlValue)
    
    df <- as.data.frame(time, stringsAsFactors = FALSE)
    df$value <- value
    
    
    #For WARIMP
    #Create vector of times
    #time <- sapply(getNodeSet(doc=xmlfilew, "//wml2:time"), xmlValue)
    #Create vector of  values
    #value <- sapply(getNodeSet(doc=xmlfilew, "//wml2:value"), xmlValue)
    
    #dfw <- as.data.frame(time, stringsAsFactors = FALSE)
    #dfw$value <- value
    
    
    #Add dataframes together, Not sure if need to overwrite value to one
    if(nrow(df)==0){
      next
    }  else {
      xmldata <- xmlfile
    }
    
    
    #Create vector of units
    myPath<-"//wml2:uom"
    c<-getNodeSet(xmldata, path=myPath)
    u<-sapply(c,function(el) xmlGetAttr(el, "code"))
    
    df$Site <- sites[i]
    df$Measurement <- Measurements[j]
    df$Units <- u
    df <- df[,c(3,4,1,2,5)]
    
    
    
    if(!exists("Data")){
      Data <- df
    } else{
      Data <- rbind.data.frame(Data, df)
    }
    
    
    
  }
}



#p <- sapply(getNodeSet(doc=xmlfilec ,path="//sos:ObservationOffering/swes:name"), xmlValue)

#procedure <- c("RERIMP.Sample.Results.P", "WARIMP.Sample.Results.P")


#----------------
tm<-Sys.time()
cat("Building XML\n")
cat("Creating:",Sys.time()-tm,"\n")

con <- xmlOutputDOM("Hilltop")
con$addTag("Agency", "WRC")
#saveXML(con$value(), file="out.xml")

#-------

max<-nrow(Data)
#max<-nrows(datatbl)

i<-1
#for each site
while(i<=max){
  s<-Data$Site[i]
  # store first counter going into while loop to use later in writing out sample values
  start<-i
  
  cat(i,Data$Site[i],"\n")   ### Monitoring progress as code runs
  
  while(Data$Site[i]==s){
    #for each measurement
    #cat(datatbl$SiteName[i],"\n")
    con$addTag("Measurement",  attrs=c(SiteName=Data$Site[i]), close=FALSE)
    con$addTag("DataSource",  attrs=c(Name=Data$Measurement[i],NumItems="2"), close=FALSE)
    con$addTag("TSType", "StdSeries")
    con$addTag("DataType", "WQData")
    con$addTag("Interpolation", "Discrete")
    con$addTag("ItemInfo", attrs=c(ItemNumber="1"),close=FALSE)
    con$addTag("ItemName", Data$Measurement[i])
    con$addTag("ItemFormat", "F")
    con$addTag("Divisor", "1")
    con$addTag("Units", Data$Units[i])
    #con$addTag("Units", "Joking")
    con$addTag("Format", "#.###")
    con$closeTag() # ItemInfo
    con$closeTag() # DataSource
    #saveXML(con$value(), file="out.xml")
    
    # for the TVP and associated measurement water quality parameters
    con$addTag("Data", attrs=c(DateFormat="Calendar", NumItems="2"),close=FALSE)
    d<- Data$Measurement[i]
    
    cat("       - ",Data$Measurement[i],"\n")   ### Monitoring progress as code runs
    
    while(Data$Measurement[i]==d){
      # for each tvp
      con$addTag("E",close=FALSE)
      con$addTag("T",Data$time[i])
      con$addTag("I1", Data$value[i])
      con$addTag("I2", paste("Units", Data$Units[i], sep="\t"))
      
      con$closeTag() # E
      i<-i+1 # incrementing overall for loop counter
      if(i>max){break}
    }
    # next
    con$closeTag() # Data
    con$closeTag() # Measurement
    
    if(i>max){break}
    # Next 
  }
  # store last counter going out of while loop to use later in writing out sample values
  end<-i-1
  
  # Adding WQ Sample Datasource to finish off this Site
  # along with Sample parameters
  con$addTag("Measurement",  attrs=c(SiteName=Data$Site[start]), close=FALSE)
  con$addTag("DataSource",  attrs=c(Name="WQ Sample", NumItems="1"), close=FALSE)
  con$addTag("TSType", "StdSeries")
  con$addTag("DataType", "WQSample")
  con$addTag("Interpolation", "Discrete")
  con$addTag("ItemInfo", attrs=c(ItemNumber="1"),close=FALSE)
  con$addTag("ItemName", "WQ Sample")
  con$addTag("ItemFormat", "S")
  con$addTag("Divisor", "1")
  con$addTag("Units")
  con$addTag("Format", "$$$")
  con$closeTag() # ItemInfo
  con$closeTag() # DataSource
  
  # for the TVP and associated measurement water quality parameters
  con$addTag("Data", attrs=c(DateFormat="Calendar", NumItems="1"),close=FALSE)
  # for each tvp
  ## LOAD SAMPLE PARAMETERS
  ## SampleID, ProjectName, SourceType, SamplingMethod and mowsecs
  sample<-Data[start:end,3]
  sample<-unique(sample)
  sample<-sample[order(sample)]
  ## THIS NEEDS SOME WORK.....
  for(a in 1:length(sample)){ 
    con$addTag("E",close=FALSE)
    con$addTag("T",sample[a])
    #put metadata in here when it arrives
    con$addTag("I1", "")
    con$closeTag() # E
  }
  
  con$closeTag() # Data
  con$closeTag() # Measurement    
  
}
cat("Saving: ",Sys.time()-tm,"\n")
saveXML(con$value(), file="wrcMacro.xml")
cat("Finished",Sys.time()-tm,"\n")
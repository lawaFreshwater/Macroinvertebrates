
## SET LOCAL WORKING DIRECTORY
od <- getwd()
setwd("//file/herman/R/OA/08/02/2018/Water Quality/R/Macroinvert")


## Load libraries ------------------------------------------------
require(XML)     ### XML library to write hilltop XML
require(dplyr)   ### dply library to manipulate table joins on dataframes
require(RCurl)

## Local variables.
f <- c("ac","nrc","wrc","bop","gdc","hbrc","hrc","trc","gwrc","mdc","ncc","tdc","ecan","wcrc",",orc","es")


for(i in 1:length(f)){
  fn <- paste(f[i],"Macro.xml",sep="")   # fn = file name
  if(file.exists(fn)){
    if(!exists("d")){
      xmlfile <- xmlParse(file = fn)
      # Create vector of sitenames
      # cat.SiteName
      sites<-unique(sapply(getNodeSet(xmlfile, path="//Measurement/@SiteName"), as.character))
      if(length(sites)!=0){
        for(j in 1:length(sites)){
          ds <- sapply(getNodeSet(xmlfile, paste("//Measurement[@SiteName='",sites[j],"']/DataSource/@Name",sep="")), as.character)
          ms <- sapply(getNodeSet(xmlfile, paste("//Measurement[@SiteName='",sites[j],"']/DataSource/ItemInfo[@ItemNumber='1']/ItemName",sep="")), xmlValue)
          if(length(ms)!=length(ds)) ms<-ds
          # Drop WQ Sample
          ds <- ds[!ds %in% "WQ Sample"]
          ms <- ms[!ms %in% "WQ Sample"]
          # cat.DefaultMeasurement
          
          for(k in 1:length(ds)){
            #while wq.getNext
            
            Time <- sapply(getNodeSet(xmlfile, paste("//Measurement[@SiteName='",sites[j],"']/DataSource[@Name='",ds[k],"']/../Data/E/T",sep="")), xmlValue)
            Value <- sapply(getNodeSet(xmlfile, paste("//Measurement[@SiteName='",sites[j],"']/DataSource[@Name='",ds[k],"']/../Data/E/I1",sep="")), xmlValue)
            
            if(k==1){
              a <- data.frame(f[i],sites[j],ms[k],Time,Value, stringsAsFactors=FALSE)
              names(a) <- c("Council","Site","Measurement","Time","Value")
            } else {
              b <- data.frame(f[i], sites[j],ms[k],Time,Value, stringsAsFactors=FALSE)
              names(b) <- c("Council","Site","Measurement","Time","Value")
              a <- rbind.data.frame(a,b,stringsAsFactors=FALSE)
            }
          }
          
          if(!exists("d")){
            d <- a
          } else {
            d <- rbind.data.frame(d,a,stringsAsFactors=FALSE)
          }         
          
        }
        write.csv(d,file = paste(f[i],"Macro-test.txt",sep=""))
        
        if(!exists("e")){
          e <- d
        } else {
          e <- rbind.data.frame(e,d)
        }
        rm(d)
      }
    }
    
  }
  
  
}

save(e,file="rbindMAcrofiles.Rdata")


  
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
library(readr)
setwd("/Users/bin/Desktop/Paper idea/4-bloomberg")

## read.csv mergers
library(readr)
merger_part1 <- read_delim("research - M&A - both listed - north america - merger part1.csv", 
                           "\t", escape_double = FALSE, col_types = cols(`Amendment Date` = col_date(format = "%Y/%m/%d"), 
                                                                         `Announce Date` = col_date(format = "%Y/%m/%d"), 
                                                                         `Completion/Termination Date` = col_date(format = "%Y/%m/%d"), 
                                                                         `Percent Owned` = col_double(), `Percent Sought` = col_double()), 
                           na = "empty", trim_ws = TRUE)
View(merger_part1)

merger_part2 <- read_delim("research - M&A - both listed - north america - merger part2.csv", 
                           "\t", escape_double = FALSE, col_types = cols(`Amendment Date` = col_date(format = "%Y/%m/%d"), 
                                                                         `Announce Date` = col_date(format = "%Y/%m/%d"), 
                                                                         `Completion/Termination Date` = col_date(format = "%Y/%m/%d"), 
                                                                         `Percent Owned` = col_double(), `Percent Sought` = col_double()), 
                           na = "empty", trim_ws = TRUE)
View(merger_part2)

#combine the two part together
merger <- rbind(merger_part1, merger_part2)

#remove all the spaces and other special signals in variable names
names(merger) <- gsub(" ", "", names(merger) , fixed = TRUE)
names(merger) <- gsub("/", "", names(merger) , fixed = TRUE)
names(merger) <- gsub("(mil.)", "", names(merger) , fixed = TRUE)

#delete rows if cusip is empty or unusual N.A. 
#merger <- merger[!(is.na(merger$Targetcusip)),]; merger <- merger[!(is.na(merger$Acquirercusip)),]
merger <- merger[which(merger$Targetcusip !="N.A."),]; merger <- merger[which(merger$Acquirercusip !="N.A."),]
merger <- merger[which(merger$Targetcusip !=""),]; merger <- merger[which(merger$Acquirercusip !=""),]


## read.csv sic
library(readr)
sic_part1 <- read_delim("research - M&A - both listed - north america - sic part1.csv", 
                        "\t", escape_double = FALSE, col_types = cols(`Amendment Date` = col_date(format = "%Y/%m/%d"), 
                                                                      `Announce Date` = col_date(format = "%Y/%m/%d"), 
                                                                      `Completion/Termination Date` = col_date(format = "%Y/%m/%d")), 
                        na = "empty", trim_ws = TRUE)
View(sic_part1)

library(readr)
sic_part2 <- read_delim("research - M&A - both listed - north america - sic part2.csv", 
                        "\t", escape_double = FALSE, col_types = cols(`Amendment Date` = col_date(format = "%Y/%m/%d"), 
                                                                      `Announce Date` = col_date(format = "%Y/%m/%d"), 
                                                                      `Completion/Termination Date` = col_date(format = "%Y/%m/%d")), 
                        na = "empty", trim_ws = TRUE)
View(sic_part2)

#combine the two part together
sic <- rbind(sic_part1, sic_part2)

#remove all the spaces and other special signals in variable names
names(sic) <- gsub(" ", "", names(sic) , fixed = TRUE)
names(sic) <- gsub("/", "", names(sic) , fixed = TRUE)
names(sic) <- gsub("(mil.)", "", names(sic) , fixed = TRUE)

#delete rows if cusip is empty or unusual N.A. 
#sic <- sic[!(is.na(sic$Targetcusip)),]; sic <- sic[!(is.na(sic$Acquirercusip)),]
sic <- sic[which(sic$Targetcusip !="N.A."),]; sic <- sic[which(sic$Acquirercusip !="N.A."),]
sic <- sic[which(sic$Targetcusip !=""),]; sic <- sic[which(sic$Acquirercusip !=""),]

#get the duplicated cases
merger1<-merger[duplicated(merger[,c("CompletionTerminationDate","Targetcusip","Acquirercusip")],fromLast = TRUE),]
merger2<-merger[duplicated(merger[,c("CompletionTerminationDate","Targetcusip","Acquirercusip")],fromLast = FALSE),]
merger0<-rbind(merger1, merger2)

sic1<-sic[duplicated(sic[,c("CompletionTerminationDate","Targetcusip","Acquirercusip")],fromLast = TRUE),]
sic2<-sic[duplicated(sic[,c("CompletionTerminationDate","Targetcusip","Acquirercusip")],fromLast = FALSE),]
sic0<-rbind(sic1, sic2)

#remove all the duplicated cases (including the original ones)
x <- rbind(merger, merger0)
merger <- x[! duplicated(x, fromLast=TRUE) & seq(nrow(x)) <= nrow(merger), ]

x <- rbind(sic, sic0)
sic <- x[! duplicated(x, fromLast=TRUE) & seq(nrow(x)) <= nrow(sic), ]

#merge merger and sic together
mydata <- merge(merger, sic, by=c("AnnounceDate","CompletionTerminationDate","Targetcusip","Acquirercusip"), all=TRUE)
mydata <- mydata[order(mydata["CompletionTerminationDate"]),]
mydata <- mydata[which(mydata$PercentOwned <= mydata$PercentSought),]

#select the specific variables 
mydata <- subset(mydata,select=c(AnnounceDate, CompletionTerminationDate, AnnouncedTotalValue, Targetcusip, Acquirercusip, Sellercusip.x, DealStatus, PercentOwned, PercentSought, CurrentTargetSICCode, CurrentAcquirerSICCode, CurrentSellerSICCode))
#delete rows if sic is empty or unusual N.A. 
mydata <- mydata[which(mydata$CurrentTargetSICCode!=""),]; mydata <- mydata[which(mydata$CurrentAcquirerSICCode!=""),]
mydata <- mydata[which(mydata$CurrentTargetSICCode!="N.A."),]; mydata <- mydata[which(mydata$CurrentAcquirerSICCode!="N.A."),]
library(reshape)
mydata <- rename(mydata,c("Sellercusip.x"="Sellercusip"))

library(stringr)
#create acquirer cusip
mydata$cusip_8a <- str_sub(mydata$Acquirercusip, start= 1, end=8)
mydata$cusip_8t <- str_sub(mydata$Targetcusip, start= 1, end=8)
mydata$cusip_8s <- str_sub(mydata$Sellercusip, start= 1, end=8)

cusip_8a <- as.data.frame(mydata$cusip_8a); cusip_8a <- rename(cusip_8a, c("mydata$cusip_8a"="cusip_8"))
cusip_8t <- as.data.frame(mydata$cusip_8t); cusip_8t <- rename(cusip_8t, c("mydata$cusip_8t"="cusip_8"))
cusip_8s <- as.data.frame(mydata$cusip_8s); cusip_8s <- rename(cusip_8s, c("mydata$cusip_8s"="cusip_8"))

cusip_8 <- rbind(cusip_8a, cusip_8t)
cusip_8 <- rbind(cusip_8, cusip_8s)

cusip_8 <- as.data.frame(cusip_8[which(cusip_8$cusip_8 !=""),])
cusip_8 <- as.data.frame(cusip_8[which(cusip_8$cusip_8 !="N.A."),])
cusip_8 <- as.data.frame(cusip_8[which(duplicated(cusip_8$cusip_8) == FALSE),])

colnames(cusip_8)[1] <- 'cusip_8'
write.csv(cusip_8, file = "cusip_8.csv",row.names = FALSE)

#import IBES data
ibes <- read_csv("~/Desktop/Paper idea/4-bloomberg/ibes_statsum_cusip8_international.csv", 
                 col_types = cols(ACTUAL = col_skip(), 
                                  ANNDATS_ACT = col_skip(), ANNTIMS_ACT = col_skip(), 
                                  CURCODE = col_skip(), CURR_ACT = col_skip(), 
                                  ESTFLAG = col_skip(), FISCALP = col_skip(), 
                                  FPEDATS = col_skip(), FPI = col_skip(), 
                                  MEASURE = col_skip(), OFTIC = col_skip(), 
                                  STATPERS = col_date(format = "%Y/%m/%d"), 
                                  TICKER = col_skip()), na = "empty")
View(ibes)

#extract year and month
library(lubridate)
ibes$YrMonth <- paste(year(ibes$STATPERS), '-', month(ibes$STATPERS), sep='')
mydata$YrMonth <- paste(year(mydata$AnnounceDate), '-', month(mydata$AnnounceDate), sep='')

#choose all the related cusip ibes data (don't need to choose, because we already chose it when downloading the original ibes)
#mydata1 <- mydata(which(mydata$Acquirercusip in ))

#prestep: form the acquistion date and time list (we have it now: mydata)

#we choose the time window
window_choice <- function(temp){
  relative = ibes[FALSE,]
  for (i in 1:length(mydata)){
    cusip <- mydata[i, 'cusip_8a']
    cusip <- as.character(cusip)
    if (cusip %in% ibes$CUSIP){
      singlestock <- ibes[which(ibes$CUSIP==cusip),]
      for (j in 1:length(singlestock)){
        if (mydata[i,'YrMonth'] == singlestock[j,'YrMonth']){
          intm=as.data.frame(singlestock[(j-temp):(j+temp+1),])
          intm['case']=i
          relative <- rbind(relative, intm)
        }
      }
    }
  }
  return(relative)
}

window3=window_choice(3)
#window12=window_choice(12)


#library(RJSONIO)
#library(rPython)

#python.exec(
#  data_clean=ibes[['OFTIC','CUSIP','CNAME','STATPERS','MEANEST','STDEV','NUMEST']]
  
#)






#library(plyr)
#library(reshape)
#library(reshape2)
#reshape ibes
#ibes0 <- subset(ibes, select=c("STATPERS","CUSIP", "STDEV"))
#ibes.reshape<-reshape(ibes0,v.names="STDEV",timevar="CUSIP",idvar = "STATPERS", direction="wide")
#edit(ibes.reshape)






##Filter Data

```{r}
library(dplyr)
subdata1<-filter(data, data["Deal Status"] == "Completed" )
View(subdata1)
```




#merger$Targetcusip <- gsub(".", "", merger$Targetcusip , fixed = TRUE)
#merger$Targetcusip <- gsub("+", "", merger$Targetcusip , fixed = TRUE)
#merger$Acquirercusip <- gsub(".", "", merger$Acquirercusip , fixed = TRUE)
#merger$Acquirercusip <- gsub("+", "", merger$Acquirercusip , fixed = TRUE)
#merger$Sellercusip <- gsub(".", "", merger$Sellercusip , fixed = TRUE)
#merger$Sellercusip <- gsub("+", "", merger$Sellercusip , fixed = TRUE)

#merger$Targetcusip <- paste("00000",merger$Targetcusip, sep="")
#merger$Targetcusip <- str_sub(merger$Targetcusip, start= -9)
#merger$Acquirercusip <- paste("00000",merger$Acquirercusip, sep="")
#merger$Acquirercusip <- str_sub(merger$Acquirercusip, start= -9)
#merger$Sellercusip <- paste("00000",merger$Sellercusip, sep="")
#merger$Sellercusip <- str_sub(merger$Sellercusip, start= -9)

#mydata2 <- mydata[,c("CompletionTerminationDate","Targetcusip","Acquirercusip")]
#edit(mydata[which(duplicated(mydata2)==TRUE),sort(names(mydata))])

#edit(mydata[mydata["Acquirer cusip"]=="989701107",])

#library(magrittr)
#cusip_8 %<>% rbind(cusip_8a, cusip_8t) %>% rbind(.,cusip_8s)
#strtrim(mydata["Acquirercusip"])
#strtrim(mydata$Acquirercusip,8)
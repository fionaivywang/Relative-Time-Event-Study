# Questions:
#1???"total value " in merger part1, N/A
#2??? 2 observations duplicated in mydata (just merged four parts)
#3) 20 observations in merger 0. what are the observations?
#4) N/A cusip in mydata?? (delete 500!)*****
#5???Other resources of sic information? (too much deleted!)
#6) ESTFLAG: explain it.


library(readr)
setwd("/Users/bin/Desktop/Paper idea/4-bloomberg")

## read.csv mergers
merger_part1 <- read_delim("research - M&A - both listed - north america - merger part1.csv", 
                           "\t", escape_double = FALSE, col_types = cols(`Amendment Date` = col_date(format = "%Y/%m/%d"), 
                                                                         `Announce Date` = col_date(format = "%Y/%m/%d"), 
                                                                         `Completion/Termination Date` = col_date(format = "%Y/%m/%d"), 
                                                                         `Percent Owned` = col_double(), `Percent Sought` = col_double()), 
                           na = "empty", trim_ws = TRUE)
merger_part2 <- read_delim("research - M&A - both listed - north america - merger part2.csv", 
                           "\t", escape_double = FALSE, col_types = cols(`Amendment Date` = col_date(format = "%Y/%m/%d"), 
                                                                         `Announce Date` = col_date(format = "%Y/%m/%d"), 
                                                                         `Completion/Termination Date` = col_date(format = "%Y/%m/%d"), 
                                                                         `Percent Owned` = col_double(), `Percent Sought` = col_double()), 
                           na = "empty", trim_ws = TRUE)

#combine the two part together
merger <- rbind(merger_part1, merger_part2)

#remove all the spaces and other special signals in variable names
names(merger) <- gsub(" ", "", names(merger) , fixed = TRUE)
names(merger) <- gsub("/", "", names(merger) , fixed = TRUE)
names(merger) <- gsub("(mil.)", "", names(merger) , fixed = TRUE)

# #delete rows if cusip is empty or unusual N.A.
# nrow(merger)
# merger <- merger[which(merger$Targetcusip !="N.A."),]
# nrow(merger)
# merger <- merger[which(merger$Acquirercusip !="N.A."),]
# nrow(merger)
# merger <- merger[which(merger$Targetcusip !=""),]; merger <- merger[which(merger$Acquirercusip !=""),]
# # Look at whether you can avoid deleting all these 600 observations -- maybe there are other ways to merger using other firms' information


## read.csv sic
sic_part1 <- read_delim("research - M&A - both listed - north america - sic part1.csv", 
                        "\t", escape_double = FALSE, col_types = cols(`Amendment Date` = col_date(format = "%Y/%m/%d"), 
                                                                      `Announce Date` = col_date(format = "%Y/%m/%d"), 
                                                                      `Completion/Termination Date` = col_date(format = "%Y/%m/%d")), 
                        na = "empty", trim_ws = TRUE)
sic_part2 <- read_delim("research - M&A - both listed - north america - sic part2.csv", 
                        "\t", escape_double = FALSE, col_types = cols(`Amendment Date` = col_date(format = "%Y/%m/%d"), 
                                                                      `Announce Date` = col_date(format = "%Y/%m/%d"), 
                                                                      `Completion/Termination Date` = col_date(format = "%Y/%m/%d")), 
                        na = "empty", trim_ws = TRUE)

# combine the two part together
sic <- rbind(sic_part1, sic_part2)

#remove all the spaces and other special signals in variable names
names(sic) <- gsub(" ", "", names(sic) , fixed = TRUE)
names(sic) <- gsub("/", "", names(sic) , fixed = TRUE)
names(sic) <- gsub("(mil.)", "", names(sic) , fixed = TRUE)

rm(merger_part1, merger_part2, sic_part1, sic_part2)
mydata <- merge(merger,sic,by=c("AnnounceDate","CompletionTerminationDate","Targetcusip","Acquirercusip"),all=TRUE)
rm(merger,sic)

# get the duplicated cases and delete them
merger1<-mydata[duplicated(mydata[,c("CompletionTerminationDate","Targetcusip","Acquirercusip")],fromLast = TRUE),]
merger2<-mydata[duplicated(mydata[,c("CompletionTerminationDate","Targetcusip","Acquirercusip")],fromLast = FALSE),]
merger0<-rbind(merger1, merger2)
x <- rbind(mydata, merger0)
mydata <- x[! duplicated(x, fromLast=TRUE) & seq(nrow(x)) <= nrow(mydata), ]
rm(merger1,merger2,merger0,x)

mydata <- mydata[which(mydata$Targetcusip !="N.A."),]; mydata <- mydata[which(mydata$Acquirercusip !="N.A."),]
mydata <- mydata[which(mydata$Targetcusip !=""),]; mydata <- mydata[which(mydata$Acquirercusip !=""),]
mydata <- mydata[which(mydata$CurrentTargetSICCode!="N.A."),]; mydata <- mydata[which(mydata$CurrentAcquirerSICCode!="N.A."),]
mydata <- mydata[which(mydata$CurrentTargetSICCode!=""),]; mydata <- mydata[which(mydata$CurrentAcquirerSICCode!=""),]


#remove cases that are not completed
mydata <- mydata[which(mydata$DealStatus=="Completed"),]

#keep company takeover only (x to 100%)
mydata <- mydata[which(grepl("Company Takeover", mydata$DealAttributes)),]
#check percent sought here
mydata$TotalOwned <- round(mydata$PercentOwned+mydata$PercentSought)
mydata <- mydata[which((mydata$TotalOwned==100)|(!grepl("Company Takeover", mydata$DealAttributes))),]

# select the specific variables 
mydata <- subset(mydata,select=c(AnnounceDate, CompletionTerminationDate, AnnouncedTotalValue, Targetcusip, 
                                 Acquirercusip, Sellercusip.x, DealStatus, PercentOwned, PercentSought, CurrentTargetSICCode, CurrentAcquirerSICCode, CurrentSellerSICCode))
names(mydata) <- gsub("Sellercusip.x", "Sellercusip", names(mydata) , fixed = TRUE)

library(stringr)
# create acquirer cusip
mydata$Acquirercusip <- substr(mydata$Acquirercusip, start = 1, stop = 8)
mydata$Targetcusip <- substr(mydata$Targetcusip, start = 1, stop = 8)
mydata$Sellercusip <- substr(mydata$Sellercusip, start = 1, stop = 8)

# create a list containing all the related M&A cusip
cusip_a <- as.data.frame(mydata$Acquirercusip, stringsAsFactors = FALSE)
colnames(cusip_a)[1] <- 'cusip'
cusip_t <- as.data.frame(mydata$Targetcusip, stringsAsFactors = FALSE)
colnames(cusip_t)[1] <- 'cusip'
cusip_s <- as.data.frame(mydata$Sellercusip, stringsAsFactors = FALSE)
colnames(cusip_s)[1] <- 'cusip'

library(dplyr)
header <- bind_rows(cusip_a, cusip_t)
header <- bind_rows(header, cusip_s)
header <- unique(header)
names(header) <- c('cusip')
header <- as.data.frame(header[which(header$cusip != "N.A."),], stringAsFactors = FALSE)
names(header) <- c('cusip')
header <- as.data.frame(header[which(header$cusip != ""),], stringAsFactors = FALSE)
names(header) <- c('cusip')
write.table(header, file = "cusip_8.txt",row.names = FALSE, col.names = FALSE, sep="\t")
rm(cusip_a, cusip_t, cusip_s)

##############
#IBES#########
##############
#import ibes (nstatsum_epsus.sas7bdat)
ibes <- read_csv("~/Desktop/Paper idea/4-bloomberg/nstatsum_epsus.csv", 
                 col_types = cols(ACTDATS_ACT = col_date(format = "%m/%d/%Y"), 
                                  ACTTIMS_ACT = col_skip(), ANNDATS_ACT = col_date(format = "%m/%d/%Y"), 
                                  ANNTIMS_ACT = col_skip(), MEASURE = col_skip(), 
                                  STATPERS = col_date(format = "%m/%d/%Y"),
                                  FPEDATS = col_date(format = "%m/%d/%Y")), 
                 na = "empty")

summary(ibes)

table

#ANNDATS_ACT: earning annoucement date (1981.12 - 2016.12)
#ACTDATS_ACT: earning annoucement to be included in IBES date

#filter ibes datasets
# 1. choose the time period
ibes1 <- ibes[which(ibes$STATPERS > "1981-12-01" & ibes$STATPERS < "2017-01-01"),]
# 2. choose ANN/QTR/LTG
ibes2 <- ibes1[which(ibes1$FISCALP=="LTG"),]
# select the specific variables 
ibes2 <- subset(ibes2,select=c(TICKER, CUSIP, STATPERS, MEANEST, STDEV, NUMEST))
# delete duplicated rows (no duplicated rows)
ibes2 <- ibes2[which(!duplicated(ibes2)),]

# PRE. choose companies (can only put it in this position)
# PRE. choose the related cusip (in header list) in ibes2 (in order to reduce loop times, because ibes is too large)
ibes_ticker <- ibes2[which(ibes2$CUSIP %in% header$cusip),]

namevector <- c("aibtic","tibtic","sibtic")
mydata[,namevector] <- NA

for (i in 1:nrow(mydata)){
  temporary <- ibes_ticker[which(ibes_ticker$CUSIP == mydata[i,'Acquirercusip']),]
  mydata[i,"aibtic"]<- temporary[1,'TICKER']
}

for (i in 1:nrow(mydata)){
  temporary <- ibes_ticker[which(ibes_ticker$CUSIP == mydata[i,'Targetcusip']),]
  mydata[i,"tibtic"]<- temporary[1,'TICKER']
}

mydata <- mydata[which(!is.na(mydata$aibtic)),]

# 3.1 choose ibes data with merger cases
ibes3 <- ibes2[which(ibes2$TICKER %in% mydata$aibtic),]

# 3.2 change the dataset from long to wide
library(reshape2)
# melt data
ibes3 <- subset(ibes3, select = -c(CUSIP))
ibes3_melt <- melt(ibes3, id=c("STATPERS", "TICKER"))
# cast the melted data
ibes3_std <- subset(ibes3, select = -c(MEANEST, NUMEST))
ibes3_cast <- dcast(ibes3_std, STATPERS ~ TICKER, value.var='STDEV')
ibes3_cast <- as.data.frame(ibes3_cast)
#4 change the date from absolute date to relative-event date
# extract year and month
library(lubridate)
ibes3_cast$YrMonth <- paste(year(ibes3_cast$STATPERS), '-', month(ibes3_cast$STATPERS), sep='')
mydata$YrMonth <- paste(year(mydata$AnnounceDate), '-', month(mydata$AnnounceDate), sep='')

window_choice <-function(temp){
  relative <- data.frame(matrix(ncol = 0, nrow = (temp*2+1)))
  relative <- as.data.frame(seq(from = -temp, to = temp, by=1))
  colnames(relative)[1] <- "time"
  for (i in 1:nrow(mydata)){
    ticker <- mydata[i, 'aibtic']
    for (j in (1+temp):(nrow(ibes3_cast)-temp)){
      if (mydata[i,'YrMonth'] == ibes3_cast[j,'YrMonth']){
        intm=as.data.frame(ibes3_cast[(j-temp):(j+temp),ticker])
        print("extract is ok")
        colnames(intm)[1] <- ticker
        #print("name is OK")
        #print(ticker)
        relative <- cbind(relative, intm)
        break
      }
    }
  }
  return(relative)
}

window_12 <-window_choice(12)
window_24 <- window_choice(24)

#take window_24 as an example
#drop columns with NA
window_24 <- window_24[ , colSums(is.na(window_24)) == 0]
#melt sample
window_24_melt <- melt(window_24, id.vars=1, variable.name="firm")
# set before-after variable
window_24_melt["BA"] <- "BEFORE"
for (i in 1:nrow(window_24_melt)){
  if (window_24_melt[i,"time"]>0){
    window_24_melt[i,"BA"]="AFTER"
  }
}

p <- ggplot(data = window_24_melt, aes(x=BA, y=value)) + geom_boxplot(aes(fill=BA))
p + facet_wrap( ~ BA, scales="free")

# choose the first ten cases as an example to draw pictures.
window_24_sample <- window_24_melt[1:490,]

library(ggplot2)
p <- ggplot(data = window_24_sample, aes(x=firm, y=value)) + geom_boxplot(aes(fill=BA))
p + facet_wrap( ~ firm, scales="free")

#choose observation window from (-24,-12) to (12,24)
window_24_sample <-window_24_melt[which((window_24_sample$time>12)|(window_24_sample$time<(-12))),]

## Boxplot
p <- ggplot(data = window_24_sample, aes(x=firm, y=value)) + geom_boxplot(aes(fill=BA))
p + facet_wrap( ~ firm, scales="free")

## merge industry information
window_24_melt["sic"] <- NA
for (i in 1: nrow(window_24_melt)){
  temporary <- mydata[which(mydata$aibtic==window_24_melt[i,"firm"]),]
  window_24_melt[i,"sic"] = temporary[1, "CurrentAcquirerSICCode"]
}

# devide by P
window_24_sample <-window_24_melt[which((window_24_melt$sic>=2999)&(window_24_melt$sic<=3500)),]
## Boxplot
p <- ggplot(data = window_24_sample, aes(x=sic, y=value)) + geom_boxplot(aes(fill=BA))
p + facet_wrap( ~ sic, scales="free")

#paired t-test (Wilcoxon test)

#levene test (Computes Levene's test for homeogeneity of variance across groups.)
library(car)
p_value = function(x,y,z){
  x=data.frame(x)
  y=data.frame(y)
  c=nrow(x)
  d=nrow(y)
  colnames(x)='variable'
  colnames(y)='variable'
  a=data.frame(rep('A',c))
  b=data.frame(rep('B',d))
  colnames(a)='group'
  colnames(b)='group'
  data=data.frame(variable=rbind(x,y),group=rbind(a,b))
  if(leveneTest(variable~group,data)$Pr[1] =='NaN'){
    print(NA)
  }else{
    if(leveneTest(variable~group,data)$Pr[1] >0.05){
      t.test(x,y,paired=z)$p.value
    }else{
      wilcox.test(data[1:c,1],data[(c+1):(c+d),1],paired=z)$p.value
    }
  }
}

p_value(before$NME,after$NME,FALSE) #FALSE for paired data
# because p_value = 0.3388815 > 0.05, we can use t-test.

########################
#Descriptive Statistics#
########################
summary(window_24[,1:10])










# 1.order the singlestock DataFrame
# 2.assign zero to the merger date
# 3.assign other related time number to them (-532,-531,....0....345,346)
# 4.get -24 to 24 window (what if there is not enough relative time cell?)





# cusip <- unique(c(mydata$Acquirercusip,mydata$Targetcusip,mydata$Sellercusip))
# save(cusip, file="cusip.RData")

#import IBES data
ibes <- read_csv("ibes_statsum_cusip8_international.csv", 
                 col_types = cols(ACTUAL = col_skip(), 
                                  ANNDATS_ACT = col_skip(), ANNTIMS_ACT = col_skip(), 
                                  CURCODE = col_skip(), CURR_ACT = col_skip(), 
                                  ESTFLAG = col_skip(), FISCALP = col_skip(), 
                                  FPEDATS = col_skip(), FPI = col_skip(), 
                                  MEASURE = col_skip(), OFTIC = col_skip(), 
                                  STATPERS = col_date(format = "%Y/%m/%d"), 
                                  TICKER = col_skip()), na = "empty")
# You should work with individual forecasts


#choose all the related cusip ibes data (don't need to choose, because we already chose it when downloading the original ibes)
#mydata1 <- mydata(which(mydata$Acquirercusip in ))

#prestep: form the acquistion date and time list (we have it now: mydata)

# we choose the time window
window_choice <- function(temp){
  relative = ibes[FALSE,]
  for (i in 1:length(mydata)){
    cusip <- mydata[i, 'Acquirercusip']
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

window3 = window_choice(3)




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




# #read combination table (Compustat and IBES. because both compustat and bloomberg use header CUSIP, 
# #we can use the table to combine bloomberg and IBES datasets.)
# slink <- read_csv("~/Desktop/Paper idea/4-bloomberg/security_link between IBES and Compustat_north america.csv", 
#                   na = "empty")
# slink <- slink[c('cusip','ibtic')]
# slinkd <- slink[duplicated(slink),]
# slink <- distinct(slink)
# 
# #select the rows in which cusip column is equal to cusip dataframe (only listed companies have ibtic).
# #the lost of tool rows because some companies may change cusip after merger and aquistion. 
# #but we use the ibtic to keep them.
# tool <- subset(slink, cusip %in% header$cusip)
# #check missing value
# #in header but not in slink
# tool1 <- subset(header, !(cusip %in% slink$cusip))



# # get all the listed north america companies identifier information from compustat database.
# # import the compustat all north america (listed and delisted, active trading) dataset.
# library(readr)
# Compustat <- read_csv("Compustat - cusip - north america.csv")
# View(Compustat_cusip_north_america)
# 
# cusip_compustat <- as.data.frame(Compustat$cusip, stringsAsFactors = FALSE)
# names(cusip_compustat) <- c('cusip')
# cusip_compustat <- unique(cusip_compustat)
# # which means there are totally 41973 firms in the markets.
# 
# #save the header cusip list from compustat
# write.table(cusip_compustat, file = "cusip_compustat_9.txt",row.names = FALSE, col.names = FALSE, sep="\t")



#get all the CRSP dataset (primary listings for the NYSE, NYSE MKT, NASDAQ, and Arca exchanges)
#read CRSP dataset
CRSP_id <- read_csv("~/Desktop/Paper idea/4-bloomberg/CRSP-identifier.csv", 
                    col_types = cols_only(COMNAM = col_guess(), 
                                          CUSIP = col_character(), NCUSIP = col_character(), 
                                          PERMNO = col_guess(), date = col_date(format = "%Y/%m/%d")), 
                    na = "empty")

cusip_crsp <- as.data.frame(CRSP_id$CUSIP, stringsAsFactors = FALSE)
cusip_crsp <- unique(cusip_crsp)

#create the historical NCUSIP list in US market
ncusip <- as.data.frame(CRSP_cusip$NCUSIP, stringsAsFactors = FALSE)
ncusip <- unique(ncusip)
colnames(ncusip)[1] <- 'ncusip'
ncusip$ncusip <- substr(ncusip$ncusip, start = 1, stop = 8)
ncusip <- as.data.frame(ncusip[!(is.na(ncusip$ncusip)),], stringsAsFactors = FALSE)

#save the ncusip file as txt and import it into IBES database
write.table(ncusip, file = "ncusip_CRSP_8.txt",row.names = FALSE, col.names = FALSE, sep="\t")

#create table (mydata$cusip -> ibes$TICKER)
#mutate(iris, sepal = Sepal.Length + Sepal. Width)

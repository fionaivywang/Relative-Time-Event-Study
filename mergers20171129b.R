#  1. sic classification is not clear
#  within or bettween industries, summary
# wihch industry merge less 1-10 10-20, try to understand why?
# no merger industries? why? control group? acquirer/targety  
#  2. FIRM 1 FRIM2 MONTH(0,1,2,3...) CODET1B CODET2B CODETA

df1 <- aggregate(sic2~asic2, data = mean_codet, FUN = sum)
df <- merge(sic, df1, by.x = c("Group"), by.y = c("asic2"), all = TRUE)


library(readr)
library(foreign)
library(ggplot2)
library(dplyr)
library(stringr)
library(reshape2)
## read.csv mergers
merger_part1 <- read_delim("research - M&A - both listed - north america - merger part1.csv", 
                           "\t", escape_double = FALSE, col_types = cols(`Amendment Date` = col_date(format = "%Y/%m/%d"), 
                                                                         `Announce Date` = col_date(format = "%Y/%m/%d"), 
                                                                         `Completion/Termination Date` = col_date(format = "%Y/%m/%d"),
                                                              #           'Announced Total Value (mil.)' = character,
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
# merger <- subset(merger,select=-c(DealDescription))

#remove all the spaces and other special signals in variable names
names(merger) <- gsub(" ", "", names(merger) , fixed = TRUE)
names(merger) <- gsub("/", "", names(merger) , fixed = TRUE)
names(merger) <- gsub("(mil.)", "", names(merger) , fixed = TRUE)


# select the specific variables 
merger <- subset(merger,select=c(AnnounceDate, CompletionTerminationDate, AnnouncedTotalValue, Sellercusip, Targetcusip, 
                                 DealAttributes, Acquirercusip, DealStatus, PercentOwned, PercentSought))

#test <- merger[duplicated(merger[,c("CompletionTerminationDate","Targetcusip","Acquirercusip")]),]
#merger1<-merger[duplicated(merger[,c("CompletionTerminationDate","Targetcusip","Acquirercusip")],fromLast = TRUE),]
#merger2<-merger[duplicated(merger[,c("CompletionTerminationDate","Targetcusip","Acquirercusip")],fromLast = FALSE),]
#merger0<-rbind(merger1, merger2)




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

# select the specific variables 
sic <- subset(sic,select=c(AnnounceDate, CompletionTerminationDate, Sellercusip, Targetcusip, 
                                 Acquirercusip, CurrentTargetSICCode, CurrentAcquirerSICCode, CurrentSellerSICCode))


mydata <- merge(merger,sic,by=c("AnnounceDate", "CompletionTerminationDate","Sellercusip", "Targetcusip", "Acquirercusip"),all.x=TRUE)

# remove cases that are not completed
mydata <- mydata[which(mydata$DealStatus=="Completed"),]
# rm(merger_part1, merger_part2, sic_part1, sic_part2)
# rm(merger,sic)

# get the duplicated cases and delete them (not sure the reason why there are duplications?)
merger1<-mydata[duplicated(mydata[,c("CompletionTerminationDate","Targetcusip","Acquirercusip")],fromLast = TRUE),]
merger2<-mydata[duplicated(mydata[,c("CompletionTerminationDate","Targetcusip","Acquirercusip")],fromLast = FALSE),]
merger0<-rbind(merger1, merger2)
x <- rbind(mydata, merger0)
mydata <- x[! duplicated(x, fromLast=TRUE) & seq(nrow(x)) <= nrow(mydata), ]
#rm(merger1,merger2,merger0,x)

#not sure the reason why there are NA for these companies?
mydata <- mydata[which(mydata$Targetcusip !="N.A."),]; mydata <- mydata[which(mydata$Acquirercusip !="N.A."),]
mydata <- mydata[which(mydata$Targetcusip !=""),]; mydata <- mydata[which(mydata$Acquirercusip !=""),]

mydata <- mydata[which(mydata$CurrentTargetSICCode!="N.A."),]; mydata <- mydata[which(mydata$CurrentAcquirerSICCode!="N.A."),] # DO SOMETHING THERE !
mydata <- mydata[which(mydata$CurrentTargetSICCode!=""),]; mydata <- mydata[which(mydata$CurrentAcquirerSICCode!=""),]


# #keep company takeover only (x to 100%)
# mydata <- mydata[which(grepl("Company Takeover", mydata$DealAttributes)),]

#check percent sought here
mydata$TotalOwned <- round(mydata$PercentOwned+mydata$PercentSought)
mydata <- mydata[which((mydata$TotalOwned==100)|(!grepl("Company Takeover", mydata$DealAttributes))),]


# Keep only the acquirer cusip that appears in the IBES dataset
mydata$Acquirercusip <- substr(mydata$Acquirercusip, start = 1, stop = 8)
mydata$Targetcusip <- substr(mydata$Targetcusip, start = 1, stop = 8)
mydata$Sellercusip <- substr(mydata$Sellercusip, start = 1, stop = 8)

# create a list containing all the related M&A cusip
cusip <- c(mydata$Acquirercusip,mydata$Targetcusip,mydata$Sellercusip)
cusip <- as.data.frame(cusip)
header <- unique(cusip)

header <- as.data.frame(header[which(header$cusip != "N.A."),], stringAsFactors = FALSE)
names(header) <- c('cusip')
header <- as.data.frame(header[which(header$cusip != ""),], stringAsFactors = FALSE)
names(header) <- c('cusip')
write.table(header, file = "cusip_8.txt",row.names = FALSE, col.names = FALSE, sep="\t")

##############
#IBES#########
##############
#import ibes (nstatsum_epsus.sas7bdat)
ibes <- read_csv("nstatsum_epsus.csv", 
                 col_types = cols(ACTDATS_ACT = col_date(format = "%m/%d/%Y"), 
                                  ANNDATS_ACT = col_date(format = "%m/%d/%Y"),
                                  ACTTIMS_ACT = col_skip(), ANNTIMS_ACT = col_skip(), MEASURE = col_skip(), #ANNDATS_ACT: earning annoucement date (1981.12 - 2016.12)
                                  ESTFLAG = col_skip(),
                                  FPI = col_character(),                                                    #ACTDATS_ACT: earning annoucement to be included in IBES date
                                  STATPERS = col_date(format = "%m/%d/%Y"),
                                  FPEDATS = col_date(format = "%m/%d/%Y")), na = "empty")
# summary(ibes)

#filter ibes datasets
# 1. choose the time period
ibes1 <- ibes[which(ibes$STATPERS > "1981-12-01" & ibes$STATPERS < "2017-01-01"),]
# 2. choose ANN/QTR/LTG
ibes2 <- ibes1[which(ibes1$FISCALP=="LTG"),]
# delete duplicated rows (no duplicated rows!)
# ibes2 <- unique(ibes2)  # it's too time-consuming, I have tested that there is no duplicated rows.

# PRE. choose companies (can only put it in this position)
# PRE. choose the related cusip (in header list) in ibes2 (in order to reduce loop times, because ibes is too large)
ibes_ticker <- unique(subset(ibes2[which(ibes2$CUSIP %in% header$cusip),],select=c(TICKER,CUSIP)))
# rm(header)

mydata <- merge(mydata,ibes_ticker,by.x=c("Targetcusip"), by.y=c("CUSIP"), all.x=TRUE)
names(mydata)[names(mydata)=="TICKER"] <- "tibtic"
mydata <- merge(mydata,ibes_ticker,by.x=c("Acquirercusip"), by.y=c("CUSIP"), all.x=TRUE)
names(mydata)[names(mydata)=="TICKER"] <- "aibtic"
mydata <- merge(mydata,ibes_ticker,by.x=c("Sellercusip"), by.y=c("CUSIP"), all.x=TRUE)
names(mydata)[names(mydata)=="TICKER"] <- "sibtic"
# rm(ibes_ticker)

mydata <- mydata[which(!is.na(mydata$aibtic)),]
mydata <- mydata[which(!is.na(mydata$tibtic)),]

# ibes_ticker <- ibes2[which(ibes2$CUSIP %in% header$cusip),]
# 
# namevector <- c("aibtic","tibtic","sibtic")
# mydata[,namevector] <- NA
# 
# for (i in 1:nrow(mydata)){
#   temporary <- ibes_ticker[which(ibes_ticker$CUSIP == mydata[i,'Acquirercusip']),]
#   mydata[i,"aibtic"]<- temporary[1,'TICKER']
# }
# 
# for (i in 1:nrow(mydata)){
#   temporary <- ibes_ticker[which(ibes_ticker$CUSIP == mydata[i,'Targetcusip']),]
#   mydata[i,"tibtic"]<- temporary[1,'TICKER']
# }
# 
# mydata <- mydata[which(!is.na(mydata$aibtic)),]



# 3.1 choose ibes data with merger cases
ibes3 <- ibes2[which(ibes2$TICKER %in% mydata$aibtic|ibes2$TICKER %in% mydata$tibtic),]



# extract year and month
ibes3$YrMonth <- format(ibes3$STATPERS, format="%Y-%m")
mydata$YrMonth <- format(mydata$AnnounceDate, format="%Y-%m")
mydata$YrMonth_C <- format(mydata$CompletionTerminationDate, format="%Y-%m")

# # extract year and month
# library(lubridate)
# ibes3$YrMonth <- paste(year(ibes3_melt$STATPERS), '-', month(ibes3_melt$STATPERS), sep='')
# mydata$YrMonth <- paste(year(mydata$AnnounceDate), '-', month(mydata$AnnounceDate), sep='')


# 3.2 change the dataset from long to wide

#ibes3_melt <- subset(ibes3, select = c("STATPERS", "TICKER"))
ibes3_cast <- dcast(ibes3, YrMonth ~ TICKER, value.var='YrMonth')
# ibes3_melt <- melt(ibes3, id=c("STATPERS", "TICKER"))
# # cast the melted data
# ibes3_std <- subset(ibes3, select = -c(MEANEST, NUMEST))
# ibes3_cast <- as.data.frame(ibes3_cast)

#4 change the date from absolute date to relative-event date for aibtic
window_choice <-function(x, temp){
  relative <- data.frame(matrix(ncol = 0, nrow = (temp*2+1)))
  relative <- as.data.frame(seq(from = -temp, to = temp, by=1))
  colnames(relative)[1] <- "time"
  for (i in 1:nrow(mydata)){
    ticker <- mydata[i, x]
    #print("i")
    for (j in (1+temp):(nrow(ibes3_cast)-temp)){
      #print("j")
      if (mydata[i,'YrMonth'] == ibes3_cast[j,'YrMonth']){
        #print("if is ok")
        intm=as.data.frame(ibes3_cast[(j-temp):(j+temp),ticker])
        #print("extract is ok")
        #print(ticker)
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

x <- 'aibtic'
#window_12 <-window_choice(12)
window_24 <- window_choice(x, 24)
x <- 'tibtic'
window_24_t <- window_choice(x, 24)

window_24 <- window_24[ , colSums(is.na(window_24))<100]
test <- window_24[!duplicated(window_24$time), ]
#melt sample
window_24_melt <- melt(window_24, id.vars="time", variable.name = "TICKER", value.name = "YrMonth", factorsAsStrings = F)


window_24_t <- window_24_t[ , colSums(is.na(window_24_t))<100]
#melt sample
window_24_t_melt <- melt(window_24_t, id.vars="time", variable.name = "TICKER", value.name = "YrMonth", factorsAsStrings = F)

window_24_melt <- cbind(window_24_melt, window_24_t_melt)
colnames(window_24_melt) <-c("time","aibtic","aYrMonth","time1","tibtic","tYrMonth")
window_24_melt <- subset(window_24_melt, select = -(time1))

library(magrittr)
# create a group number for each merger case group
window_24_melt$case <- window_24_melt %>% group_indices(aibtic, tibtic)

# remove TICKER name string after"." (eg. ITG.1 to ITG)
window_24_melt$aibtic <- gsub("\\..*","",window_24_melt$aibtic)
window_24_melt$tibtic <- gsub("\\..*","",window_24_melt$tibtic)

# #################################
# # This part is to find a link betweeen acquirer and target, so that we can merge the information
# #################################
# #assign case number to mydata
# mydata$case <- row(mydata)
# 
# #take window_24 as an example
# #drop columns with NA
# window_24na <- window_24[ , colSums(is.na(window_24)) == 0]
# #melt sample
# window_24_melt <- melt(window_24na, id.vars="time", variable.name = "TICKER", value.name = "YrMonth", factorsAsStrings = F)
# #assign each case a case number (Factor variables are integers internally. Thus, it is easy to turn them into integer variables.)
# window_24_melt$case <- as.integer(window_24_melt$TICKER)
# #remove TICKER name string after"." (eg. ITG.1 to ITG)
# window_24_melt$TICKER <- gsub("\\..*","",window_24_melt$TICKER)
# #match mydata$case to window_24_melt
# mydata_case <- subset(mydata, select=c(YrMonth, aibtic, tibtic, case))
# window_24_meltcase <- window_24_melt[which(window_24_melt$time==0),]
# window_24_key <- window_24_melt[which(window_24_melt$time==0),]
# window_24_key <- window_24_key[!duplicated(window_24_key[, c("TICKER","YrMonth","tibtic")]),]
# 
# window_24_meltcase <- merge(window_24_meltcase, mydata_case, by.x=c("TICKER", "YrMonth"), by.y=c("aibtic", "YrMonth"),all.x=TRUE)
# window_24_meltcase <- window_24_meltcase[!duplicated(window_24_meltcase[, c("TICKER","YrMonth","tibtic")]),]
# window_24_meltcase <- window_24_meltcase[order(window_24_meltcase$case.x),]
# window_24_meltcase$case.x <- row(window_24_meltcase)
# window_24_meltcase <- window_24_meltcase[,c("case.x","case.y","tibtic")]
# #test <-window_24_meltcase[which(duplicated(window_24_meltcase[,c("case.x","case.y")])),]
# window_24_melt_f <-merge(window_24_melt, window_24_meltcase, by.x=c("case"), by.y=c("case.x"), all.x=TRUE)
# window_24_melt_f <- window_24_melt_f[which(!duplicated(window_24_melt_f)),]

# #take window_24_t as an example
# #drop columns with NA
# window_24_tna <- window_24_t[ , colSums(is.na(window_24_t[1:24,])) == 0]
# #melt sample
# window_24_t_melt <- melt(window_24_tna, id.vars="time", variable.name = "TICKER", value.name = "YrMonth")
# #assign each case a case number (Factor variables are integers internally. Thus, it is easy to turn them into integer variables.)
# window_24_t_melt$case <- as.integer(window_24_t_melt$TICKER)
# #remove TICKER name string after"." (eg. ITG.1 to ITG)
# window_24_t_melt$TICKER <- gsub("\\..*","",window_24_t_melt$TICKER)


# merge required data on the "relative time ~ absolute time" table from IBES
# if YrMonth&TICKER is equal, merge the ibes data (both for acquirer and target)
analyse <- merge(window_24_melt,ibes3, by.x=c("aYrMonth", "aibtic"), by.y=c("YrMonth","TICKER"), all.x=TRUE)
analyse <- merge(analyse,ibes3, by.x=c("tYrMonth", "tibtic"), by.y=c("YrMonth","TICKER"), all.x=TRUE)

# set before-after variable
analyse["BA"] <- "BEFORE"
for (i in 1:nrow(analyse)){
  if (analyse[i,"time"]>0){
    analyse[i,"BA"]="AFTER"
  }
}

#create covar variable (coefficient of variance is std/mean)
analyse$codet.x <- analyse$STDEV.x / analyse$MEANEST.x
#analyse_at$lcodet.x <- log(analyse_at$codet.x)
analyse$codet.y <- analyse$STDEV.y / analyse$MEANEST.y
#analyse_at$lcodet.y <- log(analyse_at$codet.y)

# create equal-weighted STDEV with acquirer and target before merger
# analyse_at$eqstdev <- analyse_at$STDEV.x
# analyse_at[which(analyse_at$BA=="BEFORE"),]$eqstdev <- (analyse_at[which(analyse_at$BA=="BEFORE"),]$STDEV.x + analyse_at[which(analyse_at$BA=="BEFORE"),]$STDEV.y)/2

# choose the variables that we are interested in
analyse_subset <- subset(analyse, select = c(time, BA, case, aibtic, CUSIP.x, aYrMonth, tibtic, CUSIP.y, tYrMonth, codet.x, codet.y))

## form the X mean before and after
# codet.x = acquirer's value, codet.y = target's value
analyse_subset_melt <- melt(analyse_subset, id.vars = c("time", "BA", "case", "aibtic", "CUSIP.x", "aYrMonth", "tibtic", "CUSIP.y", "tYrMonth"), variable.name = "a_or_t", value.name = "codet")
# get the mean value for two specific observation window (eg: (-24, -14),  (5, 10)) FUNCTION
# (we need four parameters to choose the comparing window.)
compare_choice <-function(before1, before2, after1, after2){
  analyse_compare <- analyse_subset_melt[which((analyse_subset_melt$time>=before1&analyse_subset_melt$time<=before2)|(analyse_subset_melt$time>=after1&analyse_subset_melt$time<=after2)),]
  sample1_mean <- dcast(analyse_compare, aibtic + tibtic + case ~ BA + a_or_t , value.var = "codet", mean, na.rm = TRUE)
  return(sample1_mean)
}

## HERE YOU HAVE TO INPUT SOMETHING
mean_codet <- compare_choice(-12,-6,6,12)

# assign cusip to mean_codet
for (i in 1:nrow(mean_codet)){
  temporary <- mydata[which(mydata$aibtic == mean_codet[i, "aibtic"]),]
  mean_codet[i, "ACUSIP"] <- temporary[1, "Acquirercusip"]
}

for (i in 1:nrow(mean_codet)){
  temporary <- mydata[which(mydata$tibtic == mean_codet[i, "tibtic"]),]
  mean_codet[i, "TCUSIP"] <- temporary[1, "Targetcusip"]
}

#not sure about this part, need to be recheck again?
for (i in 1:nrow(mean_codet)){
  temporary <- mydata[which((mydata$tibtic == mean_codet[i, "tibtic"])&(mydata$aibtic == mean_codet[i, "aibtic"])),]
  mean_codet[i, "TotalValue"] <- temporary[1, "AnnouncedTotalValue"]
}


# import SIC data
library(readr)
msf <- read_csv("msf.csv", 
                col_types = cols(ALTPRC = col_skip(), 
                                 ALTPRCDT = col_skip(), ASK = col_skip(), 
                                 ASKHI = col_skip(), BID = col_skip(), 
                                 BIDLO = col_skip(), CFACPR = col_skip(), 
                                 CFACSHR = col_skip(), DATE = col_skip(), 
                                 HEXCD = col_skip(), ISSUNO = col_skip(), 
                                 PERMCO = col_skip(), PERMNO = col_skip(), 
                                 PRC = col_skip(), RET = col_skip(), 
                                 RETX = col_skip(), SHROUT = col_skip(), 
                                 SPREAD = col_skip(), VOL = col_skip()), 
                na = "empty")

msf <- unique(msf)

mean_codet <- merge(mean_codet, msf, by.x ="ACUSIP", by.y = "CUSIP", all.x=TRUE)
mean_codet <- merge(mean_codet, msf, by.x = "TCUSIP", by.y = "CUSIP", all.x=TRUE)

# delete or not?
mean_codet <- mean_codet[which(mean_codet$TotalValue !="N/A"),]
mean_codet <- mean_codet[which(mean_codet$TotalValue !="NA"),]

# industry dummy variable
mean_codet$sic2 <- 0
mean_codet[which(substr(mean_codet$HSICCD.x, start = 1, stop = 2) == substr(mean_codet$HSICCD.y, start = 1, stop = 2)),"sic2"] <- 1
mean_codet$sic1 <- 0
mean_codet[which(substr(mean_codet$HSICCD.x, start = 1, stop = 1) == substr(mean_codet$HSICCD.y, start = 1, stop = 1)),"sic1"] <- 1

# create 2-digit sic code
mean_codet$asic2 <- substr(mean_codet$HSICCD.x, start = 1, stop = 2)
mean_codet$tsic2 <- substr(mean_codet$HSICCD.y, start = 1, stop = 2)

# run regressions
mean_codet$TotalValue <- as.numeric(mean_codet$TotalValue)
fit <- lm(AFTER_codet.x ~ BEFORE_codet.x + BEFORE_codet.y + TotalValue + sic1 + sic2, data=mean_codet)
summary(fit)

fit1 <- lm(AFTER_codet.x ~ BEFORE_codet.x + BEFORE_codet.y, data=mean_codet)
summary(fit1)

fit2 <- lm(AFTER_codet.x ~ BEFORE_codet.x + BEFORE_codet.y + HSICCD.x + HSICCD.y, data=mean_codet)
summary(fit2)

fit3 <- lm(AFTER_codet.x ~ BEFORE_codet.x + BEFORE_codet.y + TotalValue, data = mean_codet)
summary(fit3)



library(readxl)
sic <- read_excel("sic.xlsx")

a <- as.data.frame(table(mean_codet$asic2))
b <- as.data.frame(table(mean_codet$tsic2))

sic <- merge(sic, a, by.x = "Group", by.y = "Var1", all.x = TRUE)
sic <- merge(sic, b, by.x = "Group", by.y = "Var1", all.x = TRUE)

ind_wo_merger <- sic[which(is.na(sic$Freq.x)&is.na(sic$Freq.y)),]

## If we don't use mean to run the regression ( aibtic/tibtic + case~ time)
time_dcast <- dcast(analyse_compare, case+aibtic+CUSIP.x+tibtic+CUSIP.y +a_or_t ~ time, value.var = "codet", na.rm = TRUE)


###############################################
# sample2 <- compare_choice_at(-24,-12,12,24)
# ## function for drawing the before-after scatterplot by dropping outliers (from 1% to 99%)
# scatter_ba_outlier <- function(x){
#   ## form the X mean before and after
#   sample1_mean <- dcast(sample1, TICKER + case.y ~ BA, value.var = x, mean, na.rm = TRUE)
#   #drop if before or after is equal to NaN
#   sample1_mean <- sample1_mean[which(!is.na(sample1_mean$AFTER)&!is.na(sample1_mean$BEFORE)),]
#   # drop outiliers that are from 1% to 99%
#   sample1_mean <- sample1_mean[which(sample1_mean$BEFORE>=quantile(sample1_mean$BEFORE, probs = 0.01, na.rm = TRUE)&sample1_mean$BEFORE<=quantile(sample1_mean$BEFORE, probs = 0.99, na.rm = TRUE)),]
#   sample1_mean <- sample1_mean[which(sample1_mean$AFTER>=quantile(sample1_mean$AFTER, probs = 0.01, na.rm = TRUE)&sample1_mean$AFTER<=quantile(sample1_mean$AFTER, probs = 0.99, na.rm = TRUE)),]
#   
#   #scatter plot for before and after per case with 45 degree line
#   ## Scatter plot with linear
#   ggplot(sample1_mean,
#          aes(x = BEFORE, y = AFTER)) + geom_point(size=1) +
#     geom_abline(slope=1, intercept=0) + 
#     geom_smooth(data = sample1_mean,
#                 aes(x = BEFORE, y = AFTER, color=case), method = "lm", se = FALSE) +
#     ylab('') + ggtitle('Scatterplot for before and after') + scale_colour_manual(values=groupPalette)
#   #ggsave(paste(repertoire_travail, repertoire_resultat,"scatterplot_3group.pdf", sep='/'), width = 4, height = 4)
# }
# # choose the object that we want to analyze
# x <- 'eqstdev'
# scatter_ba_outlier(x)
# x <- 'MEANEST'
# scatter_ba_outlier(x)
# x <- 'codet'
# scatter_ba_outlier(x)
#######################

# set before-after variable
analyse["BA"] <- "BEFORE"
for (i in 1:nrow(analyse)){
  if (analyse[i,"time"]>0){
    analyse[i,"BA"]="AFTER"
  }
}

#create covar variable
analyse$codet <- analyse$STDEV / analyse$MEANEST
analyse$lcodet <- log(analyse$codet)

#draw the general boxplot
p <- ggplot(data = analyse, aes(x=BA, y=STDEV)) + geom_boxplot(aes(fill=BA))
p + facet_wrap( ~ BA, scales="free")

# get the mean value for two specific observation window (eg: (-24, -14),  (5, 10)) FUNCTION
# (we need four parameters to choose the comparing window.)
compare_choice <-function(before1, before2, after1, after2){
  analyse_compare <- analyse[which((analyse$time>=before1&analyse$time<=before2)|(analyse$time>=after1&analyse$time<=after2)),]
  return(analyse_compare)
}

sample1 <- compare_choice(-12,-1,1,12)
## function for drawing the before-after scatterplot
scatter_ba <- function(x){
  ## form the X mean before and after
  sample1_mean <- dcast(sample1, TICKER + case ~ BA, value.var = x, mean, na.rm = TRUE)
  
  #drop if before or after is equal to NaN
  sample1_mean <- sample1_mean[which(!is.na(sample1_mean$AFTER)&!is.na(sample1_mean$BEFORE)),]
  
  #scatter plot for before and after per case with 45 degree line
  ## Scatter plot with linear
  ggplot(sample1_mean,
         aes(x = BEFORE, y = AFTER)) + geom_point(size=1) +
    geom_abline(slope=1, intercept=0) + 
    geom_smooth(data = sample1_mean,
                aes(x = BEFORE, y = AFTER, color=case), method = "lm", se = FALSE) +
    ylab('') + ggtitle('Scatterplot for before and after') + scale_colour_manual(values=groupPalette)
  #ggsave(paste(repertoire_travail, repertoire_resultat,"scatterplot_3group.pdf", sep='/'), width = 4, height = 4)
}
# choose the object that we want to analyze
x <- 'STDEV'
scatter_ba(x)
x <- 'MEANEST'
scatter_ba(x)
x <- 'codet'
scatter_ba(x)


sample1 <- compare_choice(-24,-12,12,24)
## function for drawing the before-after scatterplot by dropping outliers (from 1% to 99%)
scatter_ba_outlier <- function(x){
  ## form the X mean before and after
  sample1_mean <- dcast(sample1, TICKER + case ~ BA, value.var = x, mean, na.rm = TRUE)
  #drop if before or after is equal to NaN
  sample1_mean <- sample1_mean[which(!is.na(sample1_mean$AFTER)&!is.na(sample1_mean$BEFORE)),]
  # drop outiliers that are from 1% to 99%
  sample1_mean <- sample1_mean[which(sample1_mean$BEFORE>=quantile(sample1_mean$BEFORE, probs = 0.01, na.rm = TRUE)&sample1_mean$BEFORE<=quantile(sample1_mean$BEFORE, probs = 0.99, na.rm = TRUE)),]
  sample1_mean <- sample1_mean[which(sample1_mean$AFTER>=quantile(sample1_mean$AFTER, probs = 0.01, na.rm = TRUE)&sample1_mean$AFTER<=quantile(sample1_mean$AFTER, probs = 0.99, na.rm = TRUE)),]
  
  #scatter plot for before and after per case with 45 degree line
  ## Scatter plot with linear
  ggplot(sample1_mean,
         aes(x = BEFORE, y = AFTER)) + geom_point(size=1) +
    geom_abline(slope=1, intercept=0) + 
    geom_smooth(data = sample1_mean,
                aes(x = BEFORE, y = AFTER, color=case), method = "lm", se = FALSE) +
    ylab('') + ggtitle('Scatterplot for before and after') + scale_colour_manual(values=groupPalette)
  #ggsave(paste(repertoire_travail, repertoire_resultat,"scatterplot_3group.pdf", sep='/'), width = 4, height = 4)
}
# choose the object that we want to analyze
x <- 'STDEV'
scatter_ba_outlier(x)
x <- 'MEANEST'
scatter_ba_outlier(x)
x <- 'codet'
scatter_ba_outlier(x)

sample1 <- compare_choice_at(-6,-3,3,6)
## form the X mean before and after
sample1_mean <- dcast(sample1, TICKER + case.y ~ BA, value.var = 'codet', mean, na.rm = TRUE)

#drop if before or after is equal to NaN
sample1_mean <- sample1_mean[which(!is.na(sample1_mean$AFTER)&!is.na(sample1_mean$BEFORE)),]




## function for drawing the before-after scatterplot by dropping outliers (from 1% to 99%)
scatter_ba_outlier <- function(x){
  ## form the X mean before and after
  sample1_mean <- dcast(sample1, TICKER + case ~ BA, value.var = x, mean, na.rm = TRUE)
  #drop if before or after is equal to NaN
  sample1_mean <- sample1_mean[which(!is.na(sample1_mean$AFTER)&!is.na(sample1_mean$BEFORE)),]
  # drop outiliers that are from 1% to 99%
  sample1_mean <- sample1_mean[which(sample1_mean$BEFORE>=quantile(sample1_mean$BEFORE, probs = 0.01, na.rm = TRUE)&sample1_mean$BEFORE<=quantile(sample1_mean$BEFORE, probs = 0.99, na.rm = TRUE)),]
  sample1_mean <- sample1_mean[which(sample1_mean$AFTER>=quantile(sample1_mean$AFTER, probs = 0.01, na.rm = TRUE)&sample1_mean$AFTER<=quantile(sample1_mean$AFTER, probs = 0.99, na.rm = TRUE)),]
  
  #scatter plot for before and after per case with 45 degree line
  ## Scatter plot with linear
  ggplot(sample1_mean,
         aes(x = BEFORE, y = AFTER)) + geom_point(size=1) +
    geom_abline(slope=1, intercept=0) + 
    geom_smooth(data = sample1_mean,
                aes(x = BEFORE, y = AFTER, color=case), method = "lm", se = FALSE) +
    ylab('') + ggtitle('Scatterplot for before and after') + scale_colour_manual(values=groupPalette)
  #ggsave(paste(repertoire_travail, repertoire_resultat,"scatterplot_3group.pdf", sep='/'), width = 4, height = 4)
}
# choose the object that we want to analyze
x <- 'STDEV'
scatter_ba_outlier(x)
x <- 'MEANEST'
scatter_ba_outlier(x)
x <- 'codet'
scatter_ba_outlier(x)


## form the X mean before and after
sample1_mean <- dcast(sample1, TICKER + case ~ BA, value.var = x, mean, na.rm = TRUE)

#drop if before or after is equal to NaN
sample1_mean <- sample1_mean[which(!is.na(sample1_mean$AFTER)&!is.na(sample1_mean$BEFORE)),]


test <- lm()






## merge industry information
sample1_mean <- dcast(analyse, TICKER + case ~ BA, value.var = 'STDEV', mean, na.rm = TRUE)
analyse_x_sic <- function(x){
  ## form the X mean before and after
  sample1_mean <- dcast(analyse, TICKER + case ~ BA, value.var = x, mean, na.rm = TRUE)
  #drop if before or after is equal to NaN
  sample1_mean <- sample1_mean[which(!is.na(sample1_mean$AFTER)&!is.na(sample1_mean$BEFORE)),]
  # drop outiliers that are from 1% to 99%
  sample1_mean <- sample1_mean[which(sample1_mean$BEFORE>=quantile(sample1_mean$BEFORE, probs = 0.01, na.rm = TRUE)&sample1_mean$BEFORE<=quantile(sample1_mean$BEFORE, probs = 0.99, na.rm = TRUE)),]
  sample1_mean <- sample1_mean[which(sample1_mean$AFTER>=quantile(sample1_mean$AFTER, probs = 0.01, na.rm = TRUE)&sample1_mean$AFTER<=quantile(sample1_mean$AFTER, probs = 0.99, na.rm = TRUE)),]
  #add industry related information
  sample1_mean["sic"] <- NA
  for (i in 1: nrow(sample1_mean)){
    temporary <- mydata[which(mydata$aibtic==sample1_mean[i,"TICKER"]),]
    sample1_mean[i,"sic"] = temporary[1, "CurrentAcquirerSICCode"]
  }
}





# use first two digits of sic to group the cases
sample1_mean$sic2 <- substr(sample1_mean$sic, start = 1, stop = 2)
table(sample1_mean$sic2)
# use the first digit of sic to group the cases
sample1_mean$sic1 <- substr(sample1_mean$sic, start = 1, stop = 1)

# if acquirer and target has the same 2-digit sic number, we consider it is a horizontal merger case.
sample1_mean$horizontal <- 

#draw the scatterplot by 2-digit sic group again
## Scatter plot with linear (in one graph)
ggplot(sample1_mean,
       aes(x = BEFORE, y = AFTER, color=sic2)) + geom_point(size=1) +
  geom_abline(slope=1, intercept=0) +
  ylab('') + ggtitle('Scatterplot for before and after DISPERSION')
#ggsave(paste(repertoire_travail, repertoire_resultat,"scatterplot_3group.pdf", sep='/'), width = 4, height = 4)



#draw the scatterplot by 1-digit sic group again
## Scatter plot with linear (in sic1 level)
ggplot(sample1_mean,
       aes(x = BEFORE, y = AFTER, color=sic2)) + geom_point(size=1) +
  geom_abline(slope=1, intercept=0) +
  facet_wrap(~sic1, scales = 'free') +
  ylab('') + ggtitle('Scatterplot for before and after DISPERSION')
#ggsave(paste(repertoire_travail, repertoire_resultat,"scatterplot_3group.pdf", sep='/'), width = 4, height = 4)

ggplot(sample1_mean,
       aes(x = BEFORE, y = AFTER, color=case)) + geom_point(size=1) +
  geom_abline(slope=1, intercept=0) +
  facet_wrap(~sic1, scales = 'free') +
  ylab('') + ggtitle('Scatterplot for before and after DISPERSION')
#ggsave(paste(repertoire_travail, repertoire_resultat,"scatterplot_3group.pdf", sep='/'), width = 4, height = 4)

test <- lm()


#draw the scatterplot by 2-digit sic group again
## Scatter plot with linear
ggplot(sample1_mean,
       aes(x = BEFORE, y = AFTER, color=sic2)) + geom_point(size=1) +
  geom_abline(slope=1, intercept=0) +
  facet_wrap(~sic2, scales = 'free') +
  ylab('') + ggtitle('Scatterplot for before and after DISPERSION')
#ggsave(paste(repertoire_travail, repertoire_resultat,"scatterplot_3group.pdf", sep='/'), width = 4, height = 4)




p <- ggplot(data = window_24_sample, aes(x=firm, y=value)) + geom_boxplot(aes(fill=BA))
p + facet_wrap( ~ firm, scales="free")


## Boxplot
p <- ggplot(data = window_24_sample, aes(x=firm, y=value)) + geom_boxplot(aes(fill=BA))
p + facet_wrap( ~ firm, scales="free")



# devide by P
window_24_sample <-window_24_melt[which((window_24_melt$sic>=2999)&(window_24_melt$sic<=3500)),]
## Boxplot
p <- ggplot(data = window_24_sample, aes(x=sic, y=value)) + geom_boxplot(aes(fill=BA))
p + facet_wrap( ~ sic, scales="free")




mysample <- analyse[analyse$NUMEST>=2,]
mysample <- mysample[order(mysample$case),]
#create covar variable
analyse$codet <- analyse$STDEV / analyse$MEANEST

analyse$lcodet <- log(analyse$codet)

plot(analyse$time[analyse$case==15], analyse$codet[analyse$case==15],type="l")
for ( i in unique(analyse$case)){
  lines(analyse$time[analyse$case==i], analyse$codet[analyse$case==i])
}


plot(analyse$time[analyse$case==1], analyse$lcodet[analyse$case==1],type="l")

mysample <- subset(analyse,select=c(time, case,STDEV))
mysample3 <- aggregate(STDEV ~ time, data=mysample, FUN=mean, na.rm=TRUE)



plot(mysample3$time, mysample3$STDEV,type="l")
abline(v=0,col="red")













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

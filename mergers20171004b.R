

library(readr)
setwd("/Users/bin/Desktop/Paper idea/4-bloomberg")

merger <- read_delim("research - M&A - both listed - north america.csv","\t", escape_double = FALSE, trim_ws = TRUE)
sic <- read_delim("research - M&A - both listed - north america - sic.csv", "\t", escape_double = FALSE, trim_ws = TRUE)



##clean the two datasets
#get the duplicated cases
merger1<-merger[duplicated(merger[,c("Completion/Termination Date","Target cusip","Acquirer cusip")],fromLast = TRUE),]
merger2<-merger[duplicated(merger[,c("Completion/Termination Date","Target cusip","Acquirer cusip")],fromLast = FALSE),]
merger0<-rbind(merger1, merger2)

##how to remove data from a dataframeA based on a dataframeB???

#do the same thing to the sic dataset
#get the duplicated cases
sic1<-sic[duplicated(sic[,c("Completion/Termination Date","Target cusip","Acquirer cusip")],fromLast = TRUE),]
sic2<-sic[duplicated(sic[,c("Completion/Termination Date","Target cusip","Acquirer cusip")],fromLast = FALSE),]
sic0<-rbind(sic1, sic2)

##how to remove data from a dataframeA based on a dataframeB???

#try to search the stories behind the duplications!


#merge the two datasets (based on the data clean before, there will be no duplications).
mydata <- merge(merger, sic, by=c("Completion/Termination Date","Target cusip","Acquirer cusip"), all=TRUE)
mydata <- mydata[order(mydata["Completion/Termination Date"]),]



#search for the duplicated rows, include the original ones (all the possible duplicated rows)
#a<-as.character(merger1[,"Completion/Termination Date"])
#b<-as.character(merger1[,"Target cusip"])
#c<-as.character(merger1[,"Acquirer cusip"])

#a<-gsub('\'&',','',b)
#It doesn't work
#dup<-subset(merger, merger["Completion/Termination Date"] %in% c)
#dup1<-subset(merger, merger["Completion/Termination Date"]=="1999/2/1"&merger["Target cusip"]=="N.A."&merger["Acquirer cusip"]=="37938J107")
#dup2<-subset(merger, merger["Completion/Termination Date"]=="1998/10/1"&merger["Target cusip"]=="865593107"&merger["Acquirer cusip"]=="989701107")
#dup2<-subset(merger, merger["Completion/Termination Date"]=="1998/10/1"&merger["Target cusip"]=="865593107"&merger["Acquirer cusip"]=="989701107")

#edit(merger0)


##Filter Data

library(dplyr)
subdata1<-filter(data, data["Deal Status"] == "Completed" )
View(subdata1)





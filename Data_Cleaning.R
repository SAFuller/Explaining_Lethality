### Data Cleaning script
### Sam Fuller


##Load Libraries
library(readr)
library(countrycode)
library(magrittr)
library(tidyr)

##Load BAAD2
BAAD2_Insurgency_Crime_Dataset <- read_delim("BAAD2 Insurgency Crime Dataset.tab", 
                                             "\t", escape_double = FALSE, trim_ws = TRUE)

##Load Global Terrorism data base
gtd<-read_csv("globalterrorismdb.csv", col_types = cols(iyear = col_date(format = "%Y")))

##Number of attacks/group-year
gtd1<-aggregate(eventid~gname*iyear, data=gtd, FUN=length)
names(gtd1)[names(gtd1) == 'eventid'] <- 'numattack'

gtd2<-gtd1 %>%
  complete(iyear = seq.Date(min(iyear), max(iyear), by="year"),gname)
gtd2$numattack[is.na(gtd2$numattack)]<-0


#Number of casualties/group-year
gtd3<-aggregate(nkill~gname*iyear, data=gtd, FUN=sum)

gtd4<-gtd3 %>%
  complete(iyear = seq.Date(min(iyear), max(iyear), by="year"),gname)
gtd4$nkill[is.na(gtd4$nkill)]<-0

##Merge together attack and killdata

groupyear<-merge(gtd2,gtd4, by=c("gname","iyear"))


##Change year format and merge with BAAD
groupyear$year<-substr(groupyear$iyear,1,4)%>%as.numeric()

data<-merge(BAAD2_Insurgency_Crime_Dataset,groupyear, by.x=c("org","year"), by.y=c("gname", "year"))

##Write Clean data in new .csv

write.csv(data,"BAADxGTD.csv")

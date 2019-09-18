##Sam Fuller
##EDTG


#load packages
library(readxl)
library(readr)
library(data.table)
library(MASS)
library(pscl)
library(zoo)
library(dplyr)
library(tidyr)
library(RcppRoll)
library(stargazer)
library(countrycode)
#load data
EDTG<- read_excel("EDTG_Data.xls")

#Round non-integers
EDTG$nonterr_casualties<-ceiling(EDTG$nonterr_casualties)

##Hierarchial Interactions
EDTG$policyH<-NA
EDTG$policyH<-(EDTG$pch*EDTG$lead_hierarch)

EDTG$policy<-NA
EDTG$policy[EDTG$pch==1 & EDTG$lead_hierarch==0]<-1
EDTG$policy[EDTG$pch==1 & EDTG$lead_hierarch==1]<-0
EDTG$policy[EDTG$pch==0]<-0

EDTG$territoryH<-NA
EDTG$territoryH<-(EDTG$tch*EDTG$lead_hierarch)

EDTG$territory<-NA
EDTG$territory[EDTG$tch==1 & EDTG$lead_hierarch==0]<-1
EDTG$territory[EDTG$tch==1 & EDTG$lead_hierarch==1]<-0
EDTG$territory[EDTG$tch==0]<-0

EDTG$squoH<-NA
EDTG$squoH<-(EDTG$sq*EDTG$lead_hierarch)

EDTG$squo<-NA
EDTG$squo[EDTG$sq==1 & EDTG$lead_hierarch==0]<-1
EDTG$squo[EDTG$sq==1 & EDTG$lead_hierarch==1]<-0
EDTG$squo[EDTG$sq==0]<-0

EDTG$systemH<-NA
EDTG$systemH<-(EDTG$ercsr*EDTG$lead_hierarch)

EDTG$system<-NA
EDTG$system[EDTG$ercsr==1 & EDTG$lead_hierarch==0]<-1
EDTG$system[EDTG$ercsr==1 & EDTG$lead_hierarch==1]<-0
EDTG$system[EDTG$ercsr==0]<-0

##LOAD COntrols and merge

population <- read_csv("population.csv")

##Reformat World bank data
rownames(population) <- population$Year
pop <- setNames(as.data.frame.table(as.matrix(population[-1])),c("cname","year","pop"))
pop$cname <-rownames(population)

gdp<- read_csv("GDP.csv")

##Reformat world bank data
rownames(gdp) <- gdp$Year
GDP <- setNames(as.data.frame.table(as.matrix(gdp[-1])),c("cname","year","gdp"))
GDP$cname <-rownames(gdp)

##Merge world bank data
countryf<-merge(pop, GDP, by=c("cname","year"))
countryf$cc<-countrycode(countryf$cname, "country.name", "cown")


##Recode west bank and gaza new ccode
countryf$cc[countryf$cname=="West Bank and Gaza"]<-667

##Give serbia a ccode
countryf$cc[countryf$cname=="Serbia"]<-348

##Give Puerto Rico a ccode
countryf$cc[countryf$cname=="Puerto Rico"]<-3

##Fix North Korea
countryf$cc[countryf$cname=="Korea, Dem. People???s Rep."]<-731

countryf<-na.omit(countryf)

##Load vdem data
vdem <- read_csv("V-Dem-CY-Core-v9.csv")
vdem<- subset(vdem, year >1969 & year < 2017)
vdem<-subset(vdem, select=c("country_name" ,"year" ,"country_text_id",
                            "v2x_freexp_altinf", "v2xcl_rol", "v2x_frassoc_thick", "v2xeg_eqdr", "v2x_libdem"))

vdem$cc<-countrycode(vdem$country_name, "country.name", "cown")
vdem$cc[vdem$country_name=="Palestine/West Bank"]<-667
vdem$cc[vdem$country_name=="Palestine/Gaza"]<-667

##This is to creat a democracy measure for westbank and Gaza together
vdem3<-subset(vdem, select =-c(country_name, country_text_id))
vdem2<-aggregate(.~cc+year, data= vdem3, FUN= mean)

##Give serbia a ccode
vdem$cc[vdem$country_name=="Serbia"]<-348

##Give Puerto Rico a ccode
vdem$cc[vdem$country_name=="Puerto Rico"]<-3

##Merge data based on primary base
EDTG$basep<-sapply(strsplit(EDTG$base, ","), '[',1)
EDTG$cc<-countrycode(EDTG$basep, "country.name", "cown") ###NEED to re-code missing cc

##Create code for Westbank/ Gaza
EDTG$cc[EDTG$basep=="West Bank/Gaza"]<-667
##Give serbia a ccode
EDTG$cc[EDTG$basep=="Serbia"]<-348
#Give Puerto Rico ccode
EDTG$cc[EDTG$basep=="Puerto Rico"]<-3


#Merge Data
data<-merge(EDTG, vdem, by=c("cc", "year"), all.x = TRUE, all.y=FALSE) 

data2<-merge(data, countryf, by=c("cc" ,"year"), all.x = TRUE, all.y=FALSE)

write.csv(data2, "powerorprogress.csv")

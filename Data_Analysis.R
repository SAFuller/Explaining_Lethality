##Sam Fuller
##EDTG


#load packages
library(readxl)
library(data.table)
library(MASS)
library(pscl)
#load data
EDTG<- read_excel("EDTG_Data.xls")

##Prelim

pois<-glm(nonterr_casualties~pch+sq+tch, family = "poisson", data=EDTG)
summary(pois)

nb<-glm.nb(nonterr_casualties~pch+sq+tch, data=EDTG)
summary(nb)




poisful<-glm(nonterr_casualties~pch+tch+sq+terrctrl+lead_hierarch+total_atks+duration+year, family = "poisson", data=EDTG)
summary(poisful)

poisful<-glm(nonterr_casualties~pch+tch+sq+terrctrl+lead_hierarch+total_atks+duration+pch*duration+tch*duration+year, family = "poisson", data=EDTG)
summary(poisful)

nbfull<-glm.nb(nonterr_casualties~pch+tch+sq+terrctrl+lead_hierarch+total_atks+year, data=EDTG)
summary(nbfull)


##Read data
library(readr)
library(stargazer)
library(pscl)
library(MASS)
EDTG <- read_csv("powerorprogress.csv")


##Prelim

pois<-glm(nonterr_casualties~pch+sq+tch, family = "poisson", data=EDTG)
summary(pois)

nb<-glm.nb(nonterr_casualties~pch+sq+tch, data=EDTG)
summary(nb)



## Model with controls
poisful<-glm(nonterr_casualties~pch+tch+sq+terrctrl+lead_hierarch+total_atks+duration+year, family = "poisson", data=EDTG)
summary(poisful)
stargazer(poisful, Text=TRUE)

nbfull<-glm.nb(nonterr_casualties~pch+tch+sq+terrctrl+lead_hierarch+total_atks+duration+year, data=EDTG)
summary(nbfull)





#Model with only age or year
poisage<-glm(nonterr_casualties~pch+tch+sq+terrctrl+lead_hierarch+total_atks+duration , family = "poisson", data=EDTG)
summary(poisage)

poisyear<-glm(nonterr_casualties~pch+tch+sq+terrctrl+lead_hierarch+total_atks+year, family = "poisson", data=EDTG)
summary(poisyear)


##Playing with interactions

poisint<-glm(nonterr_casualties~pch+tch+sq+terrctrl+lead_hierarch+total_atks+duration+pch*duration+tch*duration+year, family = "poisson", data=EDTG)
summary(poisint)



##Testing goal/hierarchy interactions

goalhier<-glm.nb(nonterr_casualties~policy+policyH+territory+territoryH+squo+squoH+system,  data=EDTG)
summary(goalhier)

hierCont<-glm.nb(nonterr_casualties~policy+policyH,  data=EDTG)
summary(hierCont)

stargazer(poisful,nbfull)

summary(EDTG$sq)


##ZINB
m1 <- zeroinfl(nonterr_casualties ~ pch+tch+lead_hierarch+total_atks+duration
               | v2x_libdem+ pop ,
               data = EDTG, dist = "negbin", EM = TRUE)
summary(m1)

m2 <- zeroinfl(nonterr_casualties ~ policy+policyH+territory+territoryH+system+lead_hierarch++total_atks+diversity 
               | v2x_libdem ,
               data = EDTG , dist = "negbin", EM = TRUE)
summary(m2)

m3 <- zeroinfl(nonterr_casualties ~ pch*lead_hierarch+tch*lead_hierarch+lead_hierarch+total_atks+diversity
               | v2x_libdem ,
               data = EDTG, dist = "negbin", EM = TRUE)
summary(m3)

m4 <- zeroinfl(nonterr_casualties ~ pch+tch+lead_hierarch+total_atks+duration+nat+rel+diversity+shr_trans+terrctrl+public_service+num_rivals
               | v2x_libdem ,
               data = EDTG, dist = "negbin", EM = TRUE)
summary(m4)

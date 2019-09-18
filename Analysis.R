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




##ZINB
m1 <- zeroinfl(nonterr_casualties ~ pch+tch+lead_hierarch+total_atks 
               | v2x_freexp_altinf+v2x_frassoc_thick ,
               data = data2, dist = "negbin", EM = TRUE)
summary(m1)

m2 <- zeroinfl(nonterr_casualties ~ policy+policyH+territory+territoryH+squo+squoH+system+
                 lead_hierarch+total_atks 
               | tpop+cinc+v2x_freexp_altinf+v2x_frassoc_thick ,
               data = data2 , dist = "negbin", EM = TRUE)
summary(m2)

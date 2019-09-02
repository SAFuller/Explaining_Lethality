##Analysis
##Sam Fuller

#load Packages

#load data
data<-read.csv("BAADxGTD.csv")

#prelim
attacks<-lm(numattack~left+reli+age+sizerec+terrcntrl+leadhierarch, data=data)
summary(attacks)

kills<-lm(nkill~left+reli+age+sizerec+terrcntrl+leadhierarch, data=data)
summary(kills)

attacksyear<-lm(numattack~left+reli+year+sizerec+terrcntrl+leadhierarch, data=data)
summary(attacksyear)

killsyear<-lm(nkill~left+reli+year+sizerec+terrcntrl+leadhierarch, data=data)
summary(killsyear)

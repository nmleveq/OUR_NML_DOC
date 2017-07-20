setwd("E:/Arango Lab/For SFS 2016")
install.packages("nortest")
library(nortest)

ndsnat<-read.csv("NDS Natalie.csv", header=T, sep=",")

attach(ndsnat)

str(ndsnat)

sponge<-subset(ndsnat,top=="sponge")
glass<- subset(ndsnat, top=="glass")
sponge$nitrogen=as.factor(sponge$nitrogen)
sponge$phosphorus=as.factor(sponge$phosphorus)
glass$nitrogen=as.factor(glass$nitrogen)
glass$phosphorus=as.factor(glass$phosphorus)
carbon=as.factor(carbon)

CR=aov(cr~nitrogen*phosphorus*carbon, data=sponge)

qqline(residuals(CR))
ad.test(residuals(CR))
summary(CR)
TukeyHSD(CR,"nitrogen")
TukeyHSD(CR,"carbon")
TukeyHSD(CR)

plot(TukeyHSD(CR))

GPP=aov(gpp~nitrogen*phosphorus*carbon, data=sponge)
qqline(residuals(GPP))
ad.test(residuals(GPP))
summary(GPP)
TukeyHSD(GPP,"nitrogen")
TukeyHSD(GPP,"carbon")
TukeyHSD(GPP)





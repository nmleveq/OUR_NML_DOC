
setwd("H:/Arango Lab/2016 Season/Water Chemistry/")
chem<-read.csv("Water Chem Summary.csv", header = T, sep = ",")
attach(chem)
high<-subset(chem, Low.or.High=="High")
low<-subset(chem, Low.or.High=="Low")

NO3<-aov(NO3~Season+Low.or.High, data = chem)
summary.aov(NO3)
TukeyHSD(NO3)

DOC<-aov(DOC~Season+Low.or.High, data = chem)
summary.aov(DOC)
TukeyHSD(DOC)

SRP<-aov(SRP~Season+Low.or.High, data = chem)
summary.aov(SRP)
TukeyHSD(SRP)

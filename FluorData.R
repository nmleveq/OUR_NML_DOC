##Headers of Sections have one #, futher subsections and comments have ##

#Setup
##Load necessary packages
install.packages("nortest")
install.packages("nlme")
install.packages("ggplot2")
install.packages("lattice")
install.packages("Hmisc")
install.packages("lsmeans")
install.packages("lme4")
library(nortest)
library(nlme)
library(ggplot2)
library(lattice)
library(Hmisc)
library(lsmeans)
library(lme4)
##Loading files and review data structure
setwd("C:\Users\Natalie\Desktop\Arango Lab\2016 Season\EEM Data")
Data<-read.csv("R Fluorescence Indices.csv", header = T, sep = ",")
Data<-Data[-c(7, 8), ]
attach(Data)

#Data Exploration
str(Data)

dotchart(FI,groups = stream, xlab = "FI", ylab = "Stream", main = "Fluorescence Index by Stream")
plot(season,FI, main="FI", xlab="Season", ylab="Fluorescence Index")
plot(WSB,FI, main="FI", xlab="Budworm Impact", ylab="Fluorescence Index")
dotchart(groups = season, FI, ylab = "FI", xlab = "Season", main = "Fluorescence Index by Season")

dotchart(fresh,groups = stream, xlab = "Freshness Index", ylab = "Stream", main = "Freshness Index by Stream")
plot(season,fresh, main="Freshness Index", xlab="Season", ylab="Freshness Index")
plot(WSB,fresh, main="FrI", xlab="Budworm Impact", ylab="Freshness Index")

dotchart(RFE,groups = stream, xlab = "RFE", ylab = "Stream", main = "RFE by Stream")
plot(season,RFE, main="RFE", xlab="Season", ylab="RFE")
plot(WSB,RFE, main="", xlab="Budworm Impact", ylab="RFE")

dotchart(BIX,groups = stream, xlab = "BIX", ylab = "Stream", main = "Biological Index by Stream")
plot(season,BIX, main="BIX", xlab="Season", ylab="Biological Index")
plot(WSB,BIX, main="Biological Index", xlab="Budworm Impact", ylab="BIX")

#ANOVAs
##High vs. low Streams
WFI.aov<-aov(FI~WSB, data = Data)
summary(WFI.aov)
TukeyHSD(WFI.aov)

Fresh.aov<-aov(fresh~WSB, data = Data)
summary(Fresh.aov)
TukeyHSD(Fresh.aov)

WBIX.aov<-aov(BIX~WSB, data = Data)
summary(WBIX.aov)
TukeyHSD(WBIX.aov)

##Spring vs Summer

SFI.aov<-aov(FI~season, data = Data)
summary(SFI.aov)
TukeyHSD(SFI.aov)

SFresh.aov<-aov(fresh~season, data = Data)
summary(SFresh.aov)
TukeyHSD(SFresh.aov)

SBIX.aov<-aov(BIX~season, data = Data)
summary(SBIX.aov)
TukeyHSD(SBIX.aov)

#Models of Both season and location
model1<-glm(FI~WSB+season)
summary(model1)
plot(model1)

vf1<-varIdent(form=~1|stream)
model2<-gls(FI~WSB+season, weights=vf1,data=Data)
summary(model2)
plot(model2)

model3<-gls(FI~WSB*season, weights=vf1,data=Data)
summary(model3)
plot(model3)

vf2<-varIdent(form=~1|season)
model4<-gls(FI~WSB, weights=vf2, data=Data)
summary(model4)
plot(model4)

model5<-gls(FI~stream, weights=vf2, data=Data)
summary(model5)
plot(model5)

model6<-gls(FI~WSB, weights=vf2, data=Data)
summary(model6)
plot(model6)

##LMER 
##Best models seem to be 8, 9, and 10.
model7 <- lmer(fresh ~ WSB + season + (1|stream), data=Data)
summary(model7)
plot(model7)

model8 <- lmer(fresh~WSB*season+(1|stream), data=Data)
summary(model8)
plot(model8)

model9<-lmer(FI~WSB+(1|stream), data=Data)
summary(model9)
plot(model9)

model10<-lmer(BIX~WSB*season+(1|stream), data=Data)
summary(model10)
plot(model10)

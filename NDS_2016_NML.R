##Headers of Sections have one #, futher subsections and comments have ##

#Setup
##Load necessary packages
install.packages("nortest")
install.packages("nlme")
install.packages("ggplot2")
install.packages("lattice")
install.packages("Hmisc")
install.packages("lsmeans")
library(nortest)
library(nlme)
library(ggplot2)
library(lattice)
library(Hmisc)
library(lsmeans)
##Loading files and review data structure
setwd("H:/Arango Lab/2016 Season/R Fluorescence Indices")
Data<-read.csv("R Fluorescence Indices.csv", header = T, sep = ",")
attach(Data)


#Data Exploration
str(Data)
##There are some small outliers in Spring stream:blue/treatment:frass,litter, however, we will attempt to retain that variation by using a GLS model.
##Our response variables are cr (Community respiration in units of mg O2 per cm^2 per hour) or nrr (Nutrient Response Ratio).
##We will primarily use nrr because it will allow comparisons between streams and seasons. 
dotchart(nrr,groups = stream, xlab = "Nutrient Response Ratio", ylab = "Stream", main = "NRR by Stream")
dotchart(nrr,groups = stream, xlab = "Nutrient Response Ratio", ylab = "Drainage", main = "NRR by Drainage")
dotchart(nrr,groups = date, xlab = "Nutrient Response Ratio", ylab = "Date",main = "NRR by Date")
dotchart(nrr,groups = lost, xlab = "Nutrient Response Ratio", ylab = "Lost (Y/N)")
dotchart(nrr,groups = treatment, xlab = "Nutrient Response Ratio", ylab = "Nutrient Treatment")
dotchart(nrr,groups = season, xlab = "Nutrient Response Ratio", ylab = "Nutrient Treatment")
plot(treatment,nrr, main="Community Respiration", xlab="Treatment", ylab="Nutrient Response Ratio")
plot(stream,nrr, main="Community Respiration", xlab="Stream", ylab="Nutrient Response Ratio")
plot(date,nrr, main="Community Respiration", xlab="Date", ylab="Nutrient Response Ratio")
plot(lost,nrr, main="Community Respiration", xlab="Lost", ylab="Nutrient Response Ratio")
plot(season,nrr, main="Community Respiration", xlab="Season", ylab="Nutrient Response Ratio")

#Models

#RND EFCT Chapter five Zuur
model1.1<-lme(nrr~treatment*season, random=~1|stream, data=Data)
summary(model1.1)
plot(model1.1, main="Model 1.1")
model1.2<-gls(nrr~treatment*season, data=Data)

anova(model1.1, model1.2)


##Variances depend on treatment, variable is treatment
vf1<-varIdent(form=~1|treatment)
model.1<-gls(nrr~treatment, weights=vf1,data=Data)
summary(model.1)
plot(model.1, main="NRR~Treatment weights treatment")

##Variances depend on stream; variables are stream, treatment, and season/date
vf2<-varIdent(form=~1|stream)
model.2<-gls(nrr~stream+treatment+season, weights=vf2,data=Data)
summary(model.2)
plot(model.2)

##Variances depend on stream, variable is treatment
###Run streams as a random factor see Chp 4,5
model.3<-gls(nrr~treatment+stream, weights=vf2,data=Data)
summary(model.3)
plot(model.3)

##Variances depend on treatment, variable is stream
model.4<-gls(nrr~stream, weights=vf1,data=Data)
summary(model.4)
plot(model.4)

##Variances depend on stream and treatment, variable is treatment
vf3<-varIdent(form=~stream|treatment)
model.5<-gls(nrr~treatment, weights=vf3,data=Data)
summary(model.5)
plot(model.5)

##same variance as above, model with stream and treatment
model.6<-gls(nrr~treatment*stream, weights=vf3,data=Data)
summary(model.6)
plot(model.6)

##Model 7, varience dependent on season 
vf4<-varIdent(form=~1|season)
model.7<-gls(nrr~treatment, weights=vf4,data=Data)
summary(model.7)
plot(model.7)

##Model 8, variences depend on season, variables are stream, treatment, and season
model.8<-gls(nrr~stream+treatment+season, weights=vf4,data=Data)
summary(model.8)
plot(model.8)

##Model 9
model.9<-gls(nrr~stream+treatment+season, weights = vf3, data=Data)
summary(model.9)
plot(model.9)

#Model comparisons
anova(model.1,model.2,model.3,model.4, model.5,model.6, model.7, model.8, model.9)
anova(model.1, model.5, model.8, model.9)
##I'm rolling with model 8. least collinearity, good(ish) plots and ok AIC/BIC

#Once we fix the above issue we can pick a model and use the below code to validate it 
#Validation
#View normalized residuals
P.1 <- resid(model.8, type = "normalized")
coplot(P.1~nrr | treatment, data = Data, ylab = "Normalised residuals", xlab="Nutrient Response Ratio")
plot(Data$nrr, resid(model.8), xlab="NRR", ylab="Residuals")

#observed vs fitted
plot(model.8, nrr ~ fitted(.) | treatment, abline = c(0,0))

#Making lsmeans plots and charts
##By Treatment (interesting)
lsmeans(model.8,~treatment)
lsm.output1 <- lsmeans(model.8,~treatment)
lsm.table1<-as.data.frame(confint(lsm.output1))
lsm.table1
a<-ggplot(lsm.table1,aes(treatment,lsmean)) + 
  geom_bar(width=0.80,stat="identity") + 
  scale_fill_manual(values=c("grey55","grey75")) +
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL,group=treatment),width=0.5) +
  ylab("Nutrient Response Ratio") +
  xlab("Treatment") +
  theme(legend.position="none",axis.title.x = element_text(size=18),axis.title.y = element_text(size=18), axis.text.x =element_text(size=14), axis.text.y =element_text(size=14))
a

##By Stream, less interesting...
lsmeans(model.8,~stream)
lsm.output2 <- lsmeans(model.8,~stream)
lsm.table2<-as.data.frame(confint(lsm.output2))
lsm.table2
b<-ggplot(lsm.table2,aes(stream,lsmean)) + 
  geom_bar(width=0.80,stat="identity") + 
  scale_fill_manual(values=c("grey55","grey75")) +
  geom_errorbar(aes(ymin=lower.CL,ymax=upper.CL,group=stream),width=0.5) +
  ylab("Nutrient Response Ratio") +
  xlab("Stream") +
  theme(legend.position="none") 
b

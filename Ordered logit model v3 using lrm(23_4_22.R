setwd("C:/Users/Dell/Desktop/YD")

library(readxl)
final <- read_excel("Data.xlsx")


library(foreign)
library(MASS)

library(rms)

final$Satisfaction<- as.factor(final$Satisfaction)
final$Age<-as.factor(final$Age)
final$Gender<-as.factor(final$Gender)
final$Marital_Status<-as.factor(final$Marital_Status)
final$Occupation<-as.factor(final$Occupation)
final$Education_Level <-as.factor(final$Education_Level)
final$Income<-as.factor(final$Income)
final$Visiting_Frequency<-as.factor(final$Visiting_Frequency)
final$Week_Visit<-as.factor(final$Week_Visit)
final$Time_Visit<-as.factor(final$Time_Visit)
final$Duration_Visit<-as.factor(final$Duration_Visit)
final$Distance_UGS<-as.factor(final$Distance_UGS)
final$Transport_Visit<-as.factor(final$Transport_Visit)
final$Facilities<-as.factor(final$Facilities)
final$Socialbenefits<-as.factor(final$Socialbenefits)
 final$Environmentalbenefits<-as.factor(final$Environmentalbenefits)

  
str(final)


#All factors
Y<- cbind(final$Satisfaction3)
X<- cbind (final$Age,
           final$Gender,
           final$Marital_Status,
           final$Occupation,
           final$Education_Level,
           final$Income,
           final$Visiting_Frequency,
           final$Week_Visit,
           final$Time_Visit,
          final$Duration_Visit,
           final$Distance_UGS,
           final$Transport_Visit,
           final$Facilities,
           final$Socialbenefits,
           final$Environmentalbenefits)

# Ordered logit model coefficients
ddist<- datadist(X)
options(datadist='ddist') 

ologit<- lrm(Y ~ X, data= final)
print(ologit, digits = 3)

library(stargazer)
stargazer(ologit,type='html', out='allfactors.html')

library(writexl)


# odd ratio

ologitodd=exp(coef(ologit))
ologitodd

stargazer(ologit, type="html", 
          coef=list(ologitodd), p.auto=FALSE, out="oddratio.htm")




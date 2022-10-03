setwd("C:/Users/Dell/Desktop/E-mobility/travel")

library(readxl)
final <- read_excel("Travel2.xlsx")
library(readxl)
final <- read_excel("Travel1.xlsx")


library(foreign)
library(MASS)

library(rms)

# Factors
final$Overall<- as.factor(final$Overall)
final$Age<-as.factor(final$Age)
final$Gender<-as.factor(final$Gender)
final$Education<-as.factor(final$Education)
final$Monthlyincome<-as.factor(final$Monthlyincome)
final$Job <-as.factor(final$Job)
final$Vehicle<-as.factor(final$Vehicle)
final$Experience<-as.factor(final$Experience)
final$Distance<-as.factor(final$Distance)
final$Fuel<-as.factor(final$Fuel)
final$Fuelperday<-as.factor(final$Fuelperday)
final$Vehiclesathome<-as.factor(final$Vehiclesathome)
final$Loan<-as.factor(final$Loan)

  
str(final)


#All
Y<- cbind(final$Overall)
X<- cbind (final$Gender,
           final$Age,
           final$Education,
           final$Monthlyincome,
           final$Job,
           final$Vehicle,
           final$Experience,
           final$Distance,
           final$Fuel,
          final$Fuelperday,
           final$Vehiclesathome,
           final$Loan)

# Ordered logit model coefficients
ddist<- datadist(X)
options(datadist='ddist') 

ologit<- lrm(Y ~ X, data= final)
print(ologit, digits = 3)

library(stargazer)
stargazer(ologit,type='html', out='allfactors7.html')

library(writexl)


# odd ratio

ologitodd=exp(coef(ologit))
ologitodd

stargazer(ologit, type="html", 
          coef=list(ologitodd), p.auto=FALSE, out="oddratio.htm")

# Reasons

final$Overall<- as.factor(final$Overall)
final$Easeandconvinience<-as.factor(final$Easeandconvinience)
final$`Traveltime(s/l)`<-as.factor(final$`Traveltime(s/l)`)
final$Comfort<-as.factor(final$Comfort)
final$Encumbrance<-as.factor(final$Encumbrance)
final$Tripchanging <-as.factor(final$Tripchanging)
final$costeffective<-as.factor(final$costeffective)


str(final)

#All
Y<- cbind(final$Overall)
X<- cbind (final$Easeandconvinience,
           final$`Traveltime(s/l)`,
           final$Comfort,
           final$Encumbrance,
           final$Tripchanging,
           final$costeffective)
          

# Ordered logit model coefficients
ddist<- datadist(X)
options(datadist='ddist') 

ologit<- lrm(Y ~ X, data= final)
print(ologit, digits = 3)

library(stargazer)
stargazer(ologit,type='html', out='allfactors2.html')

library(writexl)


# odd ratio

ologitodd=exp(coef(ologit))
ologitodd

stargazer(ologit, type="html", 
          coef=list(ologitodd), p.auto=FALSE, out="oddratio.htm")

# travelmode

final$Overall<- as.factor(final$Overall)
final$PublicBus <-as.factor(final$PublicBus)
final$citybus<-as.factor(final$citybus)
final$Bicycle<-as.factor(final$Bicycle)
final$Motorbike<-as.factor(final$Motorbike)
final$Foot <-as.factor(final$Foot)


str(final)


#All
Y<- cbind(final$Overall)
X<- cbind (final$PublicBus,
           final$citybus,
           final$Bicycle,
           final$Motorbike,
           final$Foot)

# Ordered logit model coefficients
ddist<- datadist(X)
options(datadist='ddist') 

ologit<- lrm(Y ~ X, data= final)
print(ologit, digits = 3)

library(stargazer)
stargazer(ologit,type='html', out='allfactors3.html')

library(writexl)


# odd ratio

ologitodd=exp(coef(ologit))
ologitodd

stargazer(ologit, type="html", 
          coef=list(ologitodd), p.auto=FALSE, out="oddratio.htm")

# travel behavior
final$Overall<- as.factor(final$Overall)
final$Compulsory <-as.factor(final$Compulsory)
final$Important<-as.factor(final$Important)
final$Casual<-as.factor(final$Casual)
final$Leisure<-as.factor(final$Leisure)

#All
Y<- cbind(final$Overall)
X<- cbind (final$Compulsory,
           final$Important,
           final$Casual,
           final$Leisure)
          

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


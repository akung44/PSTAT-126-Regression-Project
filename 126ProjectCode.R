library(MASS)
library(tidyverse)
library(car)

data<-read.table("machine.data", sep=",")

#Rename columns
names(data)<-c("vendor","Modelname", "MYCT", "MMIN", "MMAX", "CACH", "CHMIN",
               "CHMAX", "PRP", "ERP")
myct=data$MYCT
mmin=data$MMIN
mmax=data$MMAX
cach=data$CACH
chmin=data$CHMIN
chmax=data$CHMAX
prp=data$PRP
erp=data$ERP

#Create dummy columns for our vendors with separate dataframe
list1=c()
for (i in seq(1,30)){
  list1[i]=as.character(unique(data$vendor)[i])
}

vendornames=data[,1:2]
vendornames[list1]=2
vendornames<-subset(vendornames, select=-Modelname)
data<-subset(data, select=-c(vendor,Modelname))

for (j in 2:ncol(vendornames)){
  for (i in 1:nrow(vendornames)){
      vendornames[i,j]=as.numeric(vendornames[i,1]==unique(vendornames$vendor)[j-1])
    }
}

count=data.frame(unique(vendornames[1]))

# Choose the most occurring categorical variables
colSums(vendornames[,-1])


vendornames<-subset(vendornames, select=-wang)
ibm=vendornames$ibm
honeywell=vendornames$honeywell
nas=vendornames$nas
ncr=vendornames$ncr
sperry=vendornames$sperry
siemens=vendornames$siemens


finalvendors=cbind(ibm, honeywell, nas, ncr, sperry, siemens)

# Join the dataframes
data<-cbind(data, finalvendors)

#Scatterplot Matrix to determine if there is a relationship
pairs(~prp + erp + chmax + chmin + cach + mmax + mmin + myct)

#Apply Best subsets regression
library(leaps)

reg=regsubsets(subset(data, select=-PRP), prp, nvmax=13)

mod=summary(reg)
mod$which
mod$adjr2
max(mod$adjr2)
mod$cp

# Select the model with the highest adjusted R^2
mod_1=lm(prp~myct+mmin+mmax+cach+chmax+erp+ibm+honeywell+ncr)
# We know check the correlation between variables
variance1=vif(lm(prp~myct+mmin+mmax+cach+chmax+erp+ibm+honeywell+ncr))
variance2=vif(lm(prp~myct+mmin+cach+chmax+erp+ibm+honeywell+ncr))
variance3=vif(lm(prp~myct+mmin+cach+chmax+ibm+honeywell+ncr))
mod_1=lm(prp~myct+mmin+cach+chmax+ibm+honeywell+ncr)
yhat1=fitted(mod_1)
errors1=prp-yhat1

plot(yhat1, errors1)
abline(mod_1)

plot(myct, errors1)
plot(mmin, errors1)
plot(mmax, errors1)
plot(chmax, errors1)
plot(erp, errors1)
plot(ibm, errors1)
plot(ncr, errors1)
plot(sperry, errors1)

# Histogram
hist(errors1)
# From our QQ plot we see that our model is heavy-tailed
qqnorm(errors1)
qqline(errors1)
# We further use the Shapiro Wilk test to clarify
shapiro.test(errors1)

# Since we have a non-constant variance throughout our residual vs predictor graphs, Box-Cox
library(MASS)
trans=boxcox(prp~myct+mmin+cach+chmax+ibm+honeywell+ncr, data=data, lambda=seq(0, 1, length=10))

# From our Box-Cox test we see that our transformation for Y has lambda=0.58
# We now apply the necessary transformation in order to check whether this improves our model
mod_2=lm(prp^(0.2)~myct+mmin+mmax+chmax+erp+ibm+ncr+sperry)
yhat2=fitted(mod_2)
errors2=erp-yhat2

plot(errors2, yhat2)

plot(myct, errors2)
plot(mmin, errors2)
plot(mmax, errors2)
plot(chmax, errors2)
plot(erp, errors2)
plot(ibm, errors2)
plot(ncr, errors2)
plot(sperry, errors2)

# We now see that our graphs are non-linear so we apply a transformation on x to myct
mod_3=mod_2=lm(prp^(0.2)~log(myct)+mmin+log(mmax)+chmax+log(prp)+ibm+ncr+sperry)
yhat3=fitted(mod_3)
errors3=erp-yhat3

plot(myct, errors3)
plot(mmin, errors3)
plot(mmax, errors3)
plot(chmax, errors3)
plot(prp, errors3)
plot(ibm, errors3)
plot(ncr, errors3)
plot(sperry, errors3)

# We see that best subset regression does not give us our desired results, apply stepwise regression
basemod=lm(erp~1)
maxmod=lm(erp~myct+mmin+mmax+chmin+chmax+prp+ibm+ncr+sperry+honeywell+nas+siemens)
step(basemod, scope = list(lower=basemod, upper=maxmod))
mod_4=lm(erp ~ prp + mmax + mmin + myct + chmax + ibm + sperry + 
           ncr + nas)

yhat4=fitted(mod_4)
errors4=erp-yhat4

plot(errors4, yhat4)

plot(myct, errors4)
plot(mmin, errors4)
plot(mmax, errors4)
plot(chmax, errors4)
plot(prp, errors4)
plot(ibm, errors4)
plot(ncr, errors4)
plot(sperry, errors4)

# From our QQ plot we see that our model is skewed
qqnorm(errors4)
qqline(errors4)
# We further use the Shapiro Wilk test to clarify
shapiro.test(errors4)

# We now apply the Box-Cox since we have variance, normality, non-linearity issues
library(MASS)
lambda=boxcox(erp ~ prp + mmax + mmin + myct + chmax + ibm + sperry + 
                ncr + nas, lambda=seq(0.5, 1, length=10))

#Lambda is 0.58
erp2=erp^0.58
mod_5=lm(erp2 ~ prp + mmax + mmin + myct + chmax + ibm + sperry + 
           ncr + nas)

yhat5=fitted(mod_5)
errors5=erp-yhat5

plot(myct, errors5)
plot(mmin, errors5)
plot(mmax, errors5)
plot(chmax, errors5)
plot(prp, errors5)
plot(ibm, errors5)
plot(ncr, errors5)
plot(sperry, errors5)

# Transform x
mod_6=lm(erp2 ~ log(prp) + log(mmax) + log(mmin) + log(myct) + chmax + ibm + sperry + 
           ncr + nas)

yhat6=fitted(mod_6)
errors6=erp-yhat6

plot(myct, errors6)
plot(mmin, errors6)
plot(mmax, errors6)
plot(chmax, errors6)
plot(prp, errors6)
plot(ibm, errors6)
plot(ncr, errors6)
plot(sperry, errors6)

setwd("~/126 Project")
data=read.table("Concrete_Data.txt", sep="\t", header=TRUE)

names(data)<-c("Cement", "Blast Furnace", "Ash", "Water", "SuperPlasticizer", "CoarseAgg",
               "FineAgg", "Age", "Strength")

cement=data$Cement
blast=data$`Blast Furnace`
ash=data$Ash
water=data$Water
super=data$SuperPlasticizer
coarse=data$CoarseAgg
fine=data$FineAgg
age=data$Age
strength=data$Strength

cor(data)

pairs(~strength + age + fine + coarse + super + water + ash + blast + cement)


#Stepwise Regression
basemod=lm(strength~1)
stepmod=lm(strength ~ age + fine + coarse + super + water + ash + blast + cement)
step(basemod, scope = list(lower=basemod, upper=stepmod))

stepwise=lm(strength ~ cement + super + age + blast + water + 
              ash)
fittedstep=fitted(stepwise)
errors=strength-fittedstep

plot(fittedstep, errors, xlab="Fitted", ylab="Residuals")
plot(cement, errors)
plot(blast, errors)
plot(ash, errors)
plot(water, errors)
plot(age, errors)
plot(super, errors)

#QQ Line
qqnorm(errors)
qqline(errors)

shapiro.test(errors)
# Log transform age
library(MASS)
stepwise=lm(strength ~ cement + super + log(age) + blast + water + 
              ash)

fittedstep=fitted(stepwise)
errors=strength-fittedstep

plot(fittedstep, errors, xlab="fitted", ylab="residuals")
plot(cement, errors)
plot(blast, errors)
plot(ash, errors)
plot(water, errors)
plot(age, errors)
plot(super, errors)

qqnorm(errors)
qqline(errors)

shapiro.test(errors)

# Studentized residuals outliers

r_ext=sort(rstudent(stepwise))
n<-length(strength)
r_ext[n]

# Delete point 382, rerun regression and check for influential y values
data<-data[-c(382, 384, 15, 9, 405),]
rownames(data) <- 1:nrow(data)
# Check our regression with the studentized residuals deleted (9 , 405)
cement=data$Cement
blast=data$`Blast Furnace`
ash=data$Ash
water=data$Water
super=data$SuperPlasticizer
coarse=data$CoarseAgg
fine=data$FineAgg
age=data$Age
strength=data$Strength

stepwise=lm(strength ~ cement + super + log(age) + blast + water + 
              ash)

fittedstep=fitted(stepwise)
errors=strength-fittedstep

# Check the LINE conditions in residual vs predictor

plot(fittedstep, errors, xlab="Fitted", ylab="Residuals")
plot(cement, errors)
plot(blast, errors)
plot(ash, errors)
plot(water, errors)
plot(age, errors)
plot(super, errors)

# Check the normality condition
qqnorm(errors)
qqline(errors)

shapiro.test(errors)
hist(errors)

# We now check residuals vs predictor for other variables we did not include
combined1=cement*water
plot(combined1, errors)

combined2=cement*age
plot(combined2, errors)

combined3=cement^2
plot(combined3, errors)

combined4=cement*coarse
plot(combined4, errors)

combined5=water*coarse
plot(combined5, errors)

combined6=water*fine
plot(combined6, errors)

combined7=blast*coarse
plot(combined7, errors)

combined8=blast*fine
plot(combined8, errors)
